-module(spectra_codec).

-doc """
Behaviour for custom codecs that extend spectra's encoding, decoding, and
schema generation for specific types and formats.

## Registering a codec

Codecs are registered in the application environment before use:

```erlang
application:set_env(spectra, codecs, #{
    {my_module, {type, my_type, 0}} => my_codec
}).
```

Alternatively, add `-behaviour(spectra_codec)` to the module that *defines*
the type and spectra will discover the codec automatically at load time.

## Callback arguments: `SpType` and `Params`

Every callback receives two arguments for inspecting the type in context.

### `Params :: term()` — static per-type configuration

`Params` is the value from the `-spectra(#{type_parameters => ...})` attribute
on the type definition, or `undefined` when no such attribute is present.

```erlang
-spectra(#{type_parameters => <<"user:">>}).
-type user_id() :: binary().
```

The codec receives `<<"user:">>` as `Params` and can use it directly:

```erlang
encode(json, _Mod, _TypeRef, Data, _SpType, Prefix, _Config) when is_binary(Prefix) ->
    {ok, <<Prefix/binary, Data/binary>>};
encode(_Format, _Mod, _TypeRef, _Data, _SpType, _Params, _Config) ->
    continue.
```

### `SpType :: spectra:sp_type()` — instantiation node with type-variable bindings

`SpType` is the type node from the traversal at the point where the codec was
invoked. For generic types this is the reference node — `#sp_user_type_ref{}`
or `#sp_remote_type{}` — and it carries the **concrete type-variable bindings**
of that specific instantiation. Use `spectra_type:type_args/1` to extract them.

For a type written as `dict:dict(binary(), integer())` the codec receives the
`#sp_remote_type{}` node and can extract `[BinaryType, IntegerType]` to
recursively encode/decode keys and values:

```erlang
encode(json, Mod, _TypeRef, Data, SpType, _Params, Config) ->
    TypeInfo = spectra_module_types:get(Mod, Config),
    [KeyType, ValueType] = spectra_type:type_args(SpType),
    encode_pairs(TypeInfo, KeyType, ValueType, dict:to_list(Data), #{}, Config).
```

Note: when a codec is invoked from the `spectra:encode/decode/schema` entry
points (rather than via mid-traversal dispatch), `SpType` is the resolved type
definition and `spectra_type:type_args/1` returns `[]`. In practice this only
affects codecs for generic types — they are naturally called from the traversal
where the reference node is available.
""".

-callback encode(
    Format :: atom(),
    Module :: module(),
    TypeRef :: spectra:sp_type_reference(),
    Data :: dynamic(),
    SpType :: spectra:sp_type(),
    Params :: term(),
    Config :: spectra:sp_config()
) ->
    spectra:codec_encode_result().
-callback decode(
    Format :: atom(),
    Module :: module(),
    TypeRef :: spectra:sp_type_reference(),
    Input :: dynamic(),
    SpType :: spectra:sp_type(),
    Params :: term(),
    Config :: spectra:sp_config()
) ->
    spectra:codec_decode_result().
-callback schema(
    Format :: atom(),
    Module :: module(),
    TypeRef :: spectra:sp_type_reference(),
    SpType :: spectra:sp_type(),
    Params :: term(),
    Config :: spectra:sp_config()
) ->
    dynamic().

-optional_callbacks([schema/6]).

-export([
    try_codec_encode/7,
    try_codec_decode/7,
    try_codec_schema/6
]).

-doc "Encodes `Data` via a registered codec for the module owning `TypeInfo`, or returns `continue`.".
-spec try_codec_encode(
    TypeInfo :: spectra:type_info(),
    Format :: atom(),
    TypeRef :: spectra:sp_type_reference(),
    Type :: spectra:sp_type(),
    Data :: dynamic(),
    SpType :: spectra:sp_type(),
    Config :: spectra:sp_config()
) -> spectra:codec_encode_result().
try_codec_encode(TypeInfo, Format, TypeReference, Type, Data, SpType, Config) ->
    Mod = spectra_type_info:get_module(TypeInfo),
    case spectra_type_info:find_codec(TypeInfo, TypeReference, Config) of
        {ok, M} ->
            M:encode(
                Format, Mod, TypeReference, Data, SpType, spectra_type:parameters(Type), Config
            );
        error ->
            continue
    end.

-doc "Decodes `Data` via a registered codec for the module owning `TypeInfo`, or returns `continue`.".
-spec try_codec_decode(
    TypeInfo :: spectra:type_info(),
    Format :: atom(),
    TypeRef :: spectra:sp_type_reference(),
    Type :: spectra:sp_type(),
    Data :: dynamic(),
    SpType :: spectra:sp_type(),
    Config :: spectra:sp_config()
) -> spectra:codec_decode_result().
try_codec_decode(TypeInfo, Format, TypeReference, Type, Data, SpType, Config) ->
    Mod = spectra_type_info:get_module(TypeInfo),
    case spectra_type_info:find_codec(TypeInfo, TypeReference, Config) of
        {ok, M} ->
            M:decode(
                Format, Mod, TypeReference, Data, SpType, spectra_type:parameters(Type), Config
            );
        error ->
            continue
    end.

-doc "Returns the schema for the module owning `TypeInfo` via a registered codec, or `continue`.".
-spec try_codec_schema(
    TypeInfo :: spectra:type_info(),
    Format :: atom(),
    TypeRef :: spectra:sp_type_reference(),
    Type :: spectra:sp_type(),
    SpType :: spectra:sp_type(),
    Config :: spectra:sp_config()
) -> dynamic() | continue.
try_codec_schema(TypeInfo, Format, TypeReference, Type, SpType, Config) ->
    Mod = spectra_type_info:get_module(TypeInfo),
    case spectra_type_info:find_codec(TypeInfo, TypeReference, Config) of
        {ok, M} ->
            try
                M:schema(
                    Format, Mod, TypeReference, SpType, spectra_type:parameters(Type), Config
                )
            catch
                error:undef -> erlang:error({schema_not_implemented, M, TypeReference})
            end;
        error ->
            continue
    end.

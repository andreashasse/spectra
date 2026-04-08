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
encode(json, _Mod, _TypeRef, Data, _SpType, Prefix) when is_binary(Prefix) ->
    {ok, <<Prefix/binary, Data/binary>>};
encode(_Format, _Mod, _TypeRef, _Data, _SpType, _Params) ->
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
encode(json, Mod, _TypeRef, Data, SpType, _Params) ->
    TypeInfo = spectra_module_types:get(Mod),
    [KeyType, ValueType] = spectra_type:type_args(SpType),
    encode_pairs(TypeInfo, KeyType, ValueType, dict:to_list(Data), #{}).
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
    Params :: term()
) ->
    spectra:codec_encode_result().
-callback decode(
    Format :: atom(),
    Module :: module(),
    TypeRef :: spectra:sp_type_reference(),
    Input :: dynamic(),
    SpType :: spectra:sp_type(),
    Params :: term()
) ->
    spectra:codec_decode_result().
-callback schema(
    Format :: atom(),
    Module :: module(),
    TypeRef :: spectra:sp_type_reference(),
    SpType :: spectra:sp_type(),
    Params :: term()
) ->
    dynamic().

-optional_callbacks([schema/5]).

-export([
    try_codec_encode/6,
    try_codec_decode/6,
    try_codec_schema/4
]).

-spec try_codec_encode(
    Mod :: module(),
    Format :: atom(),
    Type :: spectra:sp_type(),
    Data :: dynamic(),
    SpType :: spectra:sp_type(),
    Codecs :: #{spectra:codec_key() => module()}
) -> spectra:codec_encode_result().
try_codec_encode(Mod, Format, Type, Data, SpType, Codecs) ->
    #{name := TypeReference} = spectra_type:get_meta(Type),
    case spectra_type_info:find_codec(Mod, TypeReference, Codecs, false) of
        {ok, M} ->
            M:encode(Format, Mod, TypeReference, Data, SpType, spectra_type:parameters(Type));
        error ->
            continue
    end.

-spec try_codec_decode(
    Mod :: module(),
    Format :: atom(),
    Type :: spectra:sp_type(),
    Data :: dynamic(),
    SpType :: spectra:sp_type(),
    Codecs :: #{spectra:codec_key() => module()}
) -> spectra:codec_decode_result().
try_codec_decode(Mod, Format, Type, Data, SpType, Codecs) ->
    #{name := TypeReference} = spectra_type:get_meta(Type),
    case spectra_type_info:find_codec(Mod, TypeReference, Codecs, false) of
        {ok, M} ->
            M:decode(Format, Mod, TypeReference, Data, SpType, spectra_type:parameters(Type));
        error ->
            continue
    end.

-spec try_codec_schema(
    Mod :: module(),
    Format :: atom(),
    Type :: spectra:sp_type(),
    SpType :: spectra:sp_type()
) -> dynamic() | continue.
try_codec_schema(Mod, Format, Type, SpType) ->
    #{name := TypeReference} = spectra_type:get_meta(Type),
    GlobalCodecs = application:get_env(spectra, codecs, #{}),
    case spectra_type_info:find_codec(Mod, TypeReference, GlobalCodecs, false) of
        {ok, M} ->
            case erlang:function_exported(M, schema, 5) of
                true ->
                    M:schema(Format, Mod, TypeReference, SpType, spectra_type:parameters(Type));
                false ->
                    erlang:error({schema_not_implemented, M, TypeReference})
            end;
        error ->
            continue
    end.

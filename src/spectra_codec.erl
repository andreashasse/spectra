-module(spectra_codec).

-include("../include/spectra_internal.hrl").

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

## Callback arguments

Every callback receives two type-related arguments for inspecting the type in context.

### `CallerTypeInfo :: spectra:type_info()` — caller's module type information

`CallerTypeInfo` is the type information of the module where the type traversal
is currently taking place. This allows the codec to recursively encode/decode
user-defined generic arguments that might be defined locally in that module.

### `TargetType :: spectra:sp_type()` — instantiation node with type-variable bindings

`TargetType` is the type node from the traversal at the point where the codec was
invoked. For generic types this is the reference node — `#sp_user_type_ref{}`
or `#sp_remote_type{}` — and it carries the **concrete type-variable bindings**
of that specific instantiation. Use `spectra_type:type_args/1` to extract them.
Values supplied via `-spectra(#{type_parameters => ...})` are **not** propagated
onto reference nodes (`#sp_user_type_ref{}` / `#sp_remote_type{}`), so
`spectra_type:parameters/1` on `TargetType` returns `undefined` during
mid-traversal dispatch. It is only reliable when `TargetType` is the resolved
type definition (i.e. the codec is invoked from the `spectra:encode/decode/schema`
entry points directly).

For a type written as `dict:dict(binary(), integer())` the codec receives the
`#sp_remote_type{}` node and can extract `[BinaryType, IntegerType]` to
recursively encode/decode keys and values:

```erlang
encode(json, CallerTypeInfo, _TargetTypeRef, Data, TargetType, Config) ->
    [KeyType, ValueType] = spectra_type:type_args(TargetType),
    encode_pairs(CallerTypeInfo, KeyType, ValueType, dict:to_list(Data), #{}, Config).
```

Note: when a codec is invoked from the `spectra:encode/decode/schema` entry
points (rather than via mid-traversal dispatch), `TargetType` is the resolved type
definition and `spectra_type:type_args/1` returns `[]`. In practice this only
affects codecs for generic types — they are naturally called from the traversal
where the reference node is available.
""".

-callback encode(
    Format :: atom(),
    CallerTypeInfo :: spectra:type_info(),
    TargetTypeRef :: spectra:sp_type_reference(),
    Data :: dynamic(),
    TargetType :: spectra:sp_type(),
    Config :: spectra:sp_config()
) ->
    spectra:codec_encode_result().
-callback decode(
    Format :: atom(),
    CallerTypeInfo :: spectra:type_info(),
    TargetTypeRef :: spectra:sp_type_reference(),
    Input :: dynamic(),
    TargetType :: spectra:sp_type(),
    Config :: spectra:sp_config()
) ->
    spectra:codec_decode_result().
-callback schema(
    Format :: atom(),
    CallerTypeInfo :: spectra:type_info(),
    TargetTypeRef :: spectra:sp_type_reference(),
    TargetType :: spectra:sp_type(),
    Config :: spectra:sp_config()
) ->
    dynamic().

-optional_callbacks([schema/5]).

-export([
    try_codec_encode/7,
    try_codec_decode/7,
    try_codec_schema/6
]).

-doc "Encodes `Data` via a registered codec for `RemoteMod`, or returns `continue`.".
-spec try_codec_encode(
    CallerTypeInfo :: spectra:type_info(),
    RemoteMod :: module(),
    Format :: atom(),
    TargetTypeRef :: spectra:sp_type_reference(),
    Data :: dynamic(),
    TargetType :: spectra:sp_type(),
    Config :: spectra:sp_config()
) -> spectra:codec_encode_result().
try_codec_encode(CallerTypeInfo, RemoteMod, Format, TargetTypeRef, Data, TargetType, Config) ->
    case Config#sp_config.codecs of
        #{{RemoteMod, TargetTypeRef} := M} ->
            M:encode(Format, CallerTypeInfo, TargetTypeRef, Data, TargetType, Config);
        #{} ->
            case has_local_codec(CallerTypeInfo, RemoteMod, Config) of
                {ok, M} ->
                    M:encode(Format, CallerTypeInfo, TargetTypeRef, Data, TargetType, Config);
                error ->
                    continue
            end
    end.

-doc "Decodes `Data` via a registered codec for `RemoteMod`, or returns `continue`.".
-spec try_codec_decode(
    CallerTypeInfo :: spectra:type_info(),
    RemoteMod :: module(),
    Format :: atom(),
    TargetTypeRef :: spectra:sp_type_reference(),
    Data :: dynamic(),
    TargetType :: spectra:sp_type(),
    Config :: spectra:sp_config()
) -> spectra:codec_decode_result().
try_codec_decode(CallerTypeInfo, RemoteMod, Format, TargetTypeRef, Data, TargetType, Config) ->
    case Config#sp_config.codecs of
        #{{RemoteMod, TargetTypeRef} := M} ->
            M:decode(Format, CallerTypeInfo, TargetTypeRef, Data, TargetType, Config);
        #{} ->
            case has_local_codec(CallerTypeInfo, RemoteMod, Config) of
                {ok, M} ->
                    M:decode(Format, CallerTypeInfo, TargetTypeRef, Data, TargetType, Config);
                error ->
                    continue
            end
    end.

-doc "Returns the schema for `RemoteMod` via a registered codec, or `continue`.".
-spec try_codec_schema(
    CallerTypeInfo :: spectra:type_info(),
    RemoteMod :: module(),
    Format :: atom(),
    TargetTypeRef :: spectra:sp_type_reference(),
    TargetType :: spectra:sp_type(),
    Config :: spectra:sp_config()
) -> dynamic() | continue.
try_codec_schema(CallerTypeInfo, RemoteMod, Format, TargetTypeRef, TargetType, Config) ->
    case Config#sp_config.codecs of
        #{{RemoteMod, TargetTypeRef} := M} ->
            invoke_schema(M, Format, CallerTypeInfo, TargetTypeRef, TargetType, Config);
        #{} ->
            case has_local_codec(CallerTypeInfo, RemoteMod, Config) of
                {ok, M} ->
                    invoke_schema(M, Format, CallerTypeInfo, TargetTypeRef, TargetType, Config);
                error ->
                    continue
            end
    end.

invoke_schema(M, Format, CallerTypeInfo, TargetTypeRef, TargetType, Config) ->
    try
        M:schema(Format, CallerTypeInfo, TargetTypeRef, TargetType, Config)
    catch
        error:undef -> erlang:error({schema_not_implemented, M, TargetTypeRef})
    end.

has_local_codec(CallerTypeInfo, RemoteMod, Config) ->
    case spectra_type_info:get_module(CallerTypeInfo) of
        RemoteMod ->
            spectra_type_info:find_local_codec(CallerTypeInfo);
        _ ->
            RemoteTypeInfo = spectra_module_types:get(RemoteMod, Config),
            spectra_type_info:find_local_codec(RemoteTypeInfo)
    end.

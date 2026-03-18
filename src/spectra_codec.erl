-module(spectra_codec).

-doc """
Behaviour for custom codecs that extend spectra's encoding, decoding, and schema
generation for specific types and formats.

## Params argument

The `Params` argument passed to every callback is the value of the
`type_parameters` key in the `-spectra(#{type_parameters => Value})` attribute
on the type definition, or `undefined` if that attribute is absent.

**Important**: `Params` is unrelated to Erlang type variables (generic
parameters such as the `T` in `-type wrapper(T) :: {wrapper, T}`). When a
codec is invoked for a parameterised type, the concrete bindings for those type
variables are **not** available in `Params`. Codec types with Erlang type
variables are therefore not useful in practice — the codec cannot act on the
variable bindings. Use `type_parameters` in the `-spectra` attribute to pass
static, per-type configuration to your codec instead.
""".

-callback encode(
    Format :: atom(),
    Module :: module(),
    TypeRef :: spectra:sp_type_reference(),
    Data :: dynamic(),
    Params :: term()
) ->
    spectra:codec_encode_result().
-callback decode(
    Format :: atom(),
    Module :: module(),
    TypeRef :: spectra:sp_type_reference(),
    Input :: dynamic(),
    Params :: term()
) ->
    spectra:codec_decode_result().
-callback schema(
    Format :: atom(),
    Module :: module(),
    TypeRef :: spectra:sp_type_reference(),
    Params :: term()
) ->
    dynamic().

-optional_callbacks([schema/4]).

-export([
    try_codec_encode/4,
    try_codec_decode/4,
    try_codec_schema/3
]).

-spec try_codec_encode(
    Mod :: module(),
    Format :: atom(),
    Type :: spectra:sp_type(),
    Data :: dynamic()
) -> spectra:codec_encode_result().
try_codec_encode(Mod, Format, Type, Data) ->
    #{name := TypeReference} = spectra_type:get_meta(Type),
    case spectra_type_info:find_codec(Mod, TypeReference) of
        {ok, M} ->
            M:encode(Format, Mod, TypeReference, Data, spectra_type:parameters(Type));
        error ->
            continue
    end.

-spec try_codec_decode(
    Mod :: module(),
    Format :: atom(),
    Type :: spectra:sp_type(),
    Data :: dynamic()
) -> spectra:codec_decode_result().
try_codec_decode(Mod, Format, Type, Data) ->
    #{name := TypeReference} = spectra_type:get_meta(Type),
    case spectra_type_info:find_codec(Mod, TypeReference) of
        {ok, M} ->
            M:decode(Format, Mod, TypeReference, Data, spectra_type:parameters(Type));
        error ->
            continue
    end.

-spec try_codec_schema(
    Mod :: module(),
    Format :: atom(),
    Type :: spectra:sp_type()
) -> dynamic() | continue.
try_codec_schema(Mod, Format, Type) ->
    #{name := TypeReference} = spectra_type:get_meta(Type),
    case spectra_type_info:find_codec(Mod, TypeReference) of
        {ok, M} ->
            case erlang:function_exported(M, schema, 4) of
                true ->
                    M:schema(Format, Mod, TypeReference, spectra_type:parameters(Type));
                false ->
                    erlang:error({schema_not_implemented, M, TypeReference})
            end;
        error ->
            continue
    end.

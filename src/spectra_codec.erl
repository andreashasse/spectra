-module(spectra_codec).

-doc """
Behaviour for custom codecs that extend spectra's encoding, decoding, and schema
generation for specific types and formats.

## SpType argument

The `SpType` argument passed to every callback is the original `sp_type()` node
from the traversal — the instantiation node carrying concrete type-variable
bindings.

For `#sp_user_type_ref{}` and `#sp_remote_type{}` nodes the concrete type
arguments are available via `spectra_type:type_args/1`. For a type such as
`dict:dict(binary(), integer())` the codec receives the full remote-type node
and can extract `[BinaryType, IntegerType]` to recursively encode/decode keys
and values.

To recover the static `type_parameters` value from a `-spectra()` attribute
use `spectra_type:parameters(SpType)`.
""".

-callback encode(
    Format :: atom(),
    Module :: module(),
    TypeRef :: spectra:sp_type_reference(),
    Data :: dynamic(),
    SpType :: spectra:sp_type()
) ->
    spectra:codec_encode_result().
-callback decode(
    Format :: atom(),
    Module :: module(),
    TypeRef :: spectra:sp_type_reference(),
    Input :: dynamic(),
    SpType :: spectra:sp_type()
) ->
    spectra:codec_decode_result().
-callback schema(
    Format :: atom(),
    Module :: module(),
    TypeRef :: spectra:sp_type_reference(),
    SpType :: spectra:sp_type()
) ->
    dynamic().

-optional_callbacks([schema/4]).

-export([
    try_codec_encode/5,
    try_codec_decode/5,
    try_codec_schema/4
]).

-spec try_codec_encode(
    Mod :: module(),
    Format :: atom(),
    Type :: spectra:sp_type(),
    Data :: dynamic(),
    SpType :: spectra:sp_type()
) -> spectra:codec_encode_result().
try_codec_encode(Mod, Format, Type, Data, SpType) ->
    #{name := TypeReference} = spectra_type:get_meta(Type),
    case spectra_type_info:find_codec(Mod, TypeReference) of
        {ok, M} ->
            M:encode(Format, Mod, TypeReference, Data, SpType);
        error ->
            continue
    end.

-spec try_codec_decode(
    Mod :: module(),
    Format :: atom(),
    Type :: spectra:sp_type(),
    Data :: dynamic(),
    SpType :: spectra:sp_type()
) -> spectra:codec_decode_result().
try_codec_decode(Mod, Format, Type, Data, SpType) ->
    #{name := TypeReference} = spectra_type:get_meta(Type),
    case spectra_type_info:find_codec(Mod, TypeReference) of
        {ok, M} ->
            M:decode(Format, Mod, TypeReference, Data, SpType);
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
    case spectra_type_info:find_codec(Mod, TypeReference) of
        {ok, M} ->
            case erlang:function_exported(M, schema, 4) of
                true ->
                    M:schema(Format, Mod, TypeReference, SpType);
                false ->
                    erlang:error({schema_not_implemented, M, TypeReference})
            end;
        error ->
            continue
    end.

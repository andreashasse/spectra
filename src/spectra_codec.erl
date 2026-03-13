-module(spectra_codec).

-callback encode(
    Format :: atom(),
    TypeRef :: spectra:sp_type_reference(),
    Data :: dynamic(),
    Params :: term()
) ->
    spectra:codec_encode_result().
-callback decode(
    Format :: atom(),
    TypeRef :: spectra:sp_type_reference(),
    Input :: dynamic(),
    Params :: term()
) ->
    spectra:codec_decode_result().
-callback schema(
    Format :: atom(), TypeRef :: spectra:sp_type_reference(), Params :: term()
) ->
    dynamic().

-optional_callbacks([schema/3]).

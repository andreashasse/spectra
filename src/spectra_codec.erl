-module(spectra_codec).

-callback encode(
    Format :: atom(),
    TypeRef :: spectra:sp_type_reference(),
    Data :: dynamic(),
    Opts :: spectra:codec_encode_opts()
) ->
    spectra:codec_encode_result().
-callback decode(
    Format :: atom(),
    TypeRef :: spectra:sp_type_reference(),
    Input :: dynamic(),
    Opts :: spectra:codec_decode_opts()
) ->
    spectra:codec_decode_result().
-callback schema(
    Format :: atom(), TypeRef :: spectra:sp_type_reference(), Opts :: spectra:codec_schema_opts()
) ->
    dynamic().

-optional_callbacks([schema/3]).

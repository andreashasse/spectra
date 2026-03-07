-module(spectra_codec).

-callback encode(
    Format :: atom(),
    TypeRef :: spectra:sp_type_reference(),
    Data :: dynamic(),
    Opts :: spectra:codec_encode_opts()
) ->
    {ok, term()} | {error, [spectra:error()]}.
-callback decode(
    Format :: atom(),
    TypeRef :: spectra:sp_type_reference(),
    Input :: dynamic(),
    Opts :: spectra:codec_decode_opts()
) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
-callback schema(
    Format :: atom(), TypeRef :: spectra:sp_type_reference(), Opts :: spectra:codec_schema_opts()
) ->
    dynamic().

-optional_callbacks([schema/3]).

-module(spectra_codec).

-callback encode(
    Format :: atom(),
    TypeRef :: spectra:sp_type_reference(),
    Data :: dynamic(),
    Opts :: map()
) ->
    {ok, term()} | {error, [spectra:error()]}.
-callback decode(
    Format :: atom(),
    TypeRef :: spectra:sp_type_reference(),
    Input :: dynamic(),
    Opts :: map()
) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
-callback schema(Format :: atom(), TypeRef :: spectra:sp_type_reference(), Opts :: map()) ->
    dynamic().

-optional_callbacks([schema/3]).

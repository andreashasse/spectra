-module(spectra_codec).

-callback encode(Format :: atom(), TypeRef :: spectra:sp_type_reference(), Data :: dynamic()) ->
    {ok, term()} | {error, [spectra:error()]}.
-callback decode(Format :: atom(), TypeRef :: spectra:sp_type_reference(), Input :: dynamic()) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
-callback schema(Format :: atom(), TypeRef :: spectra:sp_type_reference()) -> map().

-optional_callbacks([schema/2]).

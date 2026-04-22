-module(codec_string_continue_module).

%% A module with a local type and a codec that always returns `continue` for
%% the string format. Used to verify that spectra_string falls back to
%% structural encoding/decoding on `continue` rather than returning an error.

-behaviour(spectra_codec).

-type name() :: binary().

-export_type([name/0]).
-export([encode/6, decode/6]).

-spec encode(
    atom(),
    spectra:type_info(),
    spectra:sp_type_reference(),
    spectra:sp_type(),
    dynamic(),
    spectra:sp_config()
) ->
    spectra:codec_encode_result().
encode(_, _, _, _, _, _) ->
    continue.

-spec decode(
    atom(),
    spectra:type_info(),
    spectra:sp_type_reference(),
    spectra:sp_type(),
    dynamic(),
    spectra:sp_config()
) ->
    spectra:codec_decode_result().
decode(_, _, _, _, _, _) ->
    continue.

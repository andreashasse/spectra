-module(codec_string_continue_module).

%% A module with a local type and a codec that always returns `continue` for
%% the string format. Used to verify that spectra_string falls back to
%% structural encoding/decoding on `continue` rather than returning an error.

-behaviour(spectra_codec).

-type name() :: binary().

-export_type([name/0]).
-export([encode/4, decode/4]).

-spec encode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    spectra:codec_encode_result().
encode(_, _, _, _) ->
    continue.

-spec decode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    spectra:codec_decode_result().
decode(_, _, _, _) ->
    continue.

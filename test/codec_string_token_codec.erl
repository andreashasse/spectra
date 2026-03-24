-module(codec_string_token_codec).

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-export([encode/5, decode/5]).

-spec encode(atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type()) ->
    spectra:codec_encode_result().
encode(string, _Mod, {type, token, 0}, {token, Bin}, _Opts) when is_binary(Bin) ->
    {ok, binary_to_list(Bin)};
encode(string, _Mod, {type, token, 0}, Data, _Opts) ->
    {error, [sp_error:type_mismatch({type, token, 0}, Data)]}.

-spec decode(atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type()) ->
    spectra:codec_decode_result().
decode(string, _Mod, {type, token, 0}, String, _Opts) when is_list(String) ->
    {ok, {token, list_to_binary(String)}};
decode(string, _Mod, {type, token, 0}, Data, _Opts) ->
    {error, [sp_error:type_mismatch({type, token, 0}, Data)]}.

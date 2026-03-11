-module(codec_string_token_codec).

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-export([encode/4, decode/4]).

-spec encode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    spectra:codec_encode_result().
encode(string, {type, token, 0}, {token, Bin}, _Opts) when is_binary(Bin) ->
    {ok, binary_to_list(Bin)};
encode(string, {type, token, 0}, Data, _Opts) ->
    {error, [sp_error:type_mismatch({type, token, 0}, Data)]}.

-spec decode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    spectra:codec_decode_result().
decode(string, {type, token, 0}, String, _Opts) when is_list(String) ->
    {ok, {token, list_to_binary(String)}};
decode(string, {type, token, 0}, Data, _Opts) ->
    {error, [sp_error:type_mismatch({type, token, 0}, Data)]}.

-module(codec_string_token_codec).

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-export([encode/6, decode/6]).

-spec encode(atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type(), term()) ->
    spectra:codec_encode_result().
encode(string, _Mod, {type, token, 0}, {token, Bin}, _SpType, _Params) when is_binary(Bin) ->
    {ok, binary_to_list(Bin)};
encode(string, _Mod, {type, token, 0}, Data, _SpType, _Params) ->
    {error, [sp_error:type_mismatch({type, token, 0}, Data)]}.

-spec decode(atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type(), term()) ->
    spectra:codec_decode_result().
decode(string, _Mod, {type, token, 0}, String, _SpType, _Params) when is_list(String) ->
    {ok, {token, list_to_binary(String)}};
decode(string, _Mod, {type, token, 0}, Data, _SpType, _Params) ->
    {error, [sp_error:type_mismatch({type, token, 0}, Data)]}.

-module(codec_string_token_codec).

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

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
encode(string, _CallerTypeInfo, {type, token, 0}, _TargetType, {token, Bin}, _Config) when
    is_binary(Bin)
->
    {ok, binary_to_list(Bin)};
encode(string, _CallerTypeInfo, {type, token, 0}, _TargetType, Data, _Config) ->
    {error, [sp_error:type_mismatch({type, token, 0}, Data)]}.

-spec decode(
    atom(),
    spectra:type_info(),
    spectra:sp_type_reference(),
    spectra:sp_type(),
    dynamic(),
    spectra:sp_config()
) ->
    spectra:codec_decode_result().
decode(string, _CallerTypeInfo, {type, token, 0}, _TargetType, String, _Config) when
    is_list(String)
->
    {ok, {token, list_to_binary(String)}};
decode(string, _CallerTypeInfo, {type, token, 0}, _TargetType, Data, _Config) ->
    {error, [sp_error:type_mismatch({type, token, 0}, Data)]}.

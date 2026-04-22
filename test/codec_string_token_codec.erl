-module(codec_string_token_codec).

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-export([encode/6, decode/6]).

-spec encode(
    atom(),
    spectra:type_info(),
    spectra:sp_type_reference(),
    dynamic(),
    spectra:sp_type(),
    spectra:sp_config()
) ->
    spectra:codec_encode_result().
encode(string, _CallerTypeInfo, {type, token, 0}, {token, Bin}, _TargetType, _Config) when
    is_binary(Bin)
->
    {ok, binary_to_list(Bin)};
encode(string, _CallerTypeInfo, {type, token, 0}, Data, _TargetType, _Config) ->
    {error, [sp_error:type_mismatch({type, token, 0}, Data)]}.

-spec decode(
    atom(),
    spectra:type_info(),
    spectra:sp_type_reference(),
    dynamic(),
    spectra:sp_type(),
    spectra:sp_config()
) ->
    spectra:codec_decode_result().
decode(string, _CallerTypeInfo, {type, token, 0}, String, _TargetType, _Config) when
    is_list(String)
->
    {ok, {token, list_to_binary(String)}};
decode(string, _CallerTypeInfo, {type, token, 0}, Data, _TargetType, _Config) ->
    {error, [sp_error:type_mismatch({type, token, 0}, Data)]}.

-module(codec_appenv_type_codec).

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
encode(_, _CallerTypeInfo, {type, token, 0}, _TargetType, {token, Bin}, _Config) when
    is_binary(Bin)
->
    {ok, Bin};
encode(_, _CallerTypeInfo, {type, token, 0}, _TargetType, Data, _Config) ->
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
decode(_, _CallerTypeInfo, {type, token, 0}, _TargetType, Bin, _Config) when is_binary(Bin) ->
    {ok, {token, Bin}};
decode(_, _CallerTypeInfo, {type, token, 0}, _TargetType, Data, _Config) ->
    {error, [sp_error:type_mismatch({type, token, 0}, Data)]}.

-module(codec_no_schema_module).

%% Module that implements spectra_codec but NOT the optional schema/3 callback.
-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-opaque point() :: {float(), float()}.

-export_type([point/0]).
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
encode(_, _CallerTypeInfo, {type, point, 0}, {X, Y}, _TargetType, _Config) when
    is_number(X), is_number(Y)
->
    {ok, [X, Y]};
encode(_, _CallerTypeInfo, {type, point, 0}, Data, _TargetType, _Config) ->
    {error, [sp_error:type_mismatch({type, point, 0}, Data)]}.

-spec decode(
    atom(),
    spectra:type_info(),
    spectra:sp_type_reference(),
    dynamic(),
    spectra:sp_type(),
    spectra:sp_config()
) ->
    spectra:codec_decode_result().
decode(_, _CallerTypeInfo, {type, point, 0}, [X, Y], _TargetType, _Config) when
    is_number(X), is_number(Y)
->
    {ok, {X, Y}};
decode(_, _CallerTypeInfo, {type, point, 0}, Data, _TargetType, _Config) ->
    {error, [sp_error:type_mismatch({type, point, 0}, Data)]}.

-module(codec_appenv_rec_codec).

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-export([encode/6, decode/6, schema/5]).

-spec encode(
    atom(),
    spectra:type_info(),
    spectra:sp_type_reference(),
    spectra:sp_type(),
    dynamic(),
    spectra:sp_config()
) ->
    spectra:codec_encode_result().
encode(
    json, _CallerTypeInfo, {record, point2d}, _TargetType, #{x := X, y := Y}, _Config
) when
    is_number(X) andalso is_number(Y)
->
    {ok, [X, Y]};
encode(
    json, _CallerTypeInfo, {record, point2d}, _TargetType, {point2d, X, Y}, _Config
) when
    is_number(X) andalso is_number(Y)
->
    {ok, [X, Y]};
encode(json, _CallerTypeInfo, {record, point2d}, _TargetType, Data, _Config) ->
    {error, [sp_error:type_mismatch({record, point2d}, Data)]};
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
decode(json, _CallerTypeInfo, {record, point2d}, _TargetType, [X, Y], _Config) when
    is_number(X) andalso is_number(Y)
->
    {ok, #{x => X, y => Y}};
decode(json, _CallerTypeInfo, {record, point2d}, _TargetType, Data, _Config) ->
    {error, [sp_error:type_mismatch({record, point2d}, Data)]};
decode(_, _, _, _, _, _) ->
    continue.

-spec schema(
    atom(), spectra:type_info(), spectra:sp_type_reference(), spectra:sp_type(), spectra:sp_config()
) ->
    map() | continue.
schema(json_schema, _CallerTypeInfo, {record, point2d}, _TargetType, _Config) ->
    #{
        type => <<"array">>,
        items => #{type => <<"number">>},
        minItems => 2,
        maxItems => 2
    };
schema(_, _, _, _, _) ->
    continue.

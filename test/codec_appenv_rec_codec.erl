-module(codec_appenv_rec_codec).

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-export([encode/7, decode/7, schema/6]).

-spec encode(
    atom(),
    module(),
    spectra:sp_type_reference(),
    dynamic(),
    spectra:sp_type(),
    term(),
    spectra:sp_config()
) ->
    spectra:codec_encode_result().
encode(
    json, codec_appenv_rec_module, {record, point2d}, #{x := X, y := Y}, _SpType, _Params, _Config
) when
    is_number(X) andalso is_number(Y)
->
    {ok, [X, Y]};
encode(
    json, codec_appenv_rec_module, {record, point2d}, {point2d, X, Y}, _SpType, _Params, _Config
) when
    is_number(X) andalso is_number(Y)
->
    {ok, [X, Y]};
encode(json, codec_appenv_rec_module, {record, point2d}, Data, _SpType, _Params, _Config) ->
    {error, [sp_error:type_mismatch({record, point2d}, Data)]};
encode(_, _, _, _, _, _, _) ->
    continue.

-spec decode(
    atom(),
    module(),
    spectra:sp_type_reference(),
    dynamic(),
    spectra:sp_type(),
    term(),
    spectra:sp_config()
) ->
    spectra:codec_decode_result().
decode(json, codec_appenv_rec_module, {record, point2d}, [X, Y], _SpType, _Params, _Config) when
    is_number(X) andalso is_number(Y)
->
    {ok, #{x => X, y => Y}};
decode(json, codec_appenv_rec_module, {record, point2d}, Data, _SpType, _Params, _Config) ->
    {error, [sp_error:type_mismatch({record, point2d}, Data)]};
decode(_, _, _, _, _, _, _) ->
    continue.

-spec schema(
    atom(), module(), spectra:sp_type_reference(), spectra:sp_type(), term(), spectra:sp_config()
) ->
    map() | continue.
schema(json_schema, codec_appenv_rec_module, {record, point2d}, _SpType, _Params, _Config) ->
    #{
        type => <<"array">>,
        items => #{type => <<"number">>},
        minItems => 2,
        maxItems => 2
    };
schema(_, _, _, _, _, _) ->
    continue.

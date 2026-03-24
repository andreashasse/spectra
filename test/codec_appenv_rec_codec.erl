-module(codec_appenv_rec_codec).

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-export([encode/6, decode/6, schema/5]).

-spec encode(atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type(), term()) ->
    spectra:codec_encode_result().
encode(json, codec_appenv_rec_module, {record, point2d}, #{x := X, y := Y}, _SpType, _Params) when
    is_number(X) andalso is_number(Y)
->
    {ok, [X, Y]};
encode(json, codec_appenv_rec_module, {record, point2d}, {point2d, X, Y}, _SpType, _Params) when
    is_number(X) andalso is_number(Y)
->
    {ok, [X, Y]};
encode(json, codec_appenv_rec_module, {record, point2d}, Data, _SpType, _Params) ->
    {error, [sp_error:type_mismatch({record, point2d}, Data)]};
encode(_, _, _, _, _, _) ->
    continue.

-spec decode(atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type(), term()) ->
    spectra:codec_decode_result().
decode(json, codec_appenv_rec_module, {record, point2d}, [X, Y], _SpType, _Params) when
    is_number(X) andalso is_number(Y)
->
    {ok, #{x => X, y => Y}};
decode(json, codec_appenv_rec_module, {record, point2d}, Data, _SpType, _Params) ->
    {error, [sp_error:type_mismatch({record, point2d}, Data)]};
decode(_, _, _, _, _, _) ->
    continue.

-spec schema(atom(), module(), spectra:sp_type_reference(), spectra:sp_type(), term()) ->
    map() | continue.
schema(json_schema, codec_appenv_rec_module, {record, point2d}, _SpType, _Params) ->
    #{
        type => <<"array">>,
        items => #{type => <<"number">>},
        minItems => 2,
        maxItems => 2
    };
schema(_, _, _, _, _) ->
    continue.

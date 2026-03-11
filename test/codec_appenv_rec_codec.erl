-module(codec_appenv_rec_codec).

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-export([encode/4, decode/4, schema/3]).

-spec encode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    spectra:codec_encode_result().
encode(json, {record, point2d}, #{x := X, y := Y}, _Opts) when
    is_number(X) andalso is_number(Y)
->
    {ok, [X, Y]};
encode(json, {record, point2d}, {point2d, X, Y}, _Opts) when
    is_number(X) andalso is_number(Y)
->
    {ok, [X, Y]};
encode(json, {record, point2d}, Data, _Opts) ->
    {error, [sp_error:type_mismatch({record, point2d}, Data)]};
encode(_, _, _, _) ->
    continue.

-spec decode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    spectra:codec_decode_result().
decode(json, {record, point2d}, [X, Y], _Opts) when
    is_number(X) andalso is_number(Y)
->
    {ok, #{x => X, y => Y}};
decode(json, {record, point2d}, Data, _Opts) ->
    {error, [sp_error:type_mismatch({record, point2d}, Data)]};
decode(_, _, _, _) ->
    continue.

-spec schema(atom(), spectra:sp_type_reference(), map()) -> map() | continue.
schema(json_schema, {record, point2d}, _Opts) ->
    #{
        type => <<"array">>,
        items => #{type => <<"number">>},
        minItems => 2,
        maxItems => 2
    };
schema(_, _, _) ->
    continue.

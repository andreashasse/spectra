-module(codec_geo_module).

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-opaque point() :: {float(), float()}.

-export_type([point/0]).
-export([encode/4, decode/4, schema/3]).

-spec encode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    spectra:codec_encode_result().
encode(_, {type, point, 0}, {X, Y}, _Opts) when is_number(X), is_number(Y) ->
    {ok, [X, Y]};
encode(_, _, _, _) ->
    continue.

-spec decode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    spectra:codec_decode_result().
decode(_, {type, point, 0}, [X, Y], _Opts) when is_number(X), is_number(Y) ->
    {ok, {X, Y}};
decode(_, _, _, _) ->
    continue.

-spec schema(atom(), spectra:sp_type_reference(), map()) -> map().
schema(json_schema, {type, point, 0}, _Opts) ->
    #{
        type => <<"array">>,
        items => #{type => <<"number">>},
        minItems => 2,
        maxItems => 2
    }.

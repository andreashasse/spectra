-module(codec_geo_module).

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-opaque point() :: {float(), float()}.

-export_type([point/0]).
-export([encode/4, decode/4, schema/3]).

-spec encode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    {ok, term()} | {error, [spectra:error()]}.
encode(_, {type, point, 0}, {X, Y}, _Opts) when is_number(X), is_number(Y) ->
    {ok, [X, Y]};
encode(_, TypeRef, Data, _Opts) ->
    {error, [sp_error:type_mismatch(TypeRef, Data)]}.

-spec decode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
decode(_, {type, point, 0}, [X, Y], _Opts) when is_number(X), is_number(Y) ->
    {ok, {X, Y}};
decode(_, TypeRef, Data, _Opts) ->
    {error, [sp_error:type_mismatch(TypeRef, Data)]}.

-spec schema(atom(), spectra:sp_type_reference(), map()) -> map().
schema(json_schema, {type, point, 0}, _Opts) ->
    #{
        type => <<"array">>,
        items => #{type => <<"number">>},
        minItems => 2,
        maxItems => 2
    }.

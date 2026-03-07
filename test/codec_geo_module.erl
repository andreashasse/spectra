-module(codec_geo_module).

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-opaque point() :: {float(), float()}.

-export_type([point/0]).
-export([encode/3, decode/3, schema/2]).

-spec encode(atom(), spectra:sp_type_reference(), dynamic()) ->
    {ok, term()} | {error, [spectra:error()]}.
encode(_, {type, point, 0}, {X, Y}) when is_number(X), is_number(Y) ->
    {ok, [X, Y]};
encode(_, TypeRef, Data) ->
    {error, [sp_error:type_mismatch(TypeRef, Data)]}.

-spec decode(atom(), spectra:sp_type_reference(), dynamic()) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
decode(_, {type, point, 0}, [X, Y]) when is_number(X), is_number(Y) ->
    {ok, {X, Y}};
decode(_, TypeRef, Data) ->
    {error, [sp_error:type_mismatch(TypeRef, Data)]}.

-spec schema(atom(), spectra:sp_type_reference()) -> map().
schema(json_schema, {type, point, 0}) ->
    #{
        type => <<"array">>,
        items => #{type => <<"number">>},
        minItems => 2,
        maxItems => 2
    }.

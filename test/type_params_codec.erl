-module(type_params_codec).

%% A codec that echoes received params back in its results, used for testing
%% the type_parameters feature.

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-export([encode/4, decode/4, schema/3]).

-spec encode(atom(), spectra:sp_type_reference(), dynamic(), term()) ->
    spectra:codec_encode_result().
encode(_Format, {type, parameterized_type, 0}, Data, Params) ->
    {ok, {encoded, Params, Data}};
encode(_Format, {type, no_params_type, 0}, Data, Params) ->
    {ok, {encoded, Params, Data}};
encode(_Format, {record, parameterized_rec}, Data, Params) ->
    {ok, {encoded, Params, Data}};
encode(_, _, _, _) ->
    continue.

-spec decode(atom(), spectra:sp_type_reference(), dynamic(), term()) ->
    spectra:codec_decode_result().
decode(_Format, {type, parameterized_type, 0}, Data, Params) ->
    {ok, {decoded, Params, Data}};
decode(_Format, {type, no_params_type, 0}, Data, Params) ->
    {ok, {decoded, Params, Data}};
decode(_Format, {record, parameterized_rec}, Data, Params) ->
    {ok, {decoded, Params, Data}};
decode(_, _, _, _) ->
    continue.

-spec schema(atom(), spectra:sp_type_reference(), term()) -> dynamic().
schema(json_schema, {type, parameterized_type, 0}, Params) ->
    #{<<"type">> => <<"string">>, <<"params">> => Params};
schema(json_schema, {type, no_params_type, 0}, Params) ->
    #{<<"type">> => <<"string">>, <<"params">> => Params};
schema(json_schema, {record, parameterized_rec}, Params) ->
    #{<<"type">> => <<"object">>, <<"params">> => Params};
schema(_, _, _) ->
    continue.

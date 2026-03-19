-module(type_params_codec).

%% A codec that echoes received params back in its results, used for testing
%% the type_parameters feature.

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-export([encode/5, decode/5, schema/4]).

-spec encode(atom(), module(), spectra:sp_type_reference(), dynamic(), term()) ->
    spectra:codec_encode_result().
encode(_Format, _Mod, {type, parameterized_type, 0}, Data, Params) ->
    {ok, {encoded, Params, Data}};
encode(_Format, _Mod, {type, no_params_type, 0}, Data, Params) ->
    {ok, {encoded, Params, Data}};
encode(_Format, _Mod, {record, parameterized_rec}, Data, Params) ->
    {ok, {encoded, Params, Data}};
encode(_, _, _, _, _) ->
    continue.

-spec decode(atom(), module(), spectra:sp_type_reference(), dynamic(), term()) ->
    spectra:codec_decode_result().
decode(_Format, _Mod, {type, parameterized_type, 0}, Data, Params) ->
    {ok, {decoded, Params, Data}};
decode(_Format, _Mod, {type, no_params_type, 0}, Data, Params) ->
    {ok, {decoded, Params, Data}};
decode(_Format, _Mod, {record, parameterized_rec}, Data, Params) ->
    {ok, {decoded, Params, Data}};
decode(_, _, _, _, _) ->
    continue.

-spec schema(atom(), module(), spectra:sp_type_reference(), term()) -> dynamic().
schema(json_schema, _Mod, {type, parameterized_type, 0}, Params) ->
    #{<<"type">> => <<"string">>, <<"params">> => Params};
schema(json_schema, _Mod, {type, no_params_type, 0}, Params) ->
    #{<<"type">> => <<"string">>, <<"params">> => Params};
schema(json_schema, _Mod, {record, parameterized_rec}, Params) ->
    #{<<"type">> => <<"object">>, <<"params">> => Params};
schema(_, _, _, _) ->
    continue.

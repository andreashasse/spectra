-module(type_params_codec).

%% A codec that echoes received params back in its results, used for testing
%% the type_parameters feature.

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-export([encode/6, decode/6, schema/5]).

-spec encode(atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type(), term()) ->
    spectra:codec_encode_result().
encode(_Format, _Mod, {type, parameterized_type, 0}, Data, _SpType, Params) ->
    {ok, {encoded, Params, Data}};
encode(_Format, _Mod, {type, no_params_type, 0}, Data, _SpType, Params) ->
    {ok, {encoded, Params, Data}};
encode(_Format, _Mod, {record, parameterized_rec}, Data, _SpType, Params) ->
    {ok, {encoded, Params, Data}};
encode(_, _, _, _, _, _) ->
    continue.

-spec decode(atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type(), term()) ->
    spectra:codec_decode_result().
decode(_Format, _Mod, {type, parameterized_type, 0}, Data, _SpType, Params) ->
    {ok, {decoded, Params, Data}};
decode(_Format, _Mod, {type, no_params_type, 0}, Data, _SpType, Params) ->
    {ok, {decoded, Params, Data}};
decode(_Format, _Mod, {record, parameterized_rec}, Data, _SpType, Params) ->
    {ok, {decoded, Params, Data}};
decode(_, _, _, _, _, _) ->
    continue.

-spec schema(atom(), module(), spectra:sp_type_reference(), spectra:sp_type(), term()) -> dynamic().
schema(json_schema, _Mod, {type, parameterized_type, 0}, _SpType, Params) ->
    #{<<"type">> => <<"string">>, <<"params">> => Params};
schema(json_schema, _Mod, {type, no_params_type, 0}, _SpType, Params) ->
    #{<<"type">> => <<"string">>, <<"params">> => Params};
schema(json_schema, _Mod, {record, parameterized_rec}, _SpType, Params) ->
    #{<<"type">> => <<"object">>, <<"params">> => Params};
schema(_, _, _, _, _) ->
    continue.

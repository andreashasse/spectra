-module(type_params_codec).

%% A codec that echoes received params back in its results, used for testing
%% the type_parameters feature.

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-export([encode/5, decode/5, schema/4]).

-spec encode(atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type()) ->
    spectra:codec_encode_result().
encode(_Format, _Mod, {type, parameterized_type, 0}, Data, SpType) ->
    {ok, {encoded, spectra_type:parameters(SpType), Data}};
encode(_Format, _Mod, {type, no_params_type, 0}, Data, SpType) ->
    {ok, {encoded, spectra_type:parameters(SpType), Data}};
encode(_Format, _Mod, {record, parameterized_rec}, Data, SpType) ->
    {ok, {encoded, spectra_type:parameters(SpType), Data}};
encode(_, _, _, _, _) ->
    continue.

-spec decode(atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type()) ->
    spectra:codec_decode_result().
decode(_Format, _Mod, {type, parameterized_type, 0}, Data, SpType) ->
    {ok, {decoded, spectra_type:parameters(SpType), Data}};
decode(_Format, _Mod, {type, no_params_type, 0}, Data, SpType) ->
    {ok, {decoded, spectra_type:parameters(SpType), Data}};
decode(_Format, _Mod, {record, parameterized_rec}, Data, SpType) ->
    {ok, {decoded, spectra_type:parameters(SpType), Data}};
decode(_, _, _, _, _) ->
    continue.

-spec schema(atom(), module(), spectra:sp_type_reference(), spectra:sp_type()) -> dynamic().
schema(json_schema, _Mod, {type, parameterized_type, 0}, SpType) ->
    #{<<"type">> => <<"string">>, <<"params">> => spectra_type:parameters(SpType)};
schema(json_schema, _Mod, {type, no_params_type, 0}, SpType) ->
    #{<<"type">> => <<"string">>, <<"params">> => spectra_type:parameters(SpType)};
schema(json_schema, _Mod, {record, parameterized_rec}, SpType) ->
    #{<<"type">> => <<"object">>, <<"params">> => spectra_type:parameters(SpType)};
schema(_, _, _, _) ->
    continue.

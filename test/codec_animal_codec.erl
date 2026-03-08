-module(codec_animal_codec).

%% A discriminated-union codec for animal().
%%
%% Encoding: pattern-match on the record tag to add the <<"type">> discriminator
%% field, then delegate structural field encoding to spectra.
%%
%% Decoding: inspect the <<"type">> field to pick the right record, remove the
%% discriminator, then delegate structural decoding to spectra.

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-record(cat, {name :: binary(), indoor :: boolean()}).
-record(dog, {name :: binary(), breed :: binary()}).

-type animal() :: #cat{} | #dog{}.

-export_type([animal/0]).
-export([encode/4, decode/4]).

-spec encode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    {ok, term()} | {error, [spectra:error()]}.
encode(Format, {type, animal, 0}, #cat{} = Cat, _Opts) ->
    case spectra:encode(Format, ?MODULE, {record, cat}, Cat, [pre_encoded]) of
        {ok, Fields} when is_map(Fields) -> {ok, maps:put(<<"type">>, <<"cat">>, Fields)};
        {error, _} = Err -> Err
    end;
encode(Format, {type, animal, 0}, #dog{} = Dog, _Opts) ->
    case spectra:encode(Format, ?MODULE, {record, dog}, Dog, [pre_encoded]) of
        {ok, Fields} when is_map(Fields) -> {ok, maps:put(<<"type">>, <<"dog">>, Fields)};
        {error, _} = Err -> Err
    end;
encode(_, TypeRef, Data, _Opts) ->
    {error, [sp_error:type_mismatch(TypeRef, Data)]}.

-spec decode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
decode(Format, {type, animal, 0}, #{<<"type">> := <<"cat">>} = Json, _Opts) ->
    spectra:decode(Format, ?MODULE, {record, cat}, maps:remove(<<"type">>, Json), [pre_decoded]);
decode(Format, {type, animal, 0}, #{<<"type">> := <<"dog">>} = Json, _Opts) ->
    spectra:decode(Format, ?MODULE, {record, dog}, maps:remove(<<"type">>, Json), [pre_decoded]);
decode(_, TypeRef, Data, _Opts) ->
    {error, [sp_error:type_mismatch(TypeRef, Data)]}.

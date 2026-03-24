-module(codec_animal_codec).

%% A discriminated-union codec for animal().
%%
%% Encoding: pattern-match on the type tag to add the <<"type">> discriminator
%% field, then delegate structural field encoding to spectra via `continue`.
%%
%% Decoding: inspect the <<"type">> field to pick the right type, remove the
%% discriminator, then delegate structural decoding to spectra via `continue`.

-behaviour(spectra_codec).

-record(cat, {name :: binary(), indoor :: boolean()}).
-record(dog, {name :: binary(), breed :: binary()}).

-type cat() :: #cat{}.
-type dog() :: #dog{}.
-type animal() :: cat() | dog().
-type zoo() :: [codec_animal_codec:animal()].

-export_type([animal/0, cat/0, dog/0, zoo/0]).
-export([encode/5, decode/5]).

-spec encode(atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type()) ->
    spectra:codec_encode_result().
encode(Format, _Mod, {type, animal, 0}, #cat{} = Cat, _Opts) ->
    case spectra:encode(Format, ?MODULE, {type, cat, 0}, Cat, [pre_encoded]) of
        {ok, Fields} when is_map(Fields) -> {ok, maps:put(<<"type">>, <<"cat">>, Fields)};
        {error, _} = Err -> Err
    end;
encode(Format, _Mod, {type, animal, 0}, #dog{} = Dog, _Opts) ->
    case spectra:encode(Format, ?MODULE, {type, dog, 0}, Dog, [pre_encoded]) of
        {ok, Fields} when is_map(Fields) -> {ok, maps:put(<<"type">>, <<"dog">>, Fields)};
        {error, _} = Err -> Err
    end;
encode(_, _Mod, {type, animal, 0}, Data, _Opts) ->
    {error, [sp_error:type_mismatch({type, animal, 0}, Data)]};
encode(_, _, _, _, _) ->
    continue.

-spec decode(atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type()) ->
    spectra:codec_decode_result().
decode(Format, _Mod, {type, animal, 0}, #{<<"type">> := <<"cat">>} = Json, _Opts) ->
    spectra:decode(Format, ?MODULE, {type, cat, 0}, maps:remove(<<"type">>, Json), [pre_decoded]);
decode(Format, _Mod, {type, animal, 0}, #{<<"type">> := <<"dog">>} = Json, _Opts) ->
    spectra:decode(Format, ?MODULE, {type, dog, 0}, maps:remove(<<"type">>, Json), [pre_decoded]);
decode(_, _Mod, {type, animal, 0}, Data, _Opts) ->
    {error, [sp_error:type_mismatch({type, animal, 0}, Data)]};
decode(_, _, _, _, _) ->
    continue.

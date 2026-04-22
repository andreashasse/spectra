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
-export([encode/6, decode/6]).

-spec encode(
    atom(),
    spectra:type_info(),
    spectra:sp_type_reference(),
    spectra:sp_type(),
    dynamic(),
    spectra:sp_config()
) ->
    spectra:codec_encode_result().
encode(Format, _CallerTypeInfo, {type, animal, 0}, _TargetType, #cat{} = Cat, _Config) ->
    case spectra:encode(Format, ?MODULE, {type, cat, 0}, Cat, [pre_encoded]) of
        {ok, Fields} when is_map(Fields) -> {ok, maps:put(<<"type">>, <<"cat">>, Fields)};
        {error, _} = Err -> Err
    end;
encode(Format, _CallerTypeInfo, {type, animal, 0}, _TargetType, #dog{} = Dog, _Config) ->
    case spectra:encode(Format, ?MODULE, {type, dog, 0}, Dog, [pre_encoded]) of
        {ok, Fields} when is_map(Fields) -> {ok, maps:put(<<"type">>, <<"dog">>, Fields)};
        {error, _} = Err -> Err
    end;
encode(_, _CallerTypeInfo, {type, animal, 0}, _TargetType, Data, _Config) ->
    {error, [sp_error:type_mismatch({type, animal, 0}, Data)]};
encode(_, _, _, _, _, _) ->
    continue.

-spec decode(
    atom(),
    spectra:type_info(),
    spectra:sp_type_reference(),
    spectra:sp_type(),
    dynamic(),
    spectra:sp_config()
) ->
    spectra:codec_decode_result().
decode(
    Format,
    _CallerTypeInfo,
    {type, animal, 0},
    _TargetType,
    #{<<"type">> := <<"cat">>} = Json,
    _Config
) ->
    spectra:decode(Format, ?MODULE, {type, cat, 0}, maps:remove(<<"type">>, Json), [pre_decoded]);
decode(
    Format,
    _CallerTypeInfo,
    {type, animal, 0},
    _TargetType,
    #{<<"type">> := <<"dog">>} = Json,
    _Config
) ->
    spectra:decode(Format, ?MODULE, {type, dog, 0}, maps:remove(<<"type">>, Json), [pre_decoded]);
decode(_, _CallerTypeInfo, {type, animal, 0}, _TargetType, Data, _Config) ->
    {error, [sp_error:type_mismatch({type, animal, 0}, Data)]};
decode(_, _, _, _, _, _) ->
    continue.

-module(custom_codec_test).

-include_lib("eunit/include/eunit.hrl").
-include("../include/spectra_internal.hrl").

%% 1. Module with -behaviour(spectra_codec) encodes without config
auto_discovery_encode_test() ->
    ?assertEqual(
        {ok, [1.0, 2.0]},
        spectra:encode(json, codec_geo_module, {type, point, 0}, {1.0, 2.0}, [pre_encoded])
    ).

%% 2. Same module, decode direction
auto_discovery_decode_test() ->
    ?assertEqual(
        {ok, {1.0, 2.0}},
        spectra:decode(json, codec_geo_module, {type, point, 0}, [1.0, 2.0], [pre_decoded])
    ).

%% 3. Same module, schema (optional callback present)
auto_discovery_schema_test() ->
    Schema = iolist_to_binary(spectra:schema(json_schema, codec_geo_module, {type, point, 0})),
    Decoded = json:decode(Schema),
    ?assertMatch(
        #{
            <<"type">> := <<"array">>,
            <<"items">> := #{<<"type">> := <<"number">>},
            <<"minItems">> := 2,
            <<"maxItems">> := 2
        },
        Decoded
    ).

%% 4. Codec in app env, encode works for a module that uses calendar:datetime()
app_env_remote_codec_encode_test() ->
    application:set_env(spectra, codecs, #{{calendar, {type, datetime, 0}} => codec_calendar_codec}),
    try
        Event = #{name => <<"Party">>, at => {{2024, 1, 15}, {10, 30, 0}}},
        {ok, Encoded} = spectra:encode(
            json, codec_middle_module, {type, event, 0}, Event, [pre_encoded]
        ),
        ?assertMatch(#{<<"name">> := <<"Party">>, <<"at">> := <<"2024-01-15T10:30:00">>}, Encoded)
    after
        application:unset_env(spectra, codecs)
    end.

%% 5. App env codec, decode direction
app_env_remote_codec_decode_test() ->
    application:set_env(spectra, codecs, #{{calendar, {type, datetime, 0}} => codec_calendar_codec}),
    try
        Json = #{<<"name">> => <<"Party">>, <<"at">> => <<"2024-01-15T10:30:00">>},
        ?assertEqual(
            {ok, #{name => <<"Party">>, at => {{2024, 1, 15}, {10, 30, 0}}}},
            spectra:decode(json, codec_middle_module, {type, event, 0}, Json, [pre_decoded])
        )
    after
        application:unset_env(spectra, codecs)
    end.

%% 6. Cascading: codec_top_module -> codec_middle_module -> calendar:datetime()
%% app env codec for calendar:datetime() resolves through two intermediate modules.
cascading_encode_test() ->
    application:set_env(spectra, codecs, #{{calendar, {type, datetime, 0}} => codec_calendar_codec}),
    try
        Event = #{name => <<"Party">>, at => {{2024, 6, 1}, {18, 0, 0}}},
        Schedule = #{events => [Event]},
        {ok, Encoded} = spectra:encode(
            json, codec_top_module, {type, schedule, 0}, Schedule, [pre_encoded]
        ),
        ?assertMatch(
            #{
                <<"events">> := [
                    #{<<"name">> := <<"Party">>, <<"at">> := <<"2024-06-01T18:00:00">>}
                ]
            },
            Encoded
        )
    after
        application:unset_env(spectra, codecs)
    end.

%% 7. Existing behaviour preserved when no codec registered — tuple type still throws
no_codec_still_throws_test() ->
    application:unset_env(spectra, codecs),
    ?assertError(
        {type_not_supported, _},
        spectra:encode(json, codec_tuple_module, {type, pair, 0}, {1, 2})
    ).

%% 8. Verify codec callbacks receive the correct sp_type_reference (tuple form)
type_ref_passed_to_codec_test() ->
    %% Passing wrong data to the geo codec should return {error,_} (codec's catch-all),
    %% proving the codec was invoked with {type, point, 0}.
    ?assertMatch(
        {error, _},
        spectra:encode(json, codec_geo_module, {type, point, 0}, not_a_tuple, [pre_encoded])
    ).

%% 9. Remote module that implements spectra_codec is used automatically (no app env)
remote_module_implements_codec_test() ->
    application:unset_env(spectra, codecs),
    Color = {255, 128, 0},
    {ok, Encoded} = spectra:encode(
        json, codec_uses_remote_module, {type, palette, 0}, #{primary => Color}, [pre_encoded]
    ),
    ?assertMatch(#{<<"primary">> := <<"#FF8000">>}, Encoded),
    {ok, Decoded} = spectra:decode(
        json,
        codec_uses_remote_module,
        {type, palette, 0},
        #{<<"primary">> => <<"#FF8000">>},
        [pre_decoded]
    ),
    ?assertEqual(#{primary => Color}, Decoded).

%% 10. Schema raises an exception when schema/2 not exported by codec
schema_optional_callback_test() ->
    %% codec_no_schema_module implements encode/decode but NOT schema/2.
    %% Calling schema/2 must raise an explicit error rather than silently falling through.
    ?assertError(
        {schema_not_implemented, codec_no_schema_module, {type, point, 0}},
        spectra:schema(json_schema, codec_no_schema_module, {type, point, 0})
    ).

%% 11. Discriminated-union codec: encode cat
animal_encode_cat_test() ->
    Cat = {cat, <<"Whiskers">>, true},
    ?assertEqual(
        {ok, #{<<"type">> => <<"cat">>, <<"name">> => <<"Whiskers">>, <<"indoor">> => true}},
        spectra:encode(json, codec_animal_codec, {type, animal, 0}, Cat, [pre_encoded])
    ).

%% 12. Discriminated-union codec: encode dog
animal_encode_dog_test() ->
    Dog = {dog, <<"Rex">>, <<"Labrador">>},
    ?assertEqual(
        {ok, #{<<"type">> => <<"dog">>, <<"name">> => <<"Rex">>, <<"breed">> => <<"Labrador">>}},
        spectra:encode(json, codec_animal_codec, {type, animal, 0}, Dog, [pre_encoded])
    ).

%% 13. Discriminated-union codec: decode cat
animal_decode_cat_test() ->
    Json = #{<<"type">> => <<"cat">>, <<"name">> => <<"Whiskers">>, <<"indoor">> => true},
    ?assertEqual(
        {ok, {cat, <<"Whiskers">>, true}},
        spectra:decode(json, codec_animal_codec, {type, animal, 0}, Json, [pre_decoded])
    ).

%% 14. Discriminated-union codec: decode dog
animal_decode_dog_test() ->
    Json = #{<<"type">> => <<"dog">>, <<"name">> => <<"Rex">>, <<"breed">> => <<"Labrador">>},
    ?assertEqual(
        {ok, {dog, <<"Rex">>, <<"Labrador">>}},
        spectra:decode(json, codec_animal_codec, {type, animal, 0}, Json, [pre_decoded])
    ).

%% 15. Discriminated-union codec: invalid data returns error
animal_encode_invalid_test() ->
    ?assertMatch(
        {error, _},
        spectra:encode(json, codec_animal_codec, {type, animal, 0}, not_an_animal, [pre_encoded])
    ).

%% 16. Discriminated-union codec: unknown type tag returns error
animal_decode_unknown_tag_test() ->
    Json = #{<<"type">> => <<"fish">>, <<"name">> => <<"Nemo">>},
    ?assertMatch(
        {error, _},
        spectra:decode(json, codec_animal_codec, {type, animal, 0}, Json, [pre_decoded])
    ).

%% 17. Atom, {type,N,A} and bare sp_type() all produce the same encode/decode result
animal_type_ref_forms_test() ->
    TypeInfo = spectra_module_types:get(codec_animal_codec),
    AnimalSpType = spectra_type_info:get_type(TypeInfo, animal, 0),

    Cat = {cat, <<"Whiskers">>, true},
    CatJson = #{<<"type">> => <<"cat">>, <<"name">> => <<"Whiskers">>, <<"indoor">> => true},

    EncAtom = spectra:encode(json, codec_animal_codec, animal, Cat, [pre_encoded]),
    EncTupleRef = spectra:encode(json, codec_animal_codec, {type, animal, 0}, Cat, [pre_encoded]),
    EncSpType = spectra:encode(json, TypeInfo, AnimalSpType, Cat, [pre_encoded]),
    ?assertEqual({ok, CatJson}, EncAtom),
    ?assertEqual(EncAtom, EncTupleRef),
    ?assertEqual(EncAtom, EncSpType),

    DecAtom = spectra:decode(json, codec_animal_codec, animal, CatJson, [pre_decoded]),
    DecTupleRef = spectra:decode(json, codec_animal_codec, {type, animal, 0}, CatJson, [pre_decoded]),
    DecSpType = spectra:decode(json, TypeInfo, AnimalSpType, CatJson, [pre_decoded]),
    ?assertEqual({ok, Cat}, DecAtom),
    ?assertEqual(DecAtom, DecTupleRef),
    ?assertEqual(DecAtom, DecSpType).

%% 18. {record,N} ref and bare #sp_rec{} sp_type() produce the same encode/decode result
cat_record_ref_forms_test() ->
    TypeInfo = spectra_module_types:get(codec_animal_codec),
    {ok, CatSpRec} = spectra_type_info:find_record(TypeInfo, cat),

    Cat = {cat, <<"Whiskers">>, true},
    CatJson = #{<<"name">> => <<"Whiskers">>, <<"indoor">> => true},

    EncTupleRef = spectra:encode(json, codec_animal_codec, {record, cat}, Cat, [pre_encoded]),
    EncSpType = spectra:encode(json, TypeInfo, CatSpRec, Cat, [pre_encoded]),
    ?assertEqual({ok, CatJson}, EncTupleRef),
    ?assertEqual(EncTupleRef, EncSpType),

    DecTupleRef = spectra:decode(json, codec_animal_codec, {record, cat}, CatJson, [pre_decoded]),
    DecSpType = spectra:decode(json, TypeInfo, CatSpRec, CatJson, [pre_decoded]),
    ?assertEqual({ok, Cat}, DecTupleRef),
    ?assertEqual(DecTupleRef, DecSpType).

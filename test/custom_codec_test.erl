-module(custom_codec_test).

-include_lib("eunit/include/eunit.hrl").
-include("../include/spectra_internal.hrl").

%% Copied from codec_animal_codec to avoid a shared header file.
-record(cat, {name :: binary(), indoor :: boolean()}).
-record(dog, {name :: binary(), breed :: binary()}).

auto_discovery_encode_test() ->
    ?assertEqual(
        {ok, [1.0, 2.0]},
        spectra:encode(json, codec_geo_module, {type, point, 0}, {1.0, 2.0}, [pre_encoded])
    ).

auto_discovery_decode_test() ->
    ?assertEqual(
        {ok, {1.0, 2.0}},
        spectra:decode(json, codec_geo_module, {type, point, 0}, [1.0, 2.0], [pre_decoded])
    ).

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

no_codec_still_throws_test() ->
    application:unset_env(spectra, codecs),
    ?assertError(
        {type_not_supported, _},
        spectra:encode(json, codec_tuple_module, {type, pair, 0}, {1, 2})
    ).

type_ref_passed_to_codec_test() ->
    ?assertMatch(
        {error, _},
        spectra:encode(json, codec_geo_module, {type, point, 0}, not_a_tuple, [pre_encoded])
    ).

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

schema_optional_callback_test() ->
    ?assertError(
        {schema_not_implemented, codec_no_schema_module, {type, point, 0}},
        spectra:schema(json_schema, codec_no_schema_module, {type, point, 0})
    ).

animal_encode_cat_test() ->
    Cat = #cat{name = <<"Whiskers">>, indoor = true},
    ?assertEqual(
        {ok, #{<<"type">> => <<"cat">>, <<"name">> => <<"Whiskers">>, <<"indoor">> => true}},
        spectra:encode(json, codec_animal_codec, {type, animal, 0}, Cat, [pre_encoded])
    ).

animal_encode_dog_test() ->
    Dog = #dog{name = <<"Rex">>, breed = <<"Labrador">>},
    ?assertEqual(
        {ok, #{<<"type">> => <<"dog">>, <<"name">> => <<"Rex">>, <<"breed">> => <<"Labrador">>}},
        spectra:encode(json, codec_animal_codec, {type, animal, 0}, Dog, [pre_encoded])
    ).

animal_decode_cat_test() ->
    Json = #{<<"type">> => <<"cat">>, <<"name">> => <<"Whiskers">>, <<"indoor">> => true},
    ?assertEqual(
        {ok, #cat{name = <<"Whiskers">>, indoor = true}},
        spectra:decode(json, codec_animal_codec, {type, animal, 0}, Json, [pre_decoded])
    ).

animal_decode_dog_test() ->
    Json = #{<<"type">> => <<"dog">>, <<"name">> => <<"Rex">>, <<"breed">> => <<"Labrador">>},
    ?assertEqual(
        {ok, #dog{name = <<"Rex">>, breed = <<"Labrador">>}},
        spectra:decode(json, codec_animal_codec, {type, animal, 0}, Json, [pre_decoded])
    ).

animal_encode_invalid_test() ->
    ?assertMatch(
        {error, _},
        spectra:encode(json, codec_animal_codec, {type, animal, 0}, not_an_animal, [pre_encoded])
    ).

animal_decode_unknown_tag_test() ->
    Json = #{<<"type">> => <<"fish">>, <<"name">> => <<"Nemo">>},
    ?assertMatch(
        {error, _},
        spectra:decode(json, codec_animal_codec, {type, animal, 0}, Json, [pre_decoded])
    ).

animal_type_ref_forms_test() ->
    TypeInfo = spectra_module_types:get(codec_animal_codec),
    AnimalSpType = spectra_type_info:get_type(TypeInfo, animal, 0),

    Cat = #cat{name = <<"Whiskers">>, indoor = true},
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

cat_record_ref_forms_test() ->
    TypeInfo = spectra_module_types:get(codec_animal_codec),
    {ok, CatSpRec} = spectra_type_info:find_record(TypeInfo, cat),

    Cat = #cat{name = <<"Whiskers">>, indoor = true},
    CatJson = #{<<"name">> => <<"Whiskers">>, <<"indoor">> => true},

    EncTupleRef = spectra:encode(json, codec_animal_codec, {record, cat}, Cat, [pre_encoded]),
    EncSpType = spectra:encode(json, TypeInfo, CatSpRec, Cat, [pre_encoded]),
    ?assertEqual({ok, CatJson}, EncTupleRef),
    ?assertEqual(EncTupleRef, EncSpType),

    DecTupleRef = spectra:decode(json, codec_animal_codec, {record, cat}, CatJson, [pre_decoded]),
    DecSpType = spectra:decode(json, TypeInfo, CatSpRec, CatJson, [pre_decoded]),
    ?assertEqual({ok, Cat}, DecTupleRef),
    ?assertEqual(DecTupleRef, DecSpType).

%% {record, cat} dispatches to codec; codec returns `continue`, so structural
%% encoding takes over.
record_ref_codec_dispatch_continue_test() ->
    Cat = #cat{name = <<"Luna">>, indoor = false},
    CatJson = #{<<"name">> => <<"Luna">>, <<"indoor">> => false},
    ?assertEqual(
        {ok, CatJson},
        spectra:encode(json, codec_animal_codec, {record, cat}, Cat, [pre_encoded])
    ),
    ?assertEqual(
        {ok, Cat},
        spectra:decode(json, codec_animal_codec, {record, cat}, CatJson, [pre_decoded])
    ).

%% A union type like `point() | undefined` must dispatch through the codec for
%% `point()` when trying union alternatives, not fall through to structural
%% encoding of the opaque tuple, which crashes with {type_not_supported, _}.
union_with_remote_codec_type_encode_test() ->
    ?assertEqual(
        {ok, [1.0, 2.0]},
        spectra:encode(
            json, codec_geo_module, {type, maybe_point, 0}, {1.0, 2.0}, [pre_encoded]
        )
    ),
    ?assertEqual(
        {ok, <<"undefined">>},
        spectra:encode(json, codec_geo_module, {type, maybe_point, 0}, undefined, [pre_encoded])
    ).

union_with_remote_codec_type_decode_test() ->
    ?assertEqual(
        {ok, {1.0, 2.0}},
        spectra:decode(
            json, codec_geo_module, {type, maybe_point, 0}, [1.0, 2.0], [pre_decoded]
        )
    ),
    ?assertEqual(
        {ok, undefined},
        spectra:decode(json, codec_geo_module, {type, maybe_point, 0}, null, [pre_decoded])
    ).

schema_type_ref_forms_test() ->
    TypeInfo = spectra_module_types:get(codec_geo_module),
    PointSpType = spectra_type_info:get_type(TypeInfo, point, 0),

    SchemaAtom = spectra:schema(json_schema, codec_geo_module, point),
    SchemaTupleRef = spectra:schema(json_schema, codec_geo_module, {type, point, 0}),
    SchemaSpType = spectra:schema(json_schema, TypeInfo, PointSpType),
    ?assertEqual(SchemaTupleRef, SchemaAtom),
    ?assertEqual(SchemaTupleRef, SchemaSpType).

schema_record_ref_forms_test() ->
    TypeInfo = spectra_module_types:get(record_test),
    {ok, PersonSpRec} = spectra_type_info:find_record(TypeInfo, person),

    SchemaRecordRef = spectra:schema(json_schema, record_test, {record, person}),
    SchemaSpType = spectra:schema(json_schema, TypeInfo, PersonSpRec),
    ?assertEqual(SchemaRecordRef, SchemaSpType).

%% A module that declares spectra_codec as its *second* -behaviour attribute
%% must be recognised as a codec. The bug: proplists:get_value/3 returns only
%% the first -behaviour match, so the codec is silently skipped and the opaque
%% tuple type falls through to structural encoding, which crashes.
multi_behaviour_codec_detected_test() ->
    ?assertEqual(
        {ok, [1.0, 2.0]},
        spectra:encode(
            json, codec_multi_behaviour_module, {type, point, 0}, {1.0, 2.0}, [pre_encoded]
        )
    ).

multi_behaviour_codec_decode_test() ->
    ?assertEqual(
        {ok, {1.0, 2.0}},
        spectra:decode(
            json, codec_multi_behaviour_module, {type, point, 0}, [1.0, 2.0], [pre_decoded]
        )
    ).

%% zoo() uses a fully-qualified self-reference codec_animal_codec:animal(), verifying
%% that remote type refs within the same codec module are dispatched correctly.
zoo_encode_test() ->
    Zoo = [
        #cat{name = <<"Whiskers">>, indoor = true}, #dog{name = <<"Rex">>, breed = <<"Labrador">>}
    ],
    ?assertEqual(
        {ok, [
            #{<<"type">> => <<"cat">>, <<"name">> => <<"Whiskers">>, <<"indoor">> => true},
            #{<<"type">> => <<"dog">>, <<"name">> => <<"Rex">>, <<"breed">> => <<"Labrador">>}
        ]},
        spectra:encode(json, codec_animal_codec, {type, zoo, 0}, Zoo, [pre_encoded])
    ).

zoo_decode_test() ->
    Json = [
        #{<<"type">> => <<"cat">>, <<"name">> => <<"Whiskers">>, <<"indoor">> => true},
        #{<<"type">> => <<"dog">>, <<"name">> => <<"Rex">>, <<"breed">> => <<"Labrador">>}
    ],
    ?assertEqual(
        {ok, [
            #cat{name = <<"Whiskers">>, indoor = true},
            #dog{name = <<"Rex">>, breed = <<"Labrador">>}
        ]},
        spectra:decode(json, codec_animal_codec, {type, zoo, 0}, Json, [pre_decoded])
    ).

-module(integer_key_map_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").

-compile(nowarn_unused_type).

%% Helper to validate schemas with Python validator
validate_with_python(Schema) ->
    json_schema_validator_helper:validate_or_skip(Schema).

%% Test types with integer keys
-type int_literal_key_map() :: #{1 := binary(), 2 := binary()}.
-type int_literal_optional_map() :: #{1 => binary(), 2 => atom()}.
-type mixed_int_atom_keys() :: #{1 := binary(), foo := integer()}.
-type nested_int_key_map() :: #{outer := #{1 := binary(), 2 := integer()}}.

%%====================================================================
%% JSON Encoding Tests - Integer keys should become strings in JSON
%%====================================================================

encode_int_literal_key_test() ->
    ?assertEqual(
        {ok, #{<<"1">> => <<"value1">>, <<"2">> => <<"value2">>}},
        spectra:encode(
            json,
            ?MODULE,
            {type, int_literal_key_map, 0},
            #{
                1 => <<"value1">>, 2 => <<"value2">>
            },
            [pre_encoded]
        )
    ).

encode_int_literal_key_missing_required_test() ->
    ?assertMatch(
        {error, [#sp_error{location = [1], type = missing_data}]},
        spectra:encode(json, ?MODULE, {type, int_literal_key_map, 0}, #{2 => <<"value2">>}, [
            pre_encoded
        ])
    ),
    ?assertMatch(
        {error, [#sp_error{location = [2], type = missing_data}]},
        spectra:encode(json, ?MODULE, {type, int_literal_key_map, 0}, #{1 => <<"value1">>}, [
            pre_encoded
        ])
    ).

encode_int_literal_key_wrong_value_test() ->
    ?assertMatch(
        {error, [#sp_error{location = [1], type = type_mismatch}]},
        spectra:encode(
            json,
            ?MODULE,
            {type, int_literal_key_map, 0},
            #{
                1 => not_a_binary, 2 => <<"value2">>
            },
            [pre_encoded]
        )
    ).

encode_int_literal_optional_map_test() ->
    ?assertEqual(
        {ok, #{<<"1">> => <<"value1">>}},
        spectra:encode(json, ?MODULE, {type, int_literal_optional_map, 0}, #{1 => <<"value1">>}, [
            pre_encoded
        ])
    ),
    ?assertEqual(
        {ok, #{<<"2">> => some_atom}},
        spectra:encode(json, ?MODULE, {type, int_literal_optional_map, 0}, #{2 => some_atom}, [
            pre_encoded
        ])
    ),
    ?assertEqual(
        {ok, #{<<"1">> => <<"value1">>, <<"2">> => other_atom}},
        spectra:encode(
            json,
            ?MODULE,
            {type, int_literal_optional_map, 0},
            #{
                1 => <<"value1">>, 2 => other_atom
            },
            [pre_encoded]
        )
    ).

encode_mixed_int_atom_keys_test() ->
    ?assertEqual(
        {ok, #{<<"1">> => <<"value1">>, <<"foo">> => 42}},
        spectra:encode(
            json,
            ?MODULE,
            {type, mixed_int_atom_keys, 0},
            #{
                1 => <<"value1">>, foo => 42
            },
            [pre_encoded]
        )
    ).

encode_nested_int_key_map_test() ->
    ?assertEqual(
        {ok, #{
            <<"outer">> =>
                #{
                    <<"1">> => <<"inner_value">>,
                    <<"2">> => 100
                }
        }},
        spectra:encode(
            json,
            ?MODULE,
            {type, nested_int_key_map, 0},
            #{
                outer => #{1 => <<"inner_value">>, 2 => 100}
            },
            [pre_encoded]
        )
    ).

%%====================================================================
%% JSON Decoding Tests - String keys should become integers
%%====================================================================

decode_int_literal_key_test() ->
    ?assertEqual(
        {ok, #{1 => <<"value1">>, 2 => <<"value2">>}},
        spectra:decode(
            json,
            ?MODULE,
            {type, int_literal_key_map, 0},
            #{
                <<"1">> => <<"value1">>, <<"2">> => <<"value2">>
            },
            [pre_decoded]
        )
    ).

decode_int_literal_key_missing_required_test() ->
    ?assertMatch(
        {error, [#sp_error{location = [1], type = missing_data}]},
        spectra:decode(json, ?MODULE, {type, int_literal_key_map, 0}, #{<<"2">> => <<"value2">>}, [
            pre_decoded
        ])
    ),
    ?assertMatch(
        {error, [#sp_error{location = [2], type = missing_data}]},
        spectra:decode(json, ?MODULE, {type, int_literal_key_map, 0}, #{<<"1">> => <<"value1">>}, [
            pre_decoded
        ])
    ).

decode_int_literal_key_wrong_value_test() ->
    ?assertMatch(
        {error, [#sp_error{location = [1], type = type_mismatch}]},
        spectra:decode(
            json,
            ?MODULE,
            {type, int_literal_key_map, 0},
            #{
                <<"1">> => 123, <<"2">> => <<"value2">>
            },
            [pre_decoded]
        )
    ).

decode_int_literal_key_wrong_key_test() ->
    ?assertMatch(
        {error, [#sp_error{location = [1], type = missing_data}]},
        spectra:decode(
            json,
            ?MODULE,
            {type, int_literal_key_map, 0},
            #{
                <<"3">> => <<"value">>, <<"2">> => <<"value2">>
            },
            [pre_decoded]
        )
    ).

decode_int_literal_optional_map_test() ->
    ?assertEqual(
        {ok, #{1 => <<"value1">>}},
        spectra:decode(
            json,
            ?MODULE,
            {type, int_literal_optional_map, 0},
            #{
                <<"1">> => <<"value1">>
            },
            [pre_decoded]
        )
    ),
    ?assertEqual(
        {ok, #{2 => some_atom}},
        spectra:decode(
            json,
            ?MODULE,
            {type, int_literal_optional_map, 0},
            #{
                <<"2">> => <<"some_atom">>
            },
            [pre_decoded]
        )
    ),
    ?assertEqual(
        {ok, #{1 => <<"value1">>, 2 => other_atom}},
        spectra:decode(
            json,
            ?MODULE,
            {type, int_literal_optional_map, 0},
            #{
                <<"1">> => <<"value1">>, <<"2">> => <<"other_atom">>
            },
            [pre_decoded]
        )
    ).

decode_mixed_int_atom_keys_test() ->
    ?assertEqual(
        {ok, #{1 => <<"value1">>, foo => 42}},
        spectra:decode(
            json,
            ?MODULE,
            {type, mixed_int_atom_keys, 0},
            #{
                <<"1">> => <<"value1">>, <<"foo">> => 42
            },
            [pre_decoded]
        )
    ).

decode_nested_int_key_map_test() ->
    ?assertEqual(
        {ok, #{outer => #{1 => <<"inner_value">>, 2 => 100}}},
        spectra:decode(
            json,
            ?MODULE,
            {type, nested_int_key_map, 0},
            #{
                <<"outer">> =>
                    #{
                        <<"1">> => <<"inner_value">>,
                        <<"2">> => 100
                    }
            },
            [pre_decoded]
        )
    ).

%%====================================================================
%% Round-trip Tests
%%====================================================================

round_trip_int_literal_key_test() ->
    Data = #{1 => <<"value1">>, 2 => <<"value2">>},
    {ok, Json} = spectra:encode(json, ?MODULE, {type, int_literal_key_map, 0}, Data, [pre_encoded]),
    {ok, Decoded} = spectra:decode(json, ?MODULE, {type, int_literal_key_map, 0}, Json, [
        pre_decoded
    ]),
    ?assertEqual(Data, Decoded).

round_trip_mixed_int_atom_keys_test() ->
    Data = #{1 => <<"value1">>, foo => 42},
    {ok, Json} = spectra:encode(json, ?MODULE, {type, mixed_int_atom_keys, 0}, Data, [pre_encoded]),
    {ok, Decoded} = spectra:decode(json, ?MODULE, {type, mixed_int_atom_keys, 0}, Json, [
        pre_decoded
    ]),
    ?assertEqual(Data, Decoded).

round_trip_nested_int_key_map_test() ->
    Data = #{outer => #{1 => <<"inner_value">>, 2 => 100}},
    {ok, Json} = spectra:encode(json, ?MODULE, {type, nested_int_key_map, 0}, Data, [pre_encoded]),
    {ok, Decoded} = spectra:decode(json, ?MODULE, {type, nested_int_key_map, 0}, Json, [pre_decoded]),
    ?assertEqual(Data, Decoded).

%%====================================================================
%% JSON Schema Tests - Integer keys should appear as strings
%%====================================================================

schema_int_literal_key_test() ->
    Schema = spectra:schema(json_schema, ?MODULE, {type, int_literal_key_map, 0}, [pre_encoded]),
    ?assertMatch(
        #{
            type := <<"object">>,
            properties := #{
                <<"1">> := #{type := <<"string">>},
                <<"2">> := #{type := <<"string">>}
            },
            required := _
        },
        Schema
    ),
    #{required := Required} = Schema,
    ?assertEqual([<<"1">>, <<"2">>], lists:sort(Required)),
    validate_with_python(Schema).

schema_int_literal_optional_map_test() ->
    Schema = spectra:schema(
        json_schema,
        ?MODULE,
        {
            type,
            int_literal_optional_map,
            0
        },
        [pre_encoded]
    ),
    ?assertMatch(
        #{
            type := <<"object">>,
            properties := #{<<"1">> := _, <<"2">> := _}
        },
        Schema
    ),
    ?assertEqual(false, maps:is_key(required, Schema)),
    validate_with_python(Schema).

schema_mixed_int_atom_keys_test() ->
    Schema = spectra:schema(json_schema, ?MODULE, {type, mixed_int_atom_keys, 0}, [pre_encoded]),
    ?assertMatch(
        #{
            type := <<"object">>,
            properties := #{
                <<"1">> := #{type := <<"string">>},
                <<"foo">> := #{type := <<"integer">>}
            },
            required := _
        },
        Schema
    ),
    #{required := Required} = Schema,
    ?assertEqual([<<"1">>, <<"foo">>], lists:sort(Required)),
    validate_with_python(Schema).

schema_nested_int_key_map_test() ->
    Schema = spectra:schema(json_schema, ?MODULE, {type, nested_int_key_map, 0}, [pre_encoded]),
    ?assertMatch(
        #{
            type := <<"object">>,
            properties := #{<<"outer">> := _},
            required := [<<"outer">>]
        },
        Schema
    ),
    #{properties := #{<<"outer">> := OuterSchema}} = Schema,
    ?assertMatch(
        #{
            type := <<"object">>,
            properties := #{<<"1">> := _, <<"2">> := _}
        },
        OuterSchema
    ),
    validate_with_python(Schema).

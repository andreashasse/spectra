-module(nil_fields_test).

-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_unused_type).

-record(contact, {name :: binary(), phone :: binary() | undefined}).

-type contact() :: #contact{}.
-type maybe_phone() :: binary() | undefined.
-type profile() :: #{
    name := binary(),
    nickname := binary() | undefined,
    bio => binary() | undefined
}.
-type elixir_profile() :: #{name := binary(), nickname := binary() | nil}.
-type aliased_profile() :: #{phone := maybe_phone()}.
-type mixed_profile() :: #{
    score := integer() | binary() | undefined,
    status := active | inactive | undefined,
    tags := [binary()] | undefined
}.
-type labels() :: #{binary() => binary() | undefined}.
-type team() :: #{
    lead := contact(),
    members := [profile()],
    status := active | inactive | undefined,
    backup => contact() | undefined,
    labels := labels()
}.

with_nil_fields(Value, Tests) ->
    {setup,
        fun() ->
            Previous = application:get_env(spectra, nil_fields),
            ok = set_nil_fields(Value),
            Previous
        end,
        fun(Previous) -> ok = restore_nil_fields(Previous) end, Tests}.

set_nil_fields(unset) -> application:unset_env(spectra, nil_fields);
set_nil_fields(Value) -> application:set_env(spectra, nil_fields, Value).

restore_nil_fields(undefined) -> application:unset_env(spectra, nil_fields);
restore_nil_fields({ok, Value}) -> application:set_env(spectra, nil_fields, Value).

encode(Type, Data) ->
    {ok, Json} = spectra:encode(json, ?MODULE, Type, Data),
    json:decode(iolist_to_binary(Json)).

schema(Type) ->
    spectra:schema(json_schema, ?MODULE, Type, [pre_encoded]).

jesse_validate(Schema, Json) ->
    JesseSchema = json:decode(iolist_to_binary(json:encode(maps:remove('$schema', Schema)))),
    jesse:validate_with_schema(JesseSchema, Json).

encode(Type, Data, Options) ->
    {ok, Json} = spectra:encode(json, ?MODULE, Type, Data, Options),
    json:decode(iolist_to_binary(Json)).

assert_json_schema(Expected, Type) ->
    ?assertEqual(
        json:decode(Expected),
        json:decode(iolist_to_binary(spectra:schema(json_schema, ?MODULE, Type)))
    ).

team_data() ->
    #{
        lead => #contact{name = <<"Ann">>, phone = undefined},
        members => [#{name => <<"Bo">>, nickname => undefined, bio => undefined}],
        status => undefined,
        backup => undefined,
        labels => #{<<"a">> => undefined, <<"b">> => <<"x">>}
    }.

default_test_() ->
    [
        with_nil_fields(unset, [
            fun default_schema_allows_null_for_optional_fields/0,
            fun default_encoding_omits_missing_values/0,
            fun default_per_call_null_writes_null/0,
            fun default_per_call_null_writes_null_in_pre_encoded_output/0,
            fun default_per_call_null_validates_against_schema/0,
            fun default_per_call_omit_omits_missing_values/0,
            fun default_openapi_spec_validates/0
        ]),
        with_nil_fields(omit_or_null, [
            fun default_schema_allows_null_for_optional_fields/0,
            fun default_encoding_omits_missing_values/0,
            fun default_per_call_null_writes_null/0
        ])
    ].

default_schema_allows_null_for_optional_fields() ->
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"object">>,
            properties => #{
                <<"name">> => #{type => <<"string">>},
                <<"phone">> => #{type => [<<"string">>, <<"null">>]}
            },
            required => [<<"name">>]
        },
        schema(contact)
    ),
    ?assertMatch(
        #{
            properties := #{
                <<"name">> := #{type := <<"string">>},
                <<"nickname">> := #{type := [<<"string">>, <<"null">>]},
                <<"bio">> := #{type := [<<"string">>, <<"null">>]}
            },
            required := [<<"name">>]
        },
        schema(profile)
    ),
    ?assertMatch(
        #{
            properties := #{
                <<"score">> := #{
                    anyOf := [
                        #{type := <<"integer">>}, #{type := <<"string">>}, #{type := <<"null">>}
                    ]
                },
                <<"status">> := #{
                    anyOf := [#{enum := [<<"active">>, <<"inactive">>]}, #{type := <<"null">>}]
                },
                <<"tags">> := #{type := [<<"array">>, <<"null">>]}
            }
        },
        schema(mixed_profile)
    ),
    ?assertNot(maps:is_key(required, schema(mixed_profile))),
    ?assertMatch(
        #{additionalProperties := #{type := [<<"string">>, <<"null">>]}}, schema(labels)
    ),
    lists:foreach(
        fun(Type) -> json_schema_validator_helper:validate_or_skip(schema(Type)) end,
        [contact, profile, mixed_profile, labels, team]
    ).

default_encoding_omits_missing_values() ->
    ?assertEqual(
        #{<<"name">> => <<"Ann">>},
        encode(contact, #contact{name = <<"Ann">>, phone = undefined})
    ),
    ?assertEqual(
        #{<<"name">> => <<"Ann">>},
        encode(profile, #{name => <<"Ann">>, nickname => undefined, bio => undefined})
    ),
    ?assertEqual(#{}, encode(labels, #{<<"a">> => undefined})),
    ?assertEqual(
        #{
            <<"lead">> => #{<<"name">> => <<"Ann">>},
            <<"members">> => [#{<<"name">> => <<"Bo">>}],
            <<"labels">> => #{<<"b">> => <<"x">>}
        },
        encode(team, team_data())
    ).

default_per_call_null_writes_null() ->
    ?assertEqual(
        #{<<"name">> => <<"Ann">>, <<"phone">> => null},
        encode(contact, #contact{name = <<"Ann">>, phone = undefined}, [{nil_fields, null}])
    ),
    ?assertEqual(
        #{<<"name">> => <<"Ann">>, <<"nickname">> => null},
        encode(profile, #{name => <<"Ann">>}, [{nil_fields, null}])
    ),
    ?assertEqual(
        #{
            <<"lead">> => #{<<"name">> => <<"Ann">>, <<"phone">> => null},
            <<"members">> => [
                #{<<"name">> => <<"Bo">>, <<"nickname">> => null, <<"bio">> => null}
            ],
            <<"status">> => null,
            <<"backup">> => null,
            <<"labels">> => #{<<"a">> => null, <<"b">> => <<"x">>}
        },
        encode(team, team_data(), [{nil_fields, null}])
    ).

default_per_call_null_writes_null_in_pre_encoded_output() ->
    ?assertEqual(
        {ok, #{<<"name">> => <<"Ann">>, <<"phone">> => null}},
        spectra:encode(
            json,
            ?MODULE,
            contact,
            #contact{name = <<"Ann">>, phone = undefined},
            [pre_encoded, {nil_fields, null}]
        )
    ).

default_per_call_null_validates_against_schema() ->
    lists:foreach(
        fun({Type, Data}) ->
            Schema = schema(Type),
            ?assertMatch({ok, _}, jesse_validate(Schema, encode(Type, Data))),
            ?assertMatch({ok, _}, jesse_validate(Schema, encode(Type, Data, [{nil_fields, null}])))
        end,
        [
            {contact, #contact{name = <<"Ann">>, phone = undefined}},
            {profile, #{name => <<"Ann">>, nickname => undefined, bio => undefined}},
            {mixed_profile, #{score => undefined, status => undefined, tags => undefined}},
            {mixed_profile, #{score => 3, status => active, tags => [<<"t">>]}},
            {labels, #{<<"a">> => undefined, <<"b">> => <<"x">>}},
            {team, team_data()}
        ]
    ).

default_per_call_omit_omits_missing_values() ->
    ?assertEqual(
        #{<<"name">> => <<"Ann">>},
        encode(contact, #contact{name = <<"Ann">>, phone = undefined}, [{nil_fields, omit}])
    ).

default_openapi_spec_validates() ->
    Spec = contacts_openapi_spec(),
    ?assertMatch(
        #{
            <<"components">> := #{
                <<"schemas">> := #{
                    <<"Contact">> := #{
                        properties := #{<<"phone">> := #{type := [<<"string">>, <<"null">>]}},
                        required := [<<"name">>]
                    }
                }
            }
        },
        Spec
    ),
    validate_openapi(Spec).

omit_test_() ->
    with_nil_fields(omit, [
        fun omit_schema_is_unchanged/0,
        fun omit_encoding_omits_missing_values/0,
        fun omit_per_call_null_raises_badarg/0
    ]).

omit_schema_is_unchanged() ->
    assert_json_schema(
        <<
            "{\"type\":\"object\",\"$schema\":\"https://json-schema.org/draft/2020-12/schema\","
            "\"required\":[\"name\"],\"properties\":{\"name\":{\"type\":\"string\"},"
            "\"phone\":{\"type\":\"string\"}}}"
        >>,
        contact
    ),
    assert_json_schema(
        <<
            "{\"type\":\"object\",\"$schema\":\"https://json-schema.org/draft/2020-12/schema\","
            "\"additionalProperties\":false,\"required\":[\"name\"],\"properties\":{"
            "\"bio\":{\"type\":\"string\"},\"name\":{\"type\":\"string\"},"
            "\"nickname\":{\"type\":\"string\"}}}"
        >>,
        profile
    ),
    assert_json_schema(
        <<
            "{\"type\":\"object\",\"$schema\":\"https://json-schema.org/draft/2020-12/schema\","
            "\"additionalProperties\":{\"type\":\"string\"}}"
        >>,
        labels
    ),
    assert_json_schema(
        <<
            "{\"type\":\"object\",\"$schema\":\"https://json-schema.org/draft/2020-12/schema\","
            "\"additionalProperties\":false,\"properties\":{"
            "\"score\":{\"anyOf\":[{\"type\":\"integer\"},{\"type\":\"string\"}]},"
            "\"status\":{\"type\":\"string\",\"enum\":[\"active\",\"inactive\"]},"
            "\"tags\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}}}}"
        >>,
        mixed_profile
    ),
    assert_json_schema(
        <<
            "{\"type\":\"object\",\"$schema\":\"https://json-schema.org/draft/2020-12/schema\","
            "\"additionalProperties\":false,\"required\":[\"labels\",\"members\",\"lead\"],"
            "\"properties\":{\"backup\":{\"type\":\"object\",\"required\":[\"name\"],"
            "\"properties\":{\"name\":{\"type\":\"string\"},\"phone\":{\"type\":\"string\"}}},"
            "\"labels\":{\"type\":\"object\",\"additionalProperties\":{\"type\":\"string\"}},"
            "\"lead\":{\"type\":\"object\",\"required\":[\"name\"],"
            "\"properties\":{\"name\":{\"type\":\"string\"},\"phone\":{\"type\":\"string\"}}},"
            "\"members\":{\"type\":\"array\",\"items\":{\"type\":\"object\","
            "\"additionalProperties\":false,\"required\":[\"name\"],\"properties\":{"
            "\"bio\":{\"type\":\"string\"},\"name\":{\"type\":\"string\"},"
            "\"nickname\":{\"type\":\"string\"}}}},"
            "\"status\":{\"type\":\"string\",\"enum\":[\"active\",\"inactive\"]}}}"
        >>,
        team
    ).

omit_encoding_omits_missing_values() ->
    ?assertEqual(
        #{<<"name">> => <<"Ann">>},
        encode(contact, #contact{name = <<"Ann">>, phone = undefined})
    ),
    ?assertEqual(#{}, encode(labels, #{<<"a">> => undefined})),
    ?assertEqual(
        #{<<"name">> => <<"Ann">>},
        encode(contact, #contact{name = <<"Ann">>, phone = undefined}, [{nil_fields, omit}])
    ).

omit_per_call_null_raises_badarg() ->
    Data = #contact{name = <<"Ann">>, phone = undefined},
    try spectra:encode(json, ?MODULE, contact, Data, [{nil_fields, null}]) of
        Result -> erlang:error({expected_badarg, Result})
    catch
        error:badarg:Stacktrace ->
            ?assertMatch(
                #{5 := Message} when is_binary(Message),
                spectra:format_error(badarg, Stacktrace)
            ),
            #{5 := Message} = spectra:format_error(badarg, Stacktrace),
            ?assertNotEqual(nomatch, binary:match(Message, <<"nil_fields is omit">>))
    end.

null_per_call_omit_test_() ->
    with_nil_fields(null, fun() ->
        ?assertEqual(
            #{<<"name">> => <<"Ann">>},
            encode(contact, #contact{name = <<"Ann">>, phone = undefined}, [{nil_fields, omit}])
        )
    end).

invalid_option_test_() ->
    with_nil_fields(unset, fun() ->
        Data = #contact{name = <<"Ann">>},
        lists:foreach(
            fun(Options) ->
                try spectra:encode(json, ?MODULE, contact, Data, Options) of
                    Result -> erlang:error({expected_badarg, Options, Result})
                catch
                    error:badarg:Stacktrace ->
                        ?assertMatch(#{5 := _}, spectra:format_error(badarg, Stacktrace))
                end
            end,
            [[{nil_fields, nothing}], [{nil_fields, omit_or_null}], [nil_fields]]
        )
    end).

contacts_openapi_spec() ->
    Response = spectra_openapi:response_with_body(
        spectra_openapi:response(200, <<"A profile">>), ?MODULE, {type, mixed_profile, 0}
    ),
    Endpoint = spectra_openapi:add_response(
        spectra_openapi:with_request_body(
            spectra_openapi:endpoint(post, <<"/contacts">>), ?MODULE, {record, contact}
        ),
        Response
    ),
    {ok, Spec} = spectra_openapi:endpoints_to_openapi(
        #{title => <<"Contacts">>, version => <<"1.0.0">>}, [Endpoint], [pre_encoded]
    ),
    Spec.

validate_openapi(Spec) ->
    case openapi_validator_helper:validate_openapi_3_1(Spec) of
        ok -> ok;
        {skip, Reason} -> io:format("Skipping OpenAPI validation: ~ts~n", [Reason]);
        {error, {validation_failed, Output}} -> erlang:error({openapi_validation_failed, Output})
    end.

null_test_() ->
    with_nil_fields(null, [
        fun null_record_field_is_encoded_as_null/0,
        fun null_record_field_value_is_encoded_as_before/0,
        fun null_record_schema_requires_nullable_field/0,
        fun null_exact_map_field_is_encoded_as_null/0,
        fun null_absent_exact_map_field_is_encoded_as_null/0,
        fun null_absent_optional_map_field_is_omitted/0,
        fun null_map_schema_requires_exact_nullable_fields_only/0,
        fun null_elixir_nil_field_is_encoded_as_null/0,
        fun null_aliased_nullable_type/0,
        fun null_non_simple_schemas_use_any_of/0,
        fun null_typed_map_value_is_encoded_as_null/0,
        fun null_is_decoded_as_missing_value/0,
        fun null_encoded_data_validates_against_schema/0,
        fun null_openapi_spec_validates/0
    ]).

null_record_field_is_encoded_as_null() ->
    ?assertEqual(
        #{<<"name">> => <<"Ann">>, <<"phone">> => null},
        encode(contact, #contact{name = <<"Ann">>, phone = undefined})
    ).

null_record_field_value_is_encoded_as_before() ->
    ?assertEqual(
        #{<<"name">> => <<"Ann">>, <<"phone">> => <<"123">>},
        encode(contact, #contact{name = <<"Ann">>, phone = <<"123">>})
    ).

null_record_schema_requires_nullable_field() ->
    Schema = schema(contact),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"object">>,
            properties => #{
                <<"name">> => #{type => <<"string">>},
                <<"phone">> => #{type => [<<"string">>, <<"null">>]}
            },
            required => [<<"name">>, <<"phone">>]
        },
        Schema
    ),
    json_schema_validator_helper:validate_or_skip(Schema).

null_exact_map_field_is_encoded_as_null() ->
    ?assertEqual(
        #{<<"name">> => <<"Ann">>, <<"nickname">> => null, <<"bio">> => null},
        encode(profile, #{name => <<"Ann">>, nickname => undefined, bio => undefined})
    ).

null_absent_exact_map_field_is_encoded_as_null() ->
    ?assertEqual(
        #{<<"name">> => <<"Ann">>, <<"nickname">> => null},
        encode(profile, #{name => <<"Ann">>})
    ).

null_absent_optional_map_field_is_omitted() ->
    ?assertEqual(
        #{<<"name">> => <<"Ann">>, <<"nickname">> => <<"A">>},
        encode(profile, #{name => <<"Ann">>, nickname => <<"A">>})
    ).

null_map_schema_requires_exact_nullable_fields_only() ->
    Schema = schema(profile),
    ?assertMatch(
        #{
            properties := #{
                <<"name">> := #{type := <<"string">>},
                <<"nickname">> := #{type := [<<"string">>, <<"null">>]},
                <<"bio">> := #{type := [<<"string">>, <<"null">>]}
            }
        },
        Schema
    ),
    ?assertEqual(
        [<<"name">>, <<"nickname">>],
        lists:sort(maps:get(required, Schema))
    ),
    json_schema_validator_helper:validate_or_skip(Schema).

null_elixir_nil_field_is_encoded_as_null() ->
    ?assertEqual(
        #{<<"name">> => <<"Ann">>, <<"nickname">> => null},
        encode(elixir_profile, #{name => <<"Ann">>, nickname => nil})
    ),
    ?assertMatch(
        #{
            properties := #{<<"nickname">> := #{type := [<<"string">>, <<"null">>]}},
            required := [_, _]
        },
        schema(elixir_profile)
    ).

null_aliased_nullable_type() ->
    ?assertEqual(#{<<"phone">> => null}, encode(aliased_profile, #{phone => undefined})),
    ?assertMatch(
        #{
            properties := #{<<"phone">> := #{type := [<<"string">>, <<"null">>]}},
            required := [<<"phone">>]
        },
        schema(aliased_profile)
    ).

null_non_simple_schemas_use_any_of() ->
    Schema = schema(mixed_profile),
    ?assertMatch(
        #{
            properties := #{
                <<"score">> := #{
                    anyOf := [
                        #{type := <<"integer">>}, #{type := <<"string">>}, #{type := <<"null">>}
                    ]
                },
                <<"status">> := #{
                    anyOf := [#{enum := [<<"active">>, <<"inactive">>]}, #{type := <<"null">>}]
                },
                <<"tags">> := #{
                    type := [<<"array">>, <<"null">>], items := #{type := <<"string">>}
                }
            }
        },
        Schema
    ),
    ?assertEqual(
        [<<"score">>, <<"status">>, <<"tags">>],
        lists:sort(maps:get(required, Schema))
    ),
    json_schema_validator_helper:validate_or_skip(Schema).

null_typed_map_value_is_encoded_as_null() ->
    ?assertEqual(
        #{<<"a">> => null, <<"b">> => <<"x">>},
        encode(labels, #{<<"a">> => undefined, <<"b">> => <<"x">>})
    ),
    Schema = schema(labels),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"object">>,
            additionalProperties => #{type => [<<"string">>, <<"null">>]}
        },
        Schema
    ),
    json_schema_validator_helper:validate_or_skip(Schema).

null_is_decoded_as_missing_value() ->
    Json = #{<<"name">> => <<"Ann">>, <<"phone">> => null},
    ?assertEqual(
        {ok, #contact{name = <<"Ann">>, phone = undefined}},
        spectra:decode(json, ?MODULE, contact, Json, [pre_decoded])
    ),
    ?assertEqual(
        {ok, #{phone => undefined}},
        spectra:decode(json, ?MODULE, aliased_profile, #{<<"phone">> => null}, [pre_decoded])
    ),
    ?assertEqual(
        {ok, #{<<"a">> => undefined}},
        spectra:decode(json, ?MODULE, labels, #{<<"a">> => null}, [pre_decoded])
    ).

null_encoded_data_validates_against_schema() ->
    lists:foreach(
        fun({Type, Data}) ->
            Json = encode(Type, Data),
            ?assertMatch({ok, _}, jesse_validate(schema(Type), Json))
        end,
        [
            {contact, #contact{name = <<"Ann">>, phone = undefined}},
            {profile, #{name => <<"Ann">>, nickname => undefined, bio => undefined}},
            {mixed_profile, #{score => undefined, status => undefined, tags => undefined}},
            {mixed_profile, #{score => 3, status => active, tags => [<<"t">>]}},
            {labels, #{<<"a">> => undefined, <<"b">> => <<"x">>}}
        ]
    ),
    ?assertMatch(
        {error, _},
        jesse_validate(schema(contact), #{<<"name">> => <<"Ann">>})
    ).

null_openapi_spec_validates() ->
    Spec = contacts_openapi_spec(),
    ?assertMatch(
        #{
            <<"components">> := #{
                <<"schemas">> := #{
                    <<"Contact">> := #{
                        properties := #{<<"phone">> := #{type := [<<"string">>, <<"null">>]}},
                        required := [<<"name">>, <<"phone">>]
                    }
                }
            }
        },
        Spec
    ),
    validate_openapi(Spec).

invalid_setting_test_() ->
    with_nil_fields(
        nothing,
        fun() ->
            ?assertError(
                {invalid_config, nil_fields, nothing},
                spectra:encode(json, ?MODULE, contact, #contact{name = <<"Ann">>})
            )
        end
    ).

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
-spectra(#{description => <<"A count">>}).
-type maybe_count() :: integer() | undefined.
-type maybe_status() :: active | inactive | undefined.
-type maybe_name() :: binary() | nil.
-type maybe_remote_name() :: unicode:unicode_binary() | nil.
-record(tally, {count :: maybe_count(), status :: maybe_status(), counts :: [maybe_count()]}).
-type tally() :: #tally{}.
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
            fun default_per_call_option_with_type_info/0,
            fun default_openapi_spec_validates/0,
            fun nullable_top_level_schema_allows_null/0,
            fun nullable_top_level_value_validates_against_schema/0,
            fun nullable_alias_field_has_single_null/0,
            fun parameter_and_header_schemas_exclude_null/0,
            fun body_allows_null_where_parameter_of_same_type_does_not/0
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

default_per_call_option_with_type_info() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    Data = #contact{name = <<"Ann">>, phone = undefined},
    ?assertEqual(
        {ok, #{<<"name">> => <<"Ann">>, <<"phone">> => null}},
        spectra:encode(json, TypeInfo, contact, Data, [pre_encoded, {nil_fields, null}])
    ),
    ?assertEqual(
        {ok, #{<<"name">> => <<"Ann">>}},
        spectra:encode(json, TypeInfo, contact, Data, [pre_encoded, {nil_fields, omit}])
    ).

nullable_top_level_schema_allows_null() ->
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => [<<"integer">>, <<"null">>],
            description => <<"A count">>
        },
        schema(maybe_count)
    ),
    ?assertMatch(
        #{anyOf := [#{enum := [<<"active">>, <<"inactive">>]}, #{type := <<"null">>}]},
        schema(maybe_status)
    ),
    ?assertMatch(#{type := [<<"string">>, <<"null">>]}, schema(maybe_name)).

nullable_top_level_value_validates_against_schema() ->
    lists:foreach(
        fun({Type, Data}) ->
            Json = encode(Type, Data),
            ?assertMatch({ok, _}, jesse_validate(schema(Type), Json))
        end,
        [
            {maybe_count, undefined},
            {maybe_count, 3},
            {maybe_status, undefined},
            {maybe_status, active},
            {maybe_name, nil},
            {maybe_name, <<"Ann">>}
        ]
    ),
    ?assertEqual(null, encode(maybe_count, undefined)).

nullable_alias_field_has_single_null() ->
    Schema = schema(tally),
    ?assertMatch(
        #{
            properties := #{
                <<"count">> := #{type := [<<"integer">>, <<"null">>], description := <<"A count">>},
                <<"status">> := #{
                    anyOf := [#{enum := [<<"active">>, <<"inactive">>]}, #{type := <<"null">>}]
                },
                <<"counts">> := #{items := #{type := [<<"integer">>, <<"null">>]}}
            }
        },
        Schema
    ),
    ?assertEqual(1, count_nulls(Schema, <<"count">>)),
    ?assertEqual(1, count_nulls(Schema, <<"status">>)),
    json_schema_validator_helper:validate_or_skip(Schema),
    Json = encode(tally, #tally{count = undefined, status = undefined, counts = [1, undefined]}, [
        {nil_fields, null}
    ]),
    ?assertMatch({ok, _}, jesse_validate(Schema, Json)).

count_nulls(#{properties := Properties}, Field) ->
    length(binary:matches(iolist_to_binary(json:encode(maps:get(Field, Properties))), <<"null">>)).

default_openapi_spec_validates() ->
    Spec = contacts_openapi_spec(),
    ?assertMatch(
        #{
            <<"components">> := #{
                <<"schemas">> := #{
                    <<"Contact">> := #{
                        properties := #{<<"phone">> := #{type := [<<"string">>, <<"null">>]}},
                        required := [<<"name">>]
                    },
                    <<"MaybeCount0">> := #{type := [<<"integer">>, <<"null">>]},
                    <<"Tally">> := #{
                        properties := #{<<"count">> := #{type := [<<"integer">>, <<"null">>]}}
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
        fun omit_per_call_null_raises_badarg/0,
        fun omit_top_level_schema_is_unchanged/0,
        fun parameter_and_header_schemas_exclude_null/0
    ]).

omit_top_level_schema_is_unchanged() ->
    assert_json_schema(
        <<
            "{\"type\":\"integer\",\"description\":\"A count\","
            "\"$schema\":\"https://json-schema.org/draft/2020-12/schema\"}"
        >>,
        maybe_count
    ),
    assert_json_schema(
        <<
            "{\"type\":\"string\",\"enum\":[\"active\",\"inactive\"],"
            "\"$schema\":\"https://json-schema.org/draft/2020-12/schema\"}"
        >>,
        maybe_status
    ),
    assert_json_schema(
        <<
            "{\"type\":\"object\",\"$schema\":\"https://json-schema.org/draft/2020-12/schema\","
            "\"required\":[\"counts\"],\"properties\":{"
            "\"count\":{\"type\":\"integer\",\"description\":\"A count\"},"
            "\"status\":{\"type\":\"string\",\"enum\":[\"active\",\"inactive\"]},"
            "\"counts\":{\"type\":\"array\",\"items\":{\"type\":[\"integer\",\"null\"],"
            "\"description\":\"A count\"}}}}"
        >>,
        tally
    ).

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
    assert_nil_fields_badarg([{nil_fields, null}], <<"the nil_fields setting is omit">>).

assert_nil_fields_badarg(Options, ExpectedMessage) ->
    Data = #contact{name = <<"Ann">>, phone = <<"secret">>},
    try spectra:encode(json, ?MODULE, contact, Data, Options) of
        Result -> erlang:error({expected_badarg, Options, Result})
    catch
        error:badarg:Stacktrace ->
            [TopFrame | _] = Stacktrace,
            ?assertMatch({spectra, encode, 5, _}, TopFrame),
            ?assertNot(contains(TopFrame, Data)),
            #{5 := Message} = spectra:format_error(badarg, Stacktrace),
            ?assertNotEqual(nomatch, string:find(Message, ExpectedMessage)),
            Formatted = unicode:characters_to_binary(
                erl_error:format_exception(error, badarg, Stacktrace)
            ),
            ?assertNotEqual(nomatch, string:find(Formatted, ExpectedMessage))
    end.

contains(Term, Term) ->
    true;
contains(Term, Part) when is_tuple(Term) ->
    contains(tuple_to_list(Term), Part);
contains([Head | Tail], Part) ->
    contains(Head, Part) orelse contains(Tail, Part);
contains(Term, Part) when is_map(Term) ->
    contains(maps:to_list(Term), Part);
contains(_Term, _Part) ->
    false.

null_per_call_test_() ->
    with_nil_fields(null, [
        fun() ->
            assert_nil_fields_badarg([{nil_fields, omit}], <<"the nil_fields setting is null">>)
        end,
        fun() ->
            ?assertEqual(
                #{<<"name">> => <<"Ann">>, <<"phone">> => null},
                encode(contact, #contact{name = <<"Ann">>, phone = undefined}, [{nil_fields, null}])
            )
        end
    ]).

invalid_option_test_() ->
    with_nil_fields(unset, fun() ->
        lists:foreach(
            fun(Options) ->
                assert_nil_fields_badarg(Options, <<"must be null or omit">>)
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
    CountEndpoint = spectra_openapi:add_response(
        spectra_openapi:with_request_body(
            spectra_openapi:endpoint(post, <<"/counts">>), ?MODULE, {type, maybe_count, 0}
        ),
        spectra_openapi:response_with_body(
            spectra_openapi:response(200, <<"A tally">>), ?MODULE, {record, tally}
        )
    ),
    {ok, Spec} = spectra_openapi:endpoints_to_openapi(
        #{title => <<"Contacts">>, version => <<"1.0.0">>}, [Endpoint, CountEndpoint], [pre_encoded]
    ),
    Spec.

%% A parameter or header value is never JSON null: absence is expressed with
%% `required: false`, so its schema is the schema of the non-missing type.
parameter_and_header_schemas_exclude_null() ->
    Spec = parameters_openapi_spec({record, contact}),
    String = #{type => <<"string">>},
    #{
        <<"paths">> := #{
            <<"/phones/{id}">> := #{
                <<"post">> := #{
                    <<"parameters">> := Parameters,
                    <<"responses">> := #{<<"204">> := #{<<"headers">> := Headers}}
                }
            }
        },
        <<"components">> := #{<<"schemas">> := Components}
    } = Spec,
    ?assertEqual(
        [String, String, String, String],
        [Schema || #{<<"schema">> := Schema} <- Parameters]
    ),
    ?assertEqual(
        #{<<"X-Phone">> => #{<<"schema">> => String}, <<"X-Name">> => #{<<"schema">> => String}},
        Headers
    ),
    ?assertMatch(
        #{<<"MaybePhone0">> := String, <<"MaybeRemoteName0">> := String},
        Components
    ),
    validate_openapi(Spec).

body_allows_null_where_parameter_of_same_type_does_not() ->
    Nullable = #{type => [<<"string">>, <<"null">>]},
    FieldSpec = parameters_openapi_spec({type, aliased_profile, 0}),
    ?assertMatch(
        #{
            <<"components">> := #{
                <<"schemas">> := #{
                    <<"AliasedProfile0">> := #{properties := #{<<"phone">> := Nullable}}
                }
            }
        },
        FieldSpec
    ),
    BodySpec = parameters_openapi_spec({type, maybe_phone, 0}),
    ?assertMatch(
        #{<<"components">> := #{<<"schemas">> := #{<<"MaybePhone0">> := Nullable}}},
        BodySpec
    ),
    lists:foreach(
        fun(Spec) ->
            #{
                <<"paths">> := #{
                    <<"/phones/{id}">> := #{<<"post">> := #{<<"parameters">> := Parameters}}
                }
            } = Spec,
            ?assertEqual(
                [#{type => <<"string">>}],
                lists:usort([Schema || #{<<"schema">> := Schema} <- Parameters])
            ),
            validate_openapi(Spec)
        end,
        [FieldSpec, BodySpec]
    ).

parameters_openapi_spec(BodyType) ->
    Parameters = [
        #{name => <<"x-phone">>, in => header, schema => {type, maybe_phone, 0}},
        #{name => <<"phone">>, in => query, schema => {type, maybe_phone, 0}},
        #{name => <<"id">>, in => path, required => true, schema => {type, maybe_phone, 0}},
        #{name => <<"name">>, in => query, schema => {type, maybe_remote_name, 0}}
    ],
    Response = lists:foldl(
        fun({Header, Type}, Acc) ->
            spectra_openapi:response_with_header(Acc, Header, ?MODULE, #{schema => {type, Type, 0}})
        end,
        spectra_openapi:response(204, <<"Stored">>),
        [{<<"X-Phone">>, maybe_phone}, {<<"X-Name">>, maybe_remote_name}]
    ),
    Endpoint = lists:foldl(
        fun(Parameter, Acc) -> spectra_openapi:with_parameter(Acc, ?MODULE, Parameter) end,
        spectra_openapi:with_request_body(
            spectra_openapi:endpoint(post, <<"/phones/{id}">>), ?MODULE, BodyType
        ),
        Parameters
    ),
    {ok, Spec} = spectra_openapi:endpoints_to_openapi(
        #{title => <<"Phones">>, version => <<"1.0.0">>},
        [spectra_openapi:add_response(Endpoint, Response)],
        [pre_encoded]
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
        fun null_openapi_spec_validates/0,
        fun nullable_top_level_schema_allows_null/0,
        fun nullable_top_level_value_validates_against_schema/0,
        fun nullable_alias_field_has_single_null/0,
        fun parameter_and_header_schemas_exclude_null/0,
        fun body_allows_null_where_parameter_of_same_type_does_not/0
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
                    },
                    <<"MaybeCount0">> := #{type := [<<"integer">>, <<"null">>]}
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

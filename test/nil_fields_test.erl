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

omit_test_() ->
    [
        with_nil_fields(unset, fun omit_behaviour/0),
        with_nil_fields(omit, fun omit_behaviour/0)
    ].

omit_behaviour() ->
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
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"object">>,
            properties => #{
                <<"name">> => #{type => <<"string">>},
                <<"phone">> => #{type => <<"string">>}
            },
            required => [<<"name">>]
        },
        schema(contact)
    ),
    ?assertMatch(
        #{
            properties := #{
                <<"nickname">> := #{type := <<"string">>},
                <<"bio">> := #{type := <<"string">>}
            },
            required := [<<"name">>]
        },
        schema(profile)
    ).

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
            {mixed_profile, #{score => 3, status => active, tags => [<<"t">>]}}
        ]
    ),
    ?assertMatch(
        {error, _},
        jesse_validate(schema(contact), #{<<"name">> => <<"Ann">>})
    ).

null_openapi_spec_validates() ->
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
    case openapi_validator_helper:validate_openapi_3_1(Spec) of
        ok -> ok;
        {skip, Reason} -> io:format("Skipping OpenAPI validation: ~ts~n", [Reason]);
        {error, {validation_failed, Output}} -> erlang:error({openapi_validation_failed, Output})
    end.

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

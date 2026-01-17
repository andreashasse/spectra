-module(spectra_json_schema_remote_type_test).

-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_unused_type).

%% Helper to validate schemas with Python validator
validate_with_python(Schema) ->
    json_schema_validator_helper:validate_or_skip(Schema).

-type user_with_account() :: #{
    name := string(),
    account := other:account()
}.

-type user_with_result() :: #{
    name := string(),
    result := external_type:result_t(string())
}.

-type organization() :: #{
    primary_account := other:account(),
    backup_account => other:account(),
    status := external_type:result_t(atom())
}.

-record(transaction, {
    id :: integer(),
    account :: other:account(),
    result :: external_type:result_t(binary())
}).

-type transaction() :: #transaction{}.

-record(optional_account_holder, {
    id :: integer(),
    name :: string(),
    account :: other:account() | undefined
}).

-type optional_account_holder() :: #optional_account_holder{}.

-type account_list() :: [other:account()].

-type account_or_string() :: other:account() | string().

simple_remote_type_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {type, user_with_account, 0}),
    ?assertMatch(
        #{
            type := <<"object">>,
            properties := #{
                <<"name">> := _,
                <<"account">> := #{
                    type := <<"object">>,
                    properties := #{
                        <<"id">> := #{type := <<"string">>},
                        <<"balance">> := #{type := <<"integer">>}
                    }
                }
            }
        },
        Schema
    ),
    validate_with_python(Schema).

remote_type_with_variables_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {type, user_with_result, 0}),
    ?assertMatch(
        #{
            properties := #{
                <<"result">> := #{
                    type := <<"object">>,
                    properties := #{
                        <<"value">> := #{type := <<"string">>},
                        <<"errors">> := _
                    }
                }
            }
        },
        Schema
    ),
    validate_with_python(Schema).

multiple_remote_types_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {type, organization, 0}),
    ?assertMatch(
        #{
            type := <<"object">>,
            properties := #{
                <<"primary_account">> := #{type := <<"object">>},
                <<"backup_account">> := #{type := <<"object">>},
                <<"status">> := #{type := <<"object">>}
            },
            required := _
        },
        Schema
    ),
    #{required := Required} = Schema,
    ?assertEqual([<<"primary_account">>, <<"status">>], lists:sort(Required)),
    validate_with_python(Schema).

remote_type_in_record_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {record, transaction}),
    ?assertMatch(
        #{
            type := <<"object">>,
            properties := #{
                <<"id">> := _,
                <<"account">> := #{type := <<"object">>},
                <<"result">> := #{
                    type := <<"object">>,
                    properties := #{<<"value">> := #{type := <<"string">>}}
                }
            }
        },
        Schema
    ),
    validate_with_python(Schema).

optional_remote_type_field_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {record, optional_account_holder}),
    ?assertMatch(
        #{
            type := <<"object">>,
            properties := #{
                <<"id">> := _,
                <<"name">> := _,
                <<"account">> := #{type := <<"object">>}
            },
            required := _
        },
        Schema
    ),
    #{required := Required} = Schema,
    ?assertEqual([<<"id">>, <<"name">>], lists:sort(Required)),
    validate_with_python(Schema).

remote_type_in_list_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {type, account_list, 0}),
    ?assertMatch(
        #{
            type := <<"array">>,
            items := #{
                type := <<"object">>,
                properties := #{<<"id">> := _, <<"balance">> := _}
            }
        },
        Schema
    ),
    validate_with_python(Schema).

remote_type_in_union_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {type, account_or_string, 0}),
    #{oneOf := OneOf} = Schema,
    Types = lists:sort([maps:get(type, S) || S <- OneOf]),
    ?assertEqual([<<"object">>, <<"string">>], Types),
    validate_with_python(Schema).

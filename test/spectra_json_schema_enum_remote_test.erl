-module(spectra_json_schema_enum_remote_test).

-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_unused_type).

%% Helper to validate schemas with Python validator
validate_with_python(Schema) ->
    json_schema_validator_helper:validate_or_skip(Schema).

%% Local enum type alias
-type local_status() :: active | inactive.

%% Union with remote enum type
-type result_with_status() :: {ok, other:status()} | {error, binary()}.

%% Union of remote enum literals
-type status_or_priority() :: other:status() | other:priority().

%% Union mixing remote enum with local literals
-type extended_status() :: other:status() | archived.

%% Union with user type reference that resolves to literals
-type optional_local_status() :: local_status() | undefined.

%% Record with remote enum field
-record(task, {
    id :: integer(),
    status :: other:status(),
    priority :: other:priority()
}).

-type task() :: #task{}.

%% This test demonstrates the FIXME issue: remote types that resolve to literals
%% should be handled in enum schema generation
remote_enum_type_in_union_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {type, status_or_priority, 0}),
    %% This test will fail if we get oneOf instead of a unified enum
    ?assertNot(maps:is_key(oneOf, Schema)),
    %% Should generate a single enum with all literal values
    ?assertMatch(#{enum := _}, Schema),
    #{enum := EnumValues} = Schema,
    ?assertEqual(6, length(EnumValues)),
    ExpectedValues = lists:sort([
        <<"active">>,
        <<"inactive">>,
        <<"pending">>,
        <<"low">>,
        <<"medium">>,
        <<"high">>
    ]),
    ?assertEqual(ExpectedValues, lists:sort(EnumValues)),
    validate_with_python(Schema).

%% Test user type reference that resolves to literals
user_type_ref_enum_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {type, optional_local_status, 0}),
    %% Should resolve local_status() to its literals and handle the union with undefined
    ?assertEqual(
        #{
            <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"string">>,
            enum => [<<"active">>, <<"inactive">>]
        },
        Schema
    ),
    validate_with_python(Schema).

%% Test remote enum mixed with local literal
extended_status_enum_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {type, extended_status, 0}),
    %% Should combine remote enum literals with local literal
    ?assertNot(maps:is_key(oneOf, Schema)),
    ?assertMatch(#{enum := _}, Schema),
    #{enum := EnumValues} = Schema,
    ?assertEqual(4, length(EnumValues)),
    ExpectedValues = lists:sort([
        <<"active">>,
        <<"inactive">>,
        <<"pending">>,
        <<"archived">>
    ]),
    ?assertEqual(ExpectedValues, lists:sort(EnumValues)),
    validate_with_python(Schema).

%% Test record with remote enum fields
record_with_remote_enum_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {record, task}),
    ?assertMatch(
        #{
            type := <<"object">>,
            properties := #{
                <<"id">> := #{type := <<"integer">>},
                <<"status">> := _,
                <<"priority">> := _
            },
            required := _
        },
        Schema
    ),

    %% Check that enum fields are properly generated
    #{properties := Props} = Schema,
    #{<<"status">> := StatusSchema} = Props,
    #{<<"priority">> := PrioritySchema} = Props,

    %% These should be enum schemas
    ?assertMatch(#{enum := _}, StatusSchema),
    ?assertMatch(#{enum := _}, PrioritySchema),

    validate_with_python(Schema).

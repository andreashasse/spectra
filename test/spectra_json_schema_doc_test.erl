-module(spectra_json_schema_doc_test).

-include_lib("eunit/include/eunit.hrl").

-compile([nowarn_unused_type, nowarn_unused_record]).

-spectra(#{
    title => <<"User ID">>,
    description => <<"Unique identifier for users in the system">>,
    examples => [1, 2, 100, 42]
}).
-type user_id() :: pos_integer().

-spectra(#{
    title => <<"User Status">>,
    description => <<"Current status of the user account">>,
    examples => [active, inactive]
}).
-type status() :: active | inactive | pending.

-spectra(#{
    title => <<"Email Address">>,
    description => <<"User's email address in standard format">>
}).
-type email() :: binary().

-type age() :: non_neg_integer().

-type simple_type() :: integer().

-spectra(#{
    title => <<"User Record">>,
    description => <<"A user in the system">>,
    examples => [
        {user, 1, <<"Alice">>, active},
        {user, 42, <<"Bob">>, inactive}
    ]
}).
-record(user, {
    id :: user_id(),
    name :: binary(),
    status :: status()
}).

-record(simple_record, {
    value :: integer()
}).

validate_with_python(Schema) ->
    json_schema_validator_helper:validate_or_skip(Schema).

doc_basic_test() ->
    SchemaJson = spectra:schema(json_schema, ?MODULE, {type, user_id, 0}),
    Schema = json:decode(iolist_to_binary(SchemaJson)),
    ?assertEqual(
        #{
            <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
            <<"type">> => <<"integer">>,
            <<"minimum">> => 1,
            <<"title">> => <<"User ID">>,
            <<"description">> => <<"Unique identifier for users in the system">>,
            <<"examples">> => [1, 2, 100, 42]
        },
        Schema
    ),
    validate_with_python(Schema).

doc_with_enum_test() ->
    SchemaJson = spectra:schema(json_schema, ?MODULE, {type, status, 0}),
    Schema = json:decode(iolist_to_binary(SchemaJson)),
    ?assertEqual(
        #{
            <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
            <<"type">> => <<"string">>,
            <<"enum">> => [<<"active">>, <<"inactive">>, <<"pending">>],
            <<"title">> => <<"User Status">>,
            <<"description">> => <<"Current status of the user account">>,
            <<"examples">> => [<<"active">>, <<"inactive">>]
        },
        Schema
    ),
    validate_with_python(Schema).

doc_with_string_test() ->
    SchemaJson = spectra:schema(json_schema, ?MODULE, {type, email, 0}),
    Schema = json:decode(iolist_to_binary(SchemaJson)),
    ?assertEqual(
        #{
            <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
            <<"type">> => <<"string">>,
            <<"title">> => <<"Email Address">>,
            <<"description">> => <<"User's email address in standard format">>
        },
        Schema
    ),
    validate_with_python(Schema).

no_doc_test() ->
    SchemaJson = spectra:schema(json_schema, ?MODULE, {type, simple_type, 0}),
    Schema = json:decode(iolist_to_binary(SchemaJson)),
    ?assertEqual(
        #{
            <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
            <<"type">> => <<"integer">>
        },
        Schema
    ),
    validate_with_python(Schema).

record_doc_test() ->
    SchemaJson = spectra:schema(json_schema, ?MODULE, {record, user}),
    Schema = json:decode(iolist_to_binary(SchemaJson)),

    ?assertEqual(<<"User Record">>, maps:get(<<"title">>, Schema)),
    ?assertEqual(<<"A user in the system">>, maps:get(<<"description">>, Schema)),
    ?assertEqual(<<"object">>, maps:get(<<"type">>, Schema)),

    Properties = maps:get(<<"properties">>, Schema),
    ?assert(maps:is_key(<<"id">>, Properties)),
    ?assert(maps:is_key(<<"name">>, Properties)),
    ?assert(maps:is_key(<<"status">>, Properties)),

    Examples = maps:get(<<"examples">>, Schema),
    ?assertEqual(
        [
            #{<<"id">> => 1, <<"name">> => <<"Alice">>, <<"status">> => <<"active">>},
            #{<<"id">> => 42, <<"name">> => <<"Bob">>, <<"status">> => <<"inactive">>}
        ],
        Examples
    ),

    validate_with_python(Schema).

record_no_doc_test() ->
    SchemaJson = spectra:schema(json_schema, ?MODULE, {record, simple_record}),
    Schema = json:decode(iolist_to_binary(SchemaJson)),

    ?assertEqual(false, maps:is_key(<<"title">>, Schema)),
    ?assertEqual(false, maps:is_key(<<"description">>, Schema)),
    ?assertEqual(<<"object">>, maps:get(<<"type">>, Schema)),

    validate_with_python(Schema).

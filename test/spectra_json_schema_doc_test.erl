-module(spectra_json_schema_doc_test).

-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_unused_type).

%% Test types with documentation
%% Using type_doc attribute which will be stored in the :attributes chunk

-type_doc([
    {user_id, #{
        title => <<"User ID">>,
        description => <<"Unique identifier for users in the system">>,
        examples => [1, 2, 100, 42]
    }}
]).
-type user_id() :: pos_integer().

-type_doc([
    {status, #{
        title => <<"User Status">>,
        description => <<"Current status of the user account">>,
        examples => [active, inactive]
    }}
]).
-type status() :: active | inactive | pending.

-type_doc([
    {email, #{
        title => <<"Email Address">>,
        description => <<"User's email address in standard format">>
    }}
]).
-type email() :: binary().

-type_doc([
    {age, #{
        title => <<"Age">>,
        description => <<"User's age in years">>,
        examples => [25, 30, 45],
        default => 18
    }}
]).
-type age() :: non_neg_integer().

%% Type without documentation
-type simple_type() :: integer().

%% Test documentation in JSON Schema generation
doc_basic_test() ->
    Schema = spectra_json_schema:to_schema(?MODULE, {type, user_id, 0}),
    ?assertEqual(
        #{
            <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"integer">>,
            minimum => 1,
            title => <<"User ID">>,
            description => <<"Unique identifier for users in the system">>,
            examples => [1, 2, 100, 42]
        },
        Schema
    ).

doc_with_enum_test() ->
    Schema = spectra_json_schema:to_schema(?MODULE, {type, status, 0}),
    ?assertEqual(
        #{
            <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"string">>,
            enum => [<<"active">>, <<"inactive">>, <<"pending">>],
            title => <<"User Status">>,
            description => <<"Current status of the user account">>,
            examples => [active, inactive]
        },
        Schema
    ).

doc_with_string_test() ->
    Schema = spectra_json_schema:to_schema(?MODULE, {type, email, 0}),
    ?assertEqual(
        #{
            <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"string">>,
            title => <<"Email Address">>,
            description => <<"User's email address in standard format">>
        },
        Schema
    ).

doc_with_default_test() ->
    Schema = spectra_json_schema:to_schema(?MODULE, {type, age, 0}),
    ?assertEqual(
        #{
            <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"integer">>,
            minimum => 0,
            title => <<"Age">>,
            description => <<"User's age in years">>,
            examples => [25, 30, 45],
            default => 18
        },
        Schema
    ).

no_doc_test() ->
    Schema = spectra_json_schema:to_schema(?MODULE, {type, simple_type, 0}),
    ?assertEqual(
        #{
            <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"integer">>
        },
        Schema
    ).

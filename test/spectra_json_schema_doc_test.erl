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

%% Test that record references include documentation from the record
-type user_ref() :: #user{}.

%% Test a type with its own -spectra attribute that references a record
-spectra(#{
    title => <<"Active User">>,
    description => <<"A user that is currently active in the system">>,
    examples => [
        {user, 99, <<"Charlie">>, active}
    ]
}).
-type active_user() :: #user{}.

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

    ?assertMatch(
        #{
            <<"$schema">> := <<"https://json-schema.org/draft/2020-12/schema">>,
            <<"type">> := <<"object">>,
            <<"title">> := <<"User Record">>,
            <<"description">> := <<"A user in the system">>,
            <<"properties">> := #{
                <<"id">> := _,
                <<"name">> := _,
                <<"status">> := _
            },
            <<"examples">> := [
                #{<<"id">> := 1, <<"name">> := <<"Alice">>, <<"status">> := <<"active">>},
                #{<<"id">> := 42, <<"name">> := <<"Bob">>, <<"status">> := <<"inactive">>}
            ]
        },
        Schema
    ),

    validate_with_python(Schema).

record_no_doc_test() ->
    SchemaJson = spectra:schema(json_schema, ?MODULE, {record, simple_record}),
    Schema = json:decode(iolist_to_binary(SchemaJson)),

    %% Check the exact schema - no title, no description, just the required fields
    ?assertEqual(
        #{
            <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"value">> => #{
                    <<"type">> => <<"integer">>
                }
            },
            <<"required">> => [<<"value">>]
        },
        Schema
    ),

    validate_with_python(Schema).

record_ref_doc_test() ->
    SchemaJson = spectra:schema(json_schema, ?MODULE, {type, user_ref, 0}),
    Schema = json:decode(iolist_to_binary(SchemaJson)),

    %% The schema should include the doc metadata from the user record
    ?assertMatch(
        #{
            <<"$schema">> := <<"https://json-schema.org/draft/2020-12/schema">>,
            <<"type">> := <<"object">>,
            <<"title">> := <<"User Record">>,
            <<"description">> := <<"A user in the system">>,
            <<"properties">> := #{
                <<"id">> := _,
                <<"name">> := _,
                <<"status">> := _
            },
            <<"examples">> := [
                #{<<"id">> := 1, <<"name">> := <<"Alice">>, <<"status">> := <<"active">>},
                #{<<"id">> := 42, <<"name">> := <<"Bob">>, <<"status">> := <<"inactive">>}
            ]
        },
        Schema
    ),

    validate_with_python(Schema).

%% Test that a type's own -spectra attribute takes precedence over the record's
record_ref_with_own_doc_test() ->
    SchemaJson = spectra:schema(json_schema, ?MODULE, {type, active_user, 0}),
    Schema = json:decode(iolist_to_binary(SchemaJson)),

    %% The schema should use the type's own documentation, not the record's
    ?assertMatch(
        #{
            <<"$schema">> := <<"https://json-schema.org/draft/2020-12/schema">>,
            <<"type">> := <<"object">>,
            <<"title">> := <<"Active User">>,
            <<"description">> := <<"A user that is currently active in the system">>,
            <<"properties">> := #{
                <<"id">> := _,
                <<"name">> := _,
                <<"status">> := _
            },
            <<"examples">> := [
                #{<<"id">> := 99, <<"name">> := <<"Charlie">>, <<"status">> := <<"active">>}
            ]
        },
        Schema
    ),

    validate_with_python(Schema).

%% Test passing sp_rec{} directly to to_schema
record_sp_rec_direct_test() ->
    TypeInfo = spectra_module_types:get(?MODULE),
    {ok, UserRec} = spectra_type_info:find_record(TypeInfo, user),
    SchemaJson = spectra:schema(json_schema, TypeInfo, UserRec),
    Schema = json:decode(iolist_to_binary(SchemaJson)),

    %% Should include documentation from the record
    ?assertMatch(
        #{
            <<"$schema">> := <<"https://json-schema.org/draft/2020-12/schema">>,
            <<"type">> := <<"object">>,
            <<"title">> := <<"User Record">>,
            <<"description">> := <<"A user in the system">>,
            <<"properties">> := #{
                <<"id">> := _,
                <<"name">> := _,
                <<"status">> := _
            }
        },
        Schema
    ),

    validate_with_python(Schema).

%% Test passing sp_rec_ref{} directly to to_schema
record_sp_rec_ref_direct_test() ->
    TypeInfo = spectra_module_types:get(?MODULE),
    % Get the user_ref type which is #sp_rec_ref{}
    UserRefType = spectra_type_info:get_type(TypeInfo, user_ref, 0),
    SchemaJson = spectra:schema(json_schema, TypeInfo, UserRefType),
    Schema = json:decode(iolist_to_binary(SchemaJson)),

    %% Should include documentation from the resolved record
    ?assertMatch(
        #{
            <<"$schema">> := <<"https://json-schema.org/draft/2020-12/schema">>,
            <<"type">> := <<"object">>,
            <<"title">> := <<"User Record">>,
            <<"description">> := <<"A user in the system">>,
            <<"properties">> := #{
                <<"id">> := _,
                <<"name">> := _,
                <<"status">> := _
            }
        },
        Schema
    ),

    validate_with_python(Schema).

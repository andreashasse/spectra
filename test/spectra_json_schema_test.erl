-module(spectra_json_schema_test).

-include_lib("eunit/include/eunit.hrl").
-include("../include/spectra_internal.hrl").

-compile(nowarn_unused_type).

%% Helper to validate schemas with Python validator
validate_with_python(Schema) ->
    json_schema_validator_helper:validate_or_skip(Schema).

%% Test types for schema generation
-type my_integer() :: integer().
-type my_string() :: string().
-type my_boolean() :: boolean().
-type my_number() :: number().
-type my_atom() :: atom().
-type my_binary() :: binary().
-type my_float() :: float().
-type my_iodata() :: iodata().
-type my_iolist() :: iolist().
%% Range types
-type my_range() :: 1..10.
-type my_byte() :: byte().
-type my_char() :: char().
%% Literal types
-type my_literal_atom() :: hello.
-type my_literal_integer() :: 42.
%% List types
-type my_list() :: [integer()].
-type my_nonempty_list() :: [string(), ...].
%% Union types
-type my_union() :: integer() | string().
-type my_optional() :: integer() | undefined.
%% Enum types (unions of literals)
-type role() :: admin | user | guest.
-type status() :: active | inactive | pending.
-type number_enum() :: 1 | 2 | 3.
-type bool_enum() :: true | false.
-type optional_enum() :: admin | user | undefined.
-type optional_enum_with_nil() :: admin | user | nil.
%% Mixed-type enum literal (different types)
-type mixed_enum() :: admin | 42 | true.
%% Map types
-type my_map() :: #{name := string(), age := integer()}.
-type my_flexible_map() :: #{config := string(), timeout := integer()}.
%% Generic map types (allow additional properties)
-type my_generic_map() :: #{atom() => integer()}.
-type my_mixed_map() :: #{name := string(), atom() => integer()}.

%% Record types
-record(user, {id :: integer(), name :: string(), email :: string()}).
-record(product, {id :: integer(), name :: string(), price :: float(), tags :: [string()]}).
-record(user_with_optional, {id :: integer(), name :: string(), email :: string() | undefined}).
-record(user_with_role, {id :: integer(), name :: string(), role :: role()}).
-record(user_with_optional_role, {
    id :: integer(), name :: string(), role :: optional_enum()
}).

%% Type aliases for records to avoid unused warnings
-type user() :: #user{}.
-type product() :: #product{}.
-type user_with_optional() :: #user_with_optional{}.
-type user_with_role() :: #user_with_role{}.
-type user_with_optional_role() :: #user_with_optional_role{}.

%% Test simple type mappings
simple_types_test() ->
    %% integer
    IntSchema = spectra_json_schema:to_schema(?MODULE, {type, my_integer, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"integer">>
        },
        IntSchema
    ),
    validate_with_python(IntSchema),

    %% string
    StringSchema = spectra_json_schema:to_schema(?MODULE, {type, my_string, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"string">>
        },
        StringSchema
    ),
    validate_with_python(StringSchema),

    %% boolean
    BoolSchema = spectra_json_schema:to_schema(?MODULE, {type, my_boolean, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"boolean">>
        },
        BoolSchema
    ),
    validate_with_python(BoolSchema),

    %% number
    NumberSchema = spectra_json_schema:to_schema(?MODULE, {type, my_number, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"number">>
        },
        NumberSchema
    ),
    validate_with_python(NumberSchema),

    %% atom (mapped to string)
    AtomSchema = spectra_json_schema:to_schema(?MODULE, {type, my_atom, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"string">>
        },
        AtomSchema
    ),
    validate_with_python(AtomSchema),

    %% binary (mapped to string)
    BinarySchema = spectra_json_schema:to_schema(?MODULE, {type, my_binary, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"string">>
        },
        BinarySchema
    ),
    validate_with_python(BinarySchema),

    %% float
    FloatSchema = spectra_json_schema:to_schema(?MODULE, {type, my_float, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"number">>,
            format => <<"float">>
        },
        FloatSchema
    ),
    validate_with_python(FloatSchema),

    %% iodata (mapped to string)
    IodataSchema = spectra_json_schema:to_schema(?MODULE, {type, my_iodata, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"string">>
        },
        IodataSchema
    ),
    validate_with_python(IodataSchema),

    %% iolist (mapped to string)
    IolistSchema = spectra_json_schema:to_schema(?MODULE, {type, my_iolist, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"string">>
        },
        IolistSchema
    ),
    validate_with_python(IolistSchema).

%% Test range type mappings
range_types_test() ->
    %% Custom range 1..10
    RangeSchema = spectra_json_schema:to_schema(?MODULE, {type, my_range, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"integer">>,
            minimum => 1,
            maximum => 10
        },
        RangeSchema
    ),
    validate_with_python(RangeSchema),

    %% byte (0..255)
    ByteSchema = spectra_json_schema:to_schema(?MODULE, {type, my_byte, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"integer">>,
            minimum => 0,
            maximum => 255
        },
        ByteSchema
    ),
    validate_with_python(ByteSchema),

    %% char (0..1114111)
    CharSchema = spectra_json_schema:to_schema(?MODULE, {type, my_char, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"integer">>,
            minimum => 0,
            maximum => 1114111
        },
        CharSchema
    ),
    validate_with_python(CharSchema).

%% Test literal type mappings
literal_types_test() ->
    %% Literal atom (converted to binary string)
    LiteralAtomSchema = spectra_json_schema:to_schema(?MODULE, {type, my_literal_atom, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            enum => [<<"hello">>]
        },
        LiteralAtomSchema
    ),
    validate_with_python(LiteralAtomSchema),

    %% Literal integer
    LiteralIntSchema = spectra_json_schema:to_schema(?MODULE, {type, my_literal_integer, 0}),
    ?assertEqual(
        #{'$schema' => <<"https://json-schema.org/draft/2020-12/schema">>, enum => [42]},
        LiteralIntSchema
    ),
    validate_with_python(LiteralIntSchema).

unsupported_literal_test() ->
    FloatLiteral = #sp_literal{value = 3.14},
    ?assertError(
        {type_not_supported, FloatLiteral}, spectra_json_schema:to_schema(?MODULE, FloatLiteral)
    ).

%% Test list type mappings
list_types_test() ->
    %% Regular list
    ListSchema = spectra_json_schema:to_schema(?MODULE, {type, my_list, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"array">>,
            items => #{type => <<"integer">>}
        },
        ListSchema
    ),
    validate_with_python(ListSchema),

    %% Non-empty list
    NonemptyListSchema = spectra_json_schema:to_schema(?MODULE, {type, my_nonempty_list, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"array">>,
            items => #{type => <<"string">>},
            minItems => 1
        },
        NonemptyListSchema
    ),
    validate_with_python(NonemptyListSchema).

%% Test union type mappings
union_types_test() ->
    %% Simple union of non-literals (should use oneOf)
    UnionSchema = spectra_json_schema:to_schema(?MODULE, {type, my_union, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            oneOf => [#{type => <<"integer">>}, #{type => <<"string">>}]
        },
        UnionSchema
    ),
    validate_with_python(UnionSchema),

    %% Optional type (union with undefined) - now returns just the non-undefined type
    OptionalSchema = spectra_json_schema:to_schema(?MODULE, {type, my_optional, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"integer">>
        },
        OptionalSchema
    ),
    validate_with_python(OptionalSchema).

%% Test map type mappings
map_types_test() ->
    %% Fixed map fields - order doesn't matter, just check structure
    MapSchema = spectra_json_schema:to_schema(?MODULE, {type, my_map, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"object">>,
            properties =>
                #{
                    <<"name">> => #{type => <<"string">>},
                    <<"age">> => #{type => <<"integer">>}
                },
            required => [<<"age">>, <<"name">>],
            additionalProperties => false
        },
        MapSchema
    ),
    validate_with_python(MapSchema),

    %% Structured map with specific fields
    FlexibleMapSchema = spectra_json_schema:to_schema(?MODULE, {type, my_flexible_map, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"object">>,
            properties =>
                #{
                    <<"config">> => #{type => <<"string">>},
                    <<"timeout">> => #{type => <<"integer">>}
                },
            required => [<<"timeout">>, <<"config">>],
            additionalProperties => false
        },
        FlexibleMapSchema
    ),
    validate_with_python(FlexibleMapSchema).

%% Test generic map types with additional properties
generic_map_types_test() ->
    %% Generic map with atom keys and integer values
    GenericMapSchema = spectra_json_schema:to_schema(?MODULE, {type, my_generic_map, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"object">>,
            additionalProperties => true
        },
        GenericMapSchema
    ),
    validate_with_python(GenericMapSchema),

    %% Mixed map with both specific and generic fields
    MixedMapSchema = spectra_json_schema:to_schema(?MODULE, {type, my_mixed_map, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"object">>,
            properties => #{<<"name">> => #{type => <<"string">>}},
            required => [<<"name">>],
            additionalProperties => true
        },
        MixedMapSchema
    ),
    validate_with_python(MixedMapSchema).

%% Test record type mappings
record_types_test() ->
    %% Simple record - check the actual return format
    UserSchema = spectra_json_schema:to_schema(?MODULE, {record, user}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"object">>,
            properties =>
                #{
                    <<"id">> => #{type => <<"integer">>},
                    <<"name">> => #{type => <<"string">>},
                    <<"email">> => #{type => <<"string">>}
                },
            required => [<<"id">>, <<"name">>, <<"email">>]
        },
        UserSchema
    ),
    validate_with_python(UserSchema),

    %% Record with array field
    ProductSchema = spectra_json_schema:to_schema(?MODULE, {record, product}),
    ExpectedProps =
        #{
            <<"id">> => #{type => <<"integer">>},
            <<"name">> => #{type => <<"string">>},
            <<"price">> => #{type => <<"number">>, format => <<"float">>},
            <<"tags">> => #{type => <<"array">>, items => #{type => <<"string">>}}
        },
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"object">>,
            properties => ExpectedProps,
            required => [<<"id">>, <<"name">>, <<"price">>, <<"tags">>]
        },
        ProductSchema
    ),
    validate_with_python(ProductSchema).

%% Test record with optional field
record_with_optional_fields_test() ->
    %% Record with optional email field (can be undefined)
    %% The field appears as simple string type and is excluded from required fields
    UserWithOptionalSchema = spectra_json_schema:to_schema(
        ?MODULE, {record, user_with_optional}
    ),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"object">>,
            properties =>
                #{
                    <<"id">> => #{type => <<"integer">>},
                    <<"name">> => #{type => <<"string">>},
                    <<"email">> => #{type => <<"string">>}
                },
            required => [<<"id">>, <<"name">>]
        },
        UserWithOptionalSchema
    ),
    validate_with_python(UserWithOptionalSchema).

%% Test record with enum fields
record_with_enum_fields_test() ->
    %% Record with required enum field (no undefined/nil)
    UserWithRoleSchema =
        spectra_json_schema:to_schema(?MODULE, {record, user_with_role}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"object">>,
            properties =>
                #{
                    <<"id">> => #{type => <<"integer">>},
                    <<"name">> => #{type => <<"string">>},
                    <<"role">> => #{
                        type => <<"string">>,
                        enum => [<<"admin">>, <<"user">>, <<"guest">>]
                    }
                },
            required => [<<"id">>, <<"name">>, <<"role">>]
        },
        UserWithRoleSchema
    ),
    %% Verify role field is required
    ?assert(lists:member(<<"role">>, maps:get(required, UserWithRoleSchema))),
    validate_with_python(UserWithRoleSchema),

    %% Record with optional enum field (contains undefined)
    UserWithOptionalRoleSchema =
        spectra_json_schema:to_schema(?MODULE, {record, user_with_optional_role}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"object">>,
            properties =>
                #{
                    <<"id">> => #{type => <<"integer">>},
                    <<"name">> => #{type => <<"string">>},
                    <<"role">> => #{
                        type => <<"string">>, enum => [<<"admin">>, <<"user">>]
                    }
                },
            required => [<<"id">>, <<"name">>]
        },
        UserWithOptionalRoleSchema
    ),
    %% Verify role field is NOT required
    ?assertNot(lists:member(<<"role">>, maps:get(required, UserWithOptionalRoleSchema))),
    validate_with_python(UserWithOptionalRoleSchema).

%% Test enum type optimization (unions of literals should become single enum)
enum_types_test() ->
    %% Atom enum (like role: admin | user | guest)
    RoleSchema = spectra_json_schema:to_schema(?MODULE, {type, role, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"string">>,
            enum => [<<"admin">>, <<"user">>, <<"guest">>]
        },
        RoleSchema
    ),
    validate_with_python(RoleSchema),

    %% Status enum
    StatusSchema = spectra_json_schema:to_schema(?MODULE, {type, status, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"string">>,
            enum => [<<"active">>, <<"inactive">>, <<"pending">>]
        },
        StatusSchema
    ),
    validate_with_python(StatusSchema),

    %% Number enum
    NumberEnumSchema = spectra_json_schema:to_schema(?MODULE, {type, number_enum, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"integer">>,
            enum => [1, 2, 3]
        },
        NumberEnumSchema
    ),
    validate_with_python(NumberEnumSchema),

    %% Boolean enum
    BoolEnumSchema = spectra_json_schema:to_schema(?MODULE, {type, bool_enum, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"boolean">>,
            enum => [true, false]
        },
        BoolEnumSchema
    ),
    validate_with_python(BoolEnumSchema),

    %% Optional enum (union with undefined)
    OptionalEnumSchema = spectra_json_schema:to_schema(?MODULE, {type, optional_enum, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"string">>,
            enum => [<<"admin">>, <<"user">>]
        },
        OptionalEnumSchema
    ),
    validate_with_python(OptionalEnumSchema),

    %% Optional enum (union with nil - Elixir's missing value)
    OptionalEnumWithNilSchema = spectra_json_schema:to_schema(
        ?MODULE, {type, optional_enum_with_nil, 0}
    ),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"string">>,
            enum => [<<"admin">>, <<"user">>]
        },
        OptionalEnumWithNilSchema
    ),
    validate_with_python(OptionalEnumWithNilSchema),

    %% Mixed-type enum (atom | integer | boolean) - no type field when types differ
    MixedEnumSchema = spectra_json_schema:to_schema(?MODULE, {type, mixed_enum, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            enum => [<<"admin">>, 42, true]
        },
        MixedEnumSchema
    ),
    ?assertNot(maps:is_key(type, MixedEnumSchema)),
    validate_with_python(MixedEnumSchema).

%% Test error handling
error_handling_test() ->
    ?assertError(_, spectra_json_schema:to_schema(?MODULE, {type, non_existent_type, 0})),
    ?assertError(_, spectra_json_schema:to_schema(?MODULE, {record, non_existent_record})).

-module(spectra_string_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-compile(nowarn_unused_type).

%% Helper: resolve a type reference to an sp_type() struct
resolve_type(TypeInfo, Name, Arity) ->
    spectra_type_info:get_type(TypeInfo, Name, Arity).

%% Test types
-type my_integer() :: integer().
-type my_float() :: float().
-type my_number() :: number().
-type my_boolean() :: boolean().
-type my_atom() :: atom().
-type my_string() :: string().
-type my_nonempty_string() :: nonempty_string().
-type my_binary() :: binary().
-type my_nonempty_binary() :: nonempty_binary().
-type my_non_neg_integer() :: non_neg_integer().
-type my_pos_integer() :: pos_integer().
-type my_neg_integer() :: neg_integer().
-type my_range() :: 1..10.
-type my_literal_atom() :: hello.
-type my_literal_integer() :: 42.
-type my_literal_boolean() :: true.
-type my_union() :: integer() | boolean().
-type my_complex_union() :: 1 | 2 | true | false.
-type my_var_integer() :: T :: integer().

%% Test sp_simple_type conversions
simple_types_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% integer
    ?assertEqual(
        {ok, 42},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = integer}, "42")
    ),
    ?assertEqual(
        {ok, -42},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = integer}, "-42")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(
            TypeInfo,
            #sp_simple_type{type = integer},
            "not_a_number"
        )
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = integer}, "3.14")
    ),

    %% float
    ?assertEqual(
        {ok, 3.14},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = float}, "3.14")
    ),
    ?assertEqual(
        {ok, -3.14},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = float}, "-3.14")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(
            TypeInfo,
            #sp_simple_type{type = float},
            "not_a_number"
        )
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = float}, "42")
    ),

    %% number (tries integer first, then float)
    ?assertEqual(
        {ok, 42},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = number}, "42")
    ),
    ?assertEqual(
        {ok, 3.14},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = number}, "3.14")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(
            TypeInfo,
            #sp_simple_type{type = number},
            "not_a_number"
        )
    ),

    %% boolean
    ?assertEqual(
        {ok, true},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = boolean}, "true")
    ),
    ?assertEqual(
        {ok, false},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = boolean}, "false")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = boolean}, "True")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = boolean}, "1")
    ),

    %% atom
    ?assertEqual(
        {ok, hello},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = atom}, "hello")
    ),
    ?assertEqual(
        {ok, 'hello world'},
        spectra_test_util:from_string(
            TypeInfo,
            #sp_simple_type{type = atom},
            "hello world"
        )
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(
            TypeInfo,
            #sp_simple_type{type = atom},
            "non_existing_atom_123456789"
        )
    ),

    %% string
    ?assertEqual(
        {ok, "hello"},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = string}, "hello")
    ),
    ?assertEqual(
        {ok, ""},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = string}, "")
    ),
    ?assertEqual(
        {ok, "hello world 123!"},
        spectra_test_util:from_string(
            TypeInfo,
            #sp_simple_type{type = string},
            "hello world 123!"
        )
    ),

    %% nonempty_string
    ?assertEqual(
        {ok, "hello"},
        spectra_test_util:from_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_string},
            "hello"
        )
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_string},
            ""
        )
    ),

    %% binary
    ?assertEqual(
        {ok, <<"hello">>},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = binary}, "hello")
    ),
    ?assertEqual(
        {ok, <<"">>},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = binary}, "")
    ),

    %% nonempty_binary
    ?assertEqual(
        {ok, <<"hello">>},
        spectra_test_util:from_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_binary},
            "hello"
        )
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_binary},
            ""
        )
    ),

    %% non_neg_integer
    ?assertEqual(
        {ok, 42},
        spectra_test_util:from_string(
            TypeInfo,
            #sp_simple_type{type = non_neg_integer},
            "42"
        )
    ),
    ?assertEqual(
        {ok, 0},
        spectra_test_util:from_string(
            TypeInfo,
            #sp_simple_type{type = non_neg_integer},
            "0"
        )
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(
            TypeInfo,
            #sp_simple_type{type = non_neg_integer},
            "-1"
        )
    ),

    %% pos_integer
    ?assertEqual(
        {ok, 42},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = pos_integer}, "42")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = pos_integer}, "0")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = pos_integer}, "-1")
    ),

    %% neg_integer
    ?assertEqual(
        {ok, -42},
        spectra_test_util:from_string(
            TypeInfo,
            #sp_simple_type{type = neg_integer},
            "-42"
        )
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = neg_integer}, "0")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = neg_integer}, "42")
    ),

    ok.

%% Test sp_range conversions
range_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    Range =
        #sp_range{
            type = integer,
            lower_bound = 1,
            upper_bound = 10
        },

    %% Valid values in range
    ?assertEqual({ok, 1}, spectra_test_util:from_string(TypeInfo, Range, "1")),
    ?assertEqual({ok, 5}, spectra_test_util:from_string(TypeInfo, Range, "5")),
    ?assertEqual({ok, 10}, spectra_test_util:from_string(TypeInfo, Range, "10")),

    %% Invalid values outside range
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, Range, "0")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, Range, "11")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, Range, "-5")
    ),

    %% Invalid non-integer strings
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, Range, "not_a_number")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, Range, "5.5")
    ),

    ok.

%% Test sp_literal conversions
literal_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Literal atom
    AtomLiteral = #sp_literal{value = hello},
    ?assertEqual({ok, hello}, spectra_test_util:from_string(TypeInfo, AtomLiteral, "hello")),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, AtomLiteral, "world")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(
            TypeInfo,
            AtomLiteral,
            "non_existing_atom_123456789"
        )
    ),

    %% Literal integer
    IntegerLiteral = #sp_literal{value = 42},
    ?assertEqual({ok, 42}, spectra_test_util:from_string(TypeInfo, IntegerLiteral, "42")),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, IntegerLiteral, "43")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, IntegerLiteral, "not_a_number")
    ),

    %% Literal boolean
    BooleanLiteral = #sp_literal{value = true},
    ?assertEqual({ok, true}, spectra_test_util:from_string(TypeInfo, BooleanLiteral, "true")),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, BooleanLiteral, "false")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, BooleanLiteral, "True")
    ),

    ok.

%% Test sp_union conversions
union_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Simple union: integer | boolean
    Union =
        #sp_union{types = [#sp_simple_type{type = integer}, #sp_simple_type{type = boolean}]},
    ?assertEqual({ok, 42}, spectra_test_util:from_string(TypeInfo, Union, "42")),
    ?assertEqual({ok, true}, spectra_test_util:from_string(TypeInfo, Union, "true")),
    ?assertEqual({ok, false}, spectra_test_util:from_string(TypeInfo, Union, "false")),
    ?assertMatch(
        {error, [#sp_error{type = no_match}]},
        spectra_test_util:from_string(TypeInfo, Union, "not_matching")
    ),

    %% Complex union with literals: 1 | 2 | true | false
    ComplexUnion =
        #sp_union{
            types =
                [
                    #sp_literal{value = 1},
                    #sp_literal{value = 2},
                    #sp_literal{value = true},
                    #sp_literal{value = false}
                ]
        },
    ?assertEqual({ok, 1}, spectra_test_util:from_string(TypeInfo, ComplexUnion, "1")),
    ?assertEqual({ok, 2}, spectra_test_util:from_string(TypeInfo, ComplexUnion, "2")),
    ?assertEqual({ok, true}, spectra_test_util:from_string(TypeInfo, ComplexUnion, "true")),
    ?assertEqual({ok, false}, spectra_test_util:from_string(TypeInfo, ComplexUnion, "false")),
    ?assertMatch(
        {error, [#sp_error{type = no_match}]},
        spectra_test_util:from_string(TypeInfo, ComplexUnion, "3")
    ),
    ?assertMatch(
        {error, [#sp_error{type = no_match}]},
        spectra_test_util:from_string(TypeInfo, ComplexUnion, "maybe")
    ),

    ok.

%% Test with type references
type_reference_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Test various type references from the module
    ?assertEqual(
        {ok, 42},
        spectra_test_util:from_string(TypeInfo, resolve_type(TypeInfo, my_integer, 0), "42")
    ),
    ?assertEqual(
        {ok, 3.14},
        spectra_test_util:from_string(TypeInfo, resolve_type(TypeInfo, my_float, 0), "3.14")
    ),
    ?assertEqual(
        {ok, 42},
        spectra_test_util:from_string(TypeInfo, resolve_type(TypeInfo, my_number, 0), "42")
    ),
    ?assertEqual(
        {ok, 3.14},
        spectra_test_util:from_string(TypeInfo, resolve_type(TypeInfo, my_number, 0), "3.14")
    ),
    ?assertEqual(
        {ok, true},
        spectra_test_util:from_string(TypeInfo, resolve_type(TypeInfo, my_boolean, 0), "true")
    ),
    ?assertEqual(
        {ok, hello},
        spectra_test_util:from_string(TypeInfo, resolve_type(TypeInfo, my_atom, 0), "hello")
    ),
    ?assertEqual(
        {ok, "test"},
        spectra_test_util:from_string(TypeInfo, resolve_type(TypeInfo, my_string, 0), "test")
    ),
    ?assertEqual(
        {ok, <<"test">>},
        spectra_test_util:from_string(TypeInfo, resolve_type(TypeInfo, my_binary, 0), "test")
    ),

    %% Test range type
    ?assertEqual(
        {ok, 5}, spectra_test_util:from_string(TypeInfo, resolve_type(TypeInfo, my_range, 0), "5")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, resolve_type(TypeInfo, my_range, 0), "15")
    ),

    %% Test literal types
    ?assertEqual(
        {ok, hello},
        spectra_test_util:from_string(TypeInfo, resolve_type(TypeInfo, my_literal_atom, 0), "hello")
    ),
    ?assertEqual(
        {ok, 42},
        spectra_test_util:from_string(TypeInfo, resolve_type(TypeInfo, my_literal_integer, 0), "42")
    ),
    ?assertEqual(
        {ok, true},
        spectra_test_util:from_string(
            TypeInfo, resolve_type(TypeInfo, my_literal_boolean, 0), "true"
        )
    ),

    %% Test union types
    ?assertEqual(
        {ok, 42}, spectra_test_util:from_string(TypeInfo, resolve_type(TypeInfo, my_union, 0), "42")
    ),
    ?assertEqual(
        {ok, true},
        spectra_test_util:from_string(TypeInfo, resolve_type(TypeInfo, my_union, 0), "true")
    ),
    ?assertEqual(
        {ok, 1},
        spectra_test_util:from_string(TypeInfo, resolve_type(TypeInfo, my_complex_union, 0), "1")
    ),
    ?assertEqual(
        {ok, false},
        spectra_test_util:from_string(
            TypeInfo, resolve_type(TypeInfo, my_complex_union, 0), "false"
        )
    ),

    ok.

%% Test unsupported types and operations
unsupported_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Record types are not supported for string conversion
    ?assertError(
        {type_not_supported, _},
        spectra_test_util:from_string(
            TypeInfo, #sp_rec{name = some_record, fields = [], arity = 0}, "test"
        )
    ),

    %% Unsupported simple types should error
    ?assertError(
        {type_not_supported, _},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = pid}, "test")
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = port}, "test")
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = reference}, "test")
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = bitstring}, "test")
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_test_util:from_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_bitstring},
            "test"
        )
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = none}, "test")
    ),

    %% Unknown type should give type mismatch error
    UnknownType = #sp_tuple{fields = any},
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, UnknownType, "test")
    ),

    ok.

%% Test edge cases
edge_cases_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Empty strings
    ?assertEqual(
        {ok, ""},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = string}, "")
    ),
    ?assertEqual(
        {ok, <<"">>},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = binary}, "")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_string},
            ""
        )
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_binary},
            ""
        )
    ),

    %% Large numbers
    ?assertEqual(
        {ok, 999999999999},
        spectra_test_util:from_string(
            TypeInfo,
            #sp_simple_type{type = integer},
            "999999999999"
        )
    ),
    ?assertEqual(
        {ok, -999999999999},
        spectra_test_util:from_string(
            TypeInfo,
            #sp_simple_type{type = integer},
            "-999999999999"
        )
    ),

    %% Special float values
    ?assertEqual(
        {ok, 0.0},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = float}, "0.0")
    ),
    ?assertEqual(
        {ok, -0.0},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = float}, "-0.0")
    ),

    %% Boundary values for ranges
    Range =
        #sp_range{
            type = integer,
            lower_bound = -5,
            upper_bound = 5
        },
    ?assertEqual({ok, -5}, spectra_test_util:from_string(TypeInfo, Range, "-5")),
    ?assertEqual({ok, 5}, spectra_test_util:from_string(TypeInfo, Range, "5")),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, Range, "-6")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, Range, "6")
    ),

    ok.

%% Test to_string/3 function - Simple types
to_string_simple_types_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% integer
    ?assertEqual(
        {ok, "42"},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = integer}, 42)
    ),
    ?assertEqual(
        {ok, "-42"},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = integer}, -42)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(
            TypeInfo,
            #sp_simple_type{type = integer},
            "not_integer"
        )
    ),

    %% float
    ?assertEqual(
        {ok, "3.14000000000000012434e+00"},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = float}, 3.14)
    ),
    ?assertEqual(
        {ok, "-3.14000000000000012434e+00"},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = float}, -3.14)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = float}, 42)
    ),

    %% number
    ?assertEqual(
        {ok, "42"},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = number}, 42)
    ),
    ?assertEqual(
        {ok, "3.14000000000000012434e+00"},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = number}, 3.14)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(
            TypeInfo,
            #sp_simple_type{type = number},
            "not_number"
        )
    ),

    %% boolean
    ?assertEqual(
        {ok, "true"},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = boolean}, true)
    ),
    ?assertEqual(
        {ok, "false"},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = boolean}, false)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = boolean}, "true")
    ),

    %% atom
    ?assertEqual(
        {ok, "hello"},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = atom}, hello)
    ),
    ?assertEqual(
        {ok, "hello world"},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = atom}, 'hello world')
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = atom}, "hello")
    ),

    %% string
    ?assertEqual(
        {ok, "hello"},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = string}, "hello")
    ),
    ?assertEqual(
        {ok, ""},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = string}, "")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(
            TypeInfo,
            #sp_simple_type{type = string},
            <<"binary">>
        )
    ),

    %% nonempty_string
    ?assertEqual(
        {ok, "hello"},
        spectra_test_util:to_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_string},
            "hello"
        )
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = nonempty_string}, "")
    ),

    %% binary
    ?assertEqual(
        {ok, "hello"},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = binary}, <<"hello">>)
    ),
    ?assertEqual(
        {ok, ""},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = binary}, <<"">>)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = binary}, "string")
    ),

    %% nonempty_binary
    ?assertEqual(
        {ok, "hello"},
        spectra_test_util:to_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_binary},
            <<"hello">>
        )
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_binary},
            <<"">>
        )
    ),

    %% non_neg_integer
    ?assertEqual(
        {ok, "42"},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = non_neg_integer}, 42)
    ),
    ?assertEqual(
        {ok, "0"},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = non_neg_integer}, 0)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = non_neg_integer}, -1)
    ),

    %% pos_integer
    ?assertEqual(
        {ok, "42"},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = pos_integer}, 42)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = pos_integer}, 0)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = pos_integer}, -1)
    ),

    %% neg_integer
    ?assertEqual(
        {ok, "-42"},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = neg_integer}, -42)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = neg_integer}, 0)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = neg_integer}, 42)
    ),

    ok.

%% Test to_string/3 function - Range types
to_string_range_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    Range =
        #sp_range{
            type = integer,
            lower_bound = 1,
            upper_bound = 10
        },

    %% Valid values in range
    ?assertEqual({ok, "1"}, spectra_test_util:to_string(TypeInfo, Range, 1)),
    ?assertEqual({ok, "5"}, spectra_test_util:to_string(TypeInfo, Range, 5)),
    ?assertEqual({ok, "10"}, spectra_test_util:to_string(TypeInfo, Range, 10)),

    %% Invalid values outside range
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, Range, 0)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, Range, 11)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, Range, -5)
    ),

    %% Invalid non-integer values
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, Range, "5")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, Range, 5.5)
    ),

    ok.

%% Test to_string/3 function - Literal types
to_string_literal_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Literal atom
    AtomLiteral = #sp_literal{value = hello},
    ?assertEqual({ok, "hello"}, spectra_test_util:to_string(TypeInfo, AtomLiteral, hello)),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, AtomLiteral, world)
    ),

    %% Literal integer
    IntegerLiteral = #sp_literal{value = 42},
    ?assertEqual({ok, "42"}, spectra_test_util:to_string(TypeInfo, IntegerLiteral, 42)),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, IntegerLiteral, 43)
    ),

    %% Literal boolean
    BooleanLiteralTrue = #sp_literal{value = true},
    ?assertEqual(
        {ok, "true"},
        spectra_test_util:to_string(TypeInfo, BooleanLiteralTrue, true)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, BooleanLiteralTrue, false)
    ),

    BooleanLiteralFalse = #sp_literal{value = false},
    ?assertEqual(
        {ok, "false"},
        spectra_test_util:to_string(TypeInfo, BooleanLiteralFalse, false)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, BooleanLiteralFalse, true)
    ),

    ok.

%% Test to_string/3 function - Union types
to_string_union_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Simple union: integer | boolean
    Union =
        #sp_union{types = [#sp_simple_type{type = integer}, #sp_simple_type{type = boolean}]},
    ?assertEqual({ok, "42"}, spectra_test_util:to_string(TypeInfo, Union, 42)),
    ?assertEqual({ok, "true"}, spectra_test_util:to_string(TypeInfo, Union, true)),
    ?assertEqual({ok, "false"}, spectra_test_util:to_string(TypeInfo, Union, false)),
    ?assertMatch(
        {error, [#sp_error{type = no_match}]},
        spectra_test_util:to_string(TypeInfo, Union, "not_matching")
    ),

    %% Complex union with literals: 1 | 2 | true | false
    ComplexUnion =
        #sp_union{
            types =
                [
                    #sp_literal{value = 1},
                    #sp_literal{value = 2},
                    #sp_literal{value = true},
                    #sp_literal{value = false}
                ]
        },
    ?assertEqual({ok, "1"}, spectra_test_util:to_string(TypeInfo, ComplexUnion, 1)),
    ?assertEqual({ok, "2"}, spectra_test_util:to_string(TypeInfo, ComplexUnion, 2)),
    ?assertEqual({ok, "true"}, spectra_test_util:to_string(TypeInfo, ComplexUnion, true)),
    ?assertEqual({ok, "false"}, spectra_test_util:to_string(TypeInfo, ComplexUnion, false)),
    ?assertMatch(
        {error, [#sp_error{type = no_match}]},
        spectra_test_util:to_string(TypeInfo, ComplexUnion, 3)
    ),
    ?assertMatch(
        {error, [#sp_error{type = no_match}]},
        spectra_test_util:to_string(TypeInfo, ComplexUnion, "maybe")
    ),

    ok.

to_string_union_error_accumulation_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    Union = #sp_union{types = [#sp_simple_type{type = integer}, #sp_simple_type{type = boolean}]},
    {error, [#sp_error{type = no_match, ctx = Ctx}]} = spectra_test_util:to_string(
        TypeInfo,
        Union,
        "not_matching"
    ),
    ActualErrors = [
        {Type, [#sp_error{type = ErrorType} || #sp_error{type = ErrorType} <- Errors]}
     || {Type, Errors} <- maps:get(errors, Ctx)
    ],
    ?assertEqual(
        [
            {#sp_simple_type{type = boolean}, [#sp_error{type = type_mismatch}]},
            {#sp_simple_type{type = integer}, [#sp_error{type = type_mismatch}]}
        ],
        ActualErrors
    ),
    ok.

%% Test to_string/3 function - Type references
to_string_type_reference_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Test various type references from the module
    ?assertEqual(
        {ok, "42"}, spectra_test_util:to_string(TypeInfo, resolve_type(TypeInfo, my_integer, 0), 42)
    ),
    ?assertEqual(
        {ok, "3.14000000000000012434e+00"},
        spectra_test_util:to_string(TypeInfo, resolve_type(TypeInfo, my_float, 0), 3.14)
    ),
    ?assertEqual(
        {ok, "42"}, spectra_test_util:to_string(TypeInfo, resolve_type(TypeInfo, my_number, 0), 42)
    ),
    ?assertEqual(
        {ok, "3.14000000000000012434e+00"},
        spectra_test_util:to_string(TypeInfo, resolve_type(TypeInfo, my_number, 0), 3.14)
    ),
    ?assertEqual(
        {ok, "true"},
        spectra_test_util:to_string(TypeInfo, resolve_type(TypeInfo, my_boolean, 0), true)
    ),
    ?assertEqual(
        {ok, "hello"},
        spectra_test_util:to_string(TypeInfo, resolve_type(TypeInfo, my_atom, 0), hello)
    ),
    ?assertEqual(
        {ok, "test"},
        spectra_test_util:to_string(TypeInfo, resolve_type(TypeInfo, my_string, 0), "test")
    ),
    ?assertEqual(
        {ok, "test"},
        spectra_test_util:to_string(TypeInfo, resolve_type(TypeInfo, my_binary, 0), <<"test">>)
    ),

    %% Test range type
    ?assertEqual(
        {ok, "5"}, spectra_test_util:to_string(TypeInfo, resolve_type(TypeInfo, my_range, 0), 5)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, resolve_type(TypeInfo, my_range, 0), 15)
    ),

    %% Test literal types
    ?assertEqual(
        {ok, "hello"},
        spectra_test_util:to_string(TypeInfo, resolve_type(TypeInfo, my_literal_atom, 0), hello)
    ),
    ?assertEqual(
        {ok, "42"},
        spectra_test_util:to_string(TypeInfo, resolve_type(TypeInfo, my_literal_integer, 0), 42)
    ),
    ?assertEqual(
        {ok, "true"},
        spectra_test_util:to_string(TypeInfo, resolve_type(TypeInfo, my_literal_boolean, 0), true)
    ),

    %% Test union types
    ?assertEqual(
        {ok, "42"}, spectra_test_util:to_string(TypeInfo, resolve_type(TypeInfo, my_union, 0), 42)
    ),
    ?assertEqual(
        {ok, "true"},
        spectra_test_util:to_string(TypeInfo, resolve_type(TypeInfo, my_union, 0), true)
    ),
    ?assertEqual(
        {ok, "1"},
        spectra_test_util:to_string(TypeInfo, resolve_type(TypeInfo, my_complex_union, 0), 1)
    ),
    ?assertEqual(
        {ok, "false"},
        spectra_test_util:to_string(TypeInfo, resolve_type(TypeInfo, my_complex_union, 0), false)
    ),

    ok.

%% Test to_string/3 function - Unsupported types
to_string_unsupported_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Record types are not supported for string conversion
    ?assertError(
        {type_not_supported, _},
        spectra_test_util:to_string(
            TypeInfo, #sp_rec{name = some_record, fields = [], arity = 0}, some_value
        )
    ),

    %% Unsupported simple types should error
    ?assertError(
        {type_not_supported, _},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = pid}, self())
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = port}, test)
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = reference}, test)
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = bitstring}, test)
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_test_util:to_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_bitstring},
            test
        )
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = none}, test)
    ),

    %% Unknown type should give type mismatch error
    UnknownType = #sp_tuple{fields = any},
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, UnknownType, some_value)
    ),

    ok.

%% Test from_string/3 with invalid input data type (not a string)
from_string_invalid_input_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Test that passing non-string input returns proper error
    %% Passing binary instead of string
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(
            TypeInfo,
            #sp_simple_type{type = string},
            <<"binary">>
        )
    ),

    %% Passing integer instead of string
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = integer}, 42)
    ),

    %% Passing atom instead of string
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = atom}, hello)
    ),

    ok.

%% Test to_string/3 with invalid input data type
to_string_invalid_input_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% These should fail with type_mismatch because the input doesn't match the expected type
    %% Passing binary when expecting to convert from string
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(
            TypeInfo,
            #sp_simple_type{type = string},
            <<"binary">>
        )
    ),

    %% Passing string when expecting to convert from binary
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = binary}, "string")
    ),

    %% Passing list when expecting integer
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = integer}, [1, 2, 3])
    ),

    %% Passing string when expecting atom
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = atom}, "hello")
    ),

    ok.

%% Test that a codec returning `continue` for a local type falls back to
%% structural decoding rather than returning {error, type_mismatch}.
%% codec_string_continue_module has a `name() :: binary()` type and a codec
%% that always returns `continue` for every callback.
string_codec_continue_decode_test() ->
    %% Without a codec the structural path gives {ok, <<"hello">>}.
    %% With a codec that returns `continue`, the result must be identical.
    ?assertEqual(
        {ok, <<"hello">>},
        spectra:decode(string, codec_string_continue_module, name, "hello")
    ).

string_codec_continue_encode_test() ->
    %% Without a codec the structural path gives {ok, "hello"}.
    %% With a codec that returns `continue`, the result must be identical.
    ?assertEqual(
        {ok, "hello"},
        spectra:encode(string, codec_string_continue_module, name, <<"hello">>)
    ).

%% A codec returning `continue` for a record type must not fall back to
%% structural record decoding (unsupported) — it must raise type_not_supported.
string_codec_record_continue_decode_test() ->
    ?assertError(
        {type_not_supported, _},
        spectra:decode(string, codec_animal_codec, {record, cat}, "anything")
    ).

string_codec_record_continue_encode_test() ->
    ?assertError(
        {type_not_supported, _},
        spectra:encode(string, codec_animal_codec, {record, cat}, any_value)
    ).

%% Problem 1: union members must go through the outer from_string so that
%% codec dispatch happens for each union branch. Here maybe_token() is
%% token() | undefined; token() has a codec registered via app env that
%% converts a string into {token, Binary}. The union branch for
%% token() must invoke the codec, not fall straight to structural decoding.
string_union_codec_decode_test() ->
    application:set_env(
        spectra,
        codecs,
        #{{codec_string_token_module, {type, token, 0}} => codec_string_token_codec}
    ),
    try
        ?assertEqual(
            {ok, {token, <<"abc">>}},
            spectra:decode(string, codec_string_token_module, maybe_token, "abc")
        )
    after
        application:unset_env(spectra, codecs)
    end.

string_union_codec_encode_test() ->
    application:set_env(
        spectra,
        codecs,
        #{{codec_string_token_module, {type, token, 0}} => codec_string_token_codec}
    ),
    try
        ?assertEqual(
            {ok, "abc"},
            spectra:encode(string, codec_string_token_module, maybe_token, {token, <<"abc">>})
        )
    after
        application:unset_env(spectra, codecs)
    end.

%% Binary-typed values are UTF-8 encoded/decoded, so <<"é"/utf8>> (two
%% bytes) becomes the single-codepoint list [233] — not the latin1
%% byte list [195, 169].
string_utf8_binary_roundtrip_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    Bin = <<"é"/utf8>>,
    ?assertEqual(
        {ok, [16#E9]},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = binary}, Bin)
    ),
    ?assertEqual(
        {ok, Bin},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = binary}, [16#E9])
    ).

string_utf8_nonempty_binary_roundtrip_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    Bin = <<"é"/utf8>>,
    ?assertEqual(
        {ok, [16#E9]},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = nonempty_binary}, Bin)
    ),
    ?assertEqual(
        {ok, Bin},
        spectra_test_util:from_string(
            TypeInfo, #sp_simple_type{type = nonempty_binary}, [16#E9]
        )
    ).

string_invalid_utf8_binary_reports_context_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    InvalidBin = <<255, 254>>,
    {error, [Err]} =
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = binary}, InvalidBin),
    ?assertMatch(
        #sp_error{ctx = #{reason := invalid_utf8}},
        Err
    ).

string_codepoint_out_of_range_reports_context_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    %% 0x110000 is above the max valid Unicode codepoint (0x10FFFF).
    InvalidString = [16#110000],
    {error, [Err]} =
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = binary}, InvalidString),
    ?assertMatch(
        #sp_error{ctx = #{reason := invalid_codepoints}},
        Err
    ).

%% Codepoint lists roundtrip unchanged through the string type. Locks
%% in the contract that the string path performs no transformation so
%% non-ASCII codepoints survive intact.
string_type_non_ascii_roundtrip_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    %% é, λ, 😀
    Codepoints = [16#E9, 16#3BB, 16#1F600],
    ?assertEqual(
        {ok, Codepoints},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = string}, Codepoints)
    ),
    ?assertEqual(
        {ok, Codepoints},
        spectra_test_util:from_string(TypeInfo, #sp_simple_type{type = string}, Codepoints)
    ).

%% Encode always validates Unicode, so an iolist containing an invalid
%% UTF-8 binary is rejected.
string_type_invalid_unicode_rejected_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    InvalidIolist = [16#41, <<255, 254>>],
    ?assertMatch(
        {error, [#sp_error{}]},
        spectra_test_util:to_string(TypeInfo, #sp_simple_type{type = string}, InvalidIolist)
    ).

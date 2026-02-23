-module(typed_map_field_schema_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").

-compile(nowarn_unused_type).

%% Test types with typed map fields - string-like keys (should work with JSON schema)
-type string_key_map_assoc() :: #{string() => integer()}.
-type string_key_map_exact() :: #{string() := integer()}.
-type binary_key_map_assoc() :: #{binary() => atom()}.
-type binary_key_map_exact() :: #{binary() := atom()}.
-type atom_key_map_assoc() :: #{atom() => binary()}.
-type atom_key_map_exact() :: #{atom() := binary()}.

%% Test types with typed map fields - non-string keys (should fail with JSON schema)
-type integer_key_map_assoc() :: #{integer() => binary()}.
-type integer_key_map_exact() :: #{integer() := binary()}.
-type tuple_key_map_assoc() :: #{{atom(), integer()} => binary()}.
-type tuple_key_map_exact() :: #{{atom(), integer()} := binary()}.
-type float_key_map_assoc() :: #{float() => integer()}.
-type float_key_map_exact() :: #{float() := integer()}.

%%====================================================================
%% JSON Schema Tests - String-like keys should succeed
%%====================================================================

schema_string_key_map_assoc_test() ->
    Schema = spectra_json_schema:to_schema(?MODULE, {type, string_key_map_assoc, 0}),
    ?assertMatch(
        #{
            type := <<"object">>,
            additionalProperties := true
        },
        Schema
    ),
    %% No specific properties since it's a typed field with variable keys
    ?assertEqual(
        false,
        maps:is_key(properties, Schema) andalso maps:get(properties, Schema) =/= #{}
    ),
    %% No required fields for assoc (=>)
    ?assertEqual(false, maps:is_key(required, Schema)).

schema_string_key_map_exact_test() ->
    Schema = spectra_json_schema:to_schema(?MODULE, {type, string_key_map_exact, 0}),
    ?assertMatch(
        #{
            type := <<"object">>,
            additionalProperties := true
        },
        Schema
    ),
    %% No specific properties since it's a typed field with variable keys
    ?assertEqual(
        false,
        maps:is_key(properties, Schema) andalso maps:get(properties, Schema) =/= #{}
    ),
    %% No required fields even for exact (:=) with typed fields
    ?assertEqual(false, maps:is_key(required, Schema)).

schema_binary_key_map_assoc_test() ->
    Schema = spectra_json_schema:to_schema(?MODULE, {type, binary_key_map_assoc, 0}),
    ?assertMatch(
        #{
            type := <<"object">>,
            additionalProperties := true
        },
        Schema
    ).

schema_binary_key_map_exact_test() ->
    Schema = spectra_json_schema:to_schema(?MODULE, {type, binary_key_map_exact, 0}),
    ?assertMatch(
        #{
            type := <<"object">>,
            additionalProperties := true
        },
        Schema
    ).

schema_atom_key_map_assoc_test() ->
    Schema = spectra_json_schema:to_schema(?MODULE, {type, atom_key_map_assoc, 0}),
    ?assertMatch(
        #{
            type := <<"object">>,
            additionalProperties := true
        },
        Schema
    ).

schema_atom_key_map_exact_test() ->
    Schema = spectra_json_schema:to_schema(?MODULE, {type, atom_key_map_exact, 0}),
    ?assertMatch(
        #{
            type := <<"object">>,
            additionalProperties := true
        },
        Schema
    ).

%%====================================================================
%% JSON Schema Tests - Non-string keys should fail
%%====================================================================

schema_integer_key_map_assoc_test() ->
    ?assertError(
        {type_not_supported, _},
        spectra_json_schema:to_schema(?MODULE, {type, integer_key_map_assoc, 0})
    ).

schema_integer_key_map_exact_test() ->
    ?assertError(
        {type_not_supported, _},
        spectra_json_schema:to_schema(?MODULE, {type, integer_key_map_exact, 0})
    ).

schema_tuple_key_map_assoc_test() ->
    ?assertError(
        {type_not_supported, _},
        spectra_json_schema:to_schema(?MODULE, {type, tuple_key_map_assoc, 0})
    ).

schema_tuple_key_map_exact_test() ->
    ?assertError(
        {type_not_supported, _},
        spectra_json_schema:to_schema(?MODULE, {type, tuple_key_map_exact, 0})
    ).

schema_float_key_map_assoc_test() ->
    ?assertError(
        {type_not_supported, _},
        spectra_json_schema:to_schema(?MODULE, {type, float_key_map_assoc, 0})
    ).

schema_float_key_map_exact_test() ->
    ?assertError(
        {type_not_supported, _},
        spectra_json_schema:to_schema(?MODULE, {type, float_key_map_exact, 0})
    ).

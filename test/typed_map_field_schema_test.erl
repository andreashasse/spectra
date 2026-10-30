-module(typed_map_field_schema_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").

-compile(nowarn_unused_type).

-define(SCHEMA_VERSION, <<"https://json-schema.org/draft/2020-12/schema">>).

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

%% Typed map fields whose key is a remote type
-type local_remote_key_map() :: #{other:label() => binary()}.
-type otp_remote_key_map() :: #{unicode:unicode_binary() => binary()}.

-type any_value_map() :: #{binary() => term()}.
-type multi_typed_map() :: #{atom() => integer(), binary() => [binary()]}.
-type labels() :: #{color | size => binary()}.
-type tags() :: #{name := binary(), binary() => [binary()]}.

%%====================================================================
%% JSON Schema Tests - String-like keys should succeed
%%====================================================================

schema_string_key_map_assoc_test() ->
    Schema = spectra:schema(json_schema, ?MODULE, {type, string_key_map_assoc, 0}, [pre_encoded]),
    ?assertMatch(
        #{
            type := <<"object">>,
            additionalProperties := #{type := <<"integer">>}
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
    Schema = spectra:schema(json_schema, ?MODULE, {type, string_key_map_exact, 0}, [pre_encoded]),
    ?assertMatch(
        #{
            type := <<"object">>,
            additionalProperties := #{type := <<"integer">>}
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
    Schema = spectra:schema(json_schema, ?MODULE, {type, binary_key_map_assoc, 0}, [pre_encoded]),
    ?assertMatch(
        #{
            type := <<"object">>,
            additionalProperties := #{type := <<"string">>}
        },
        Schema
    ).

schema_binary_key_map_exact_test() ->
    Schema = spectra:schema(json_schema, ?MODULE, {type, binary_key_map_exact, 0}, [pre_encoded]),
    ?assertMatch(
        #{
            type := <<"object">>,
            additionalProperties := #{type := <<"string">>}
        },
        Schema
    ).

schema_atom_key_map_assoc_test() ->
    Schema = spectra:schema(json_schema, ?MODULE, {type, atom_key_map_assoc, 0}, [pre_encoded]),
    ?assertMatch(
        #{
            type := <<"object">>,
            additionalProperties := #{type := <<"string">>}
        },
        Schema
    ).

schema_atom_key_map_exact_test() ->
    Schema = spectra:schema(json_schema, ?MODULE, {type, atom_key_map_exact, 0}, [pre_encoded]),
    ?assertMatch(
        #{
            type := <<"object">>,
            additionalProperties := #{type := <<"string">>}
        },
        Schema
    ).

%%====================================================================
%% JSON Schema Tests - Non-string keys should fail
%%====================================================================

schema_integer_key_map_assoc_test() ->
    ?assertError(
        {type_not_supported, _},
        spectra:schema(json_schema, ?MODULE, {type, integer_key_map_assoc, 0}, [pre_encoded])
    ).

schema_integer_key_map_exact_test() ->
    ?assertError(
        {type_not_supported, _},
        spectra:schema(json_schema, ?MODULE, {type, integer_key_map_exact, 0}, [pre_encoded])
    ).

schema_tuple_key_map_assoc_test() ->
    ?assertError(
        {type_not_supported, _},
        spectra:schema(json_schema, ?MODULE, {type, tuple_key_map_assoc, 0}, [pre_encoded])
    ).

schema_tuple_key_map_exact_test() ->
    ?assertError(
        {type_not_supported, _},
        spectra:schema(json_schema, ?MODULE, {type, tuple_key_map_exact, 0}, [pre_encoded])
    ).

schema_float_key_map_assoc_test() ->
    ?assertError(
        {type_not_supported, _},
        spectra:schema(json_schema, ?MODULE, {type, float_key_map_assoc, 0}, [pre_encoded])
    ).

schema_float_key_map_exact_test() ->
    ?assertError(
        {type_not_supported, _},
        spectra:schema(json_schema, ?MODULE, {type, float_key_map_exact, 0}, [pre_encoded])
    ).

%%====================================================================
%% JSON Schema Tests - Remote string key types
%%====================================================================

schema_remote_key_map_test() ->
    ?assertMatch(
        #{type := <<"object">>},
        spectra:schema(json_schema, ?MODULE, {type, local_remote_key_map, 0}, [pre_encoded])
    ).

schema_otp_remote_key_map_test() ->
    ?assertMatch(
        #{type := <<"object">>},
        spectra:schema(json_schema, ?MODULE, {type, otp_remote_key_map, 0}, [pre_encoded])
    ).

%%====================================================================
%% JSON Schema Tests - Typed map value schemas
%%====================================================================

schema_any_value_map_test() ->
    Schema = spectra:schema(json_schema, ?MODULE, {type, any_value_map, 0}, [pre_encoded]),
    ?assertEqual(
        #{'$schema' => ?SCHEMA_VERSION, type => <<"object">>, additionalProperties => true}, Schema
    ),
    json_schema_validator_helper:validate_or_skip(Schema).

schema_multi_typed_map_test() ->
    Schema = spectra:schema(json_schema, ?MODULE, {type, multi_typed_map, 0}, [pre_encoded]),
    ?assertEqual(
        #{
            '$schema' => ?SCHEMA_VERSION,
            type => <<"object">>,
            additionalProperties => #{
                anyOf => [
                    #{type => <<"integer">>},
                    #{type => <<"array">>, items => #{type => <<"string">>}}
                ]
            }
        },
        Schema
    ),
    json_schema_validator_helper:validate_or_skip(Schema).

schema_constrained_key_map_test() ->
    Schema = spectra:schema(json_schema, ?MODULE, {type, labels, 0}, [pre_encoded]),
    ?assertEqual(
        #{
            '$schema' => ?SCHEMA_VERSION,
            type => <<"object">>,
            propertyNames => #{type => <<"string">>, enum => [<<"color">>, <<"size">>]},
            additionalProperties => #{type => <<"string">>}
        },
        Schema
    ),
    json_schema_validator_helper:validate_or_skip(Schema).

schema_literal_and_typed_fields_map_test() ->
    Schema = spectra:schema(json_schema, ?MODULE, {type, tags, 0}, [pre_encoded]),
    ?assertEqual(
        #{
            '$schema' => ?SCHEMA_VERSION,
            type => <<"object">>,
            properties => #{<<"name">> => #{type => <<"string">>}},
            required => [<<"name">>],
            additionalProperties => #{type => <<"array">>, items => #{type => <<"string">>}}
        },
        Schema
    ),
    json_schema_validator_helper:validate_or_skip(Schema).

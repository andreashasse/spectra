-module(json_schema_validator_test).

-include_lib("eunit/include/eunit.hrl").

%% Test that the Python validator correctly rejects invalid schemas

%% Helper function to validate a schema and assert on the result
assert_validation(Schema, ShouldPass, ExpectedErrorSubstring) ->
    Result = json_schema_validator_helper:validate_schema_2020_12(Schema),
    case Result of
        {skip, _Reason} ->
            %% uv not installed, test is skipped
            ok;
        {error, {validation_failed, Output}} when not ShouldPass ->
            %% Validation should fail - check error message if provided
            case ExpectedErrorSubstring of
                undefined ->
                    ok;
                Substring ->
                    ?assert(string:find(Output, Substring) =/= nomatch)
            end;
        {error, {validation_failed, Output}} when ShouldPass ->
            %% If validation failed but should have passed, fail the test
            ?assert(false, io_lib:format("Expected validation to pass, but got: ~s", [Output]));
        ok when ShouldPass ->
            %% Validation passed as expected
            ok;
        ok when not ShouldPass ->
            %% If validation passed but should have failed, fail the test
            ?assert(false, "Expected validation to fail")
    end.

%% Test 1: Schema missing $schema field should be rejected
missing_schema_field_test() ->
    InvalidSchema = #{
        type => <<"string">>,
        minLength => 1
    },
    assert_validation(InvalidSchema, false, "missing $schema field").

%% Test 2: Schema with wrong version (draft-07) should be rejected
wrong_schema_version_test() ->
    InvalidSchema = #{
        <<"$schema">> => <<"http://json-schema.org/draft-07/schema#">>,
        type => <<"string">>
    },
    assert_validation(InvalidSchema, false, "does not declare JSON Schema 2020-12").

%% Test 3: Schema with invalid structure should be rejected
%% (e.g., type field has invalid value)
invalid_type_value_test() ->
    InvalidSchema = #{
        <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
        type => <<"not_a_valid_type">>
    },
    assert_validation(InvalidSchema, false, "Schema validation failed").

%% Test 4: Schema with invalid keyword combination
%% (e.g., minLength applied to integer type)
%% Note: This is actually allowed by JSON Schema spec - minLength is just ignored for integers
invalid_keyword_combination_test() ->
    Schema = #{
        <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
        type => <<"integer">>,
        minLength => 5
    },
    %% This should pass - the spec allows it even though it doesn't make semantic sense
    assert_validation(Schema, true, undefined).

%% Test 5: Valid schema should pass
valid_schema_test() ->
    ValidSchema = #{
        <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
        type => <<"string">>,
        minLength => 1
    },
    assert_validation(ValidSchema, true, undefined).

%% Test 6: Schema with 2020-12 specific feature (prefixItems) should pass
%% prefixItems is new in 2020-12, replacing the array form of items from draft-07
schema_2020_12_prefix_items_test() ->
    Schema = #{
        <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
        type => <<"array">>,
        prefixItems => [
            #{type => <<"number">>},
            #{type => <<"string">>},
            #{type => <<"boolean">>}
        ]
    },
    assert_validation(Schema, true, undefined).

%% Test 7: Schema claiming to be draft-07 but using 2020-12 features should be rejected
draft_07_with_2020_12_features_test() ->
    Schema = #{
        <<"$schema">> => <<"http://json-schema.org/draft-07/schema#">>,
        type => <<"array">>,
        prefixItems => [
            #{type => <<"number">>},
            #{type => <<"string">>}
        ]
    },
    %% This should be rejected because we only accept 2020-12 schemas
    assert_validation(Schema, false, "does not declare JSON Schema 2020-12").

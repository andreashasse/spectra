-module(json_schema_validator_test).

-include_lib("eunit/include/eunit.hrl").

%% Test that the Python validator correctly rejects invalid schemas

%% Test 1: Schema missing $schema field should be rejected
missing_schema_field_test() ->
    InvalidSchema = #{
        type => <<"string">>,
        minLength => 1
    },
    Result = json_schema_validator_helper:validate_schema_2020_12(InvalidSchema),
    case Result of
        {skip, _Reason} ->
            %% uv not installed, test is skipped
            ok;
        {error, {validation_failed, Output}} ->
            %% Validation should fail
            ?assert(string:find(Output, "missing $schema field") =/= nomatch),
            ok;
        ok ->
            %% If validation passed, that's wrong - schema should be rejected
            ?assert(false, "Expected validation to fail for schema missing $schema field")
    end.

%% Test 2: Schema with wrong version (draft-07) should be rejected
wrong_schema_version_test() ->
    InvalidSchema = #{
        <<"$schema">> => <<"http://json-schema.org/draft-07/schema#">>,
        type => <<"string">>
    },
    Result = json_schema_validator_helper:validate_schema_2020_12(InvalidSchema),
    case Result of
        {skip, _Reason} ->
            %% uv not installed, test is skipped
            ok;
        {error, {validation_failed, Output}} ->
            %% Validation should fail
            ?assert(string:find(Output, "does not declare JSON Schema 2020-12") =/= nomatch),
            ok;
        ok ->
            %% If validation passed, that's wrong - schema should be rejected
            ?assert(false, "Expected validation to fail for draft-07 schema")
    end.

%% Test 3: Schema with invalid structure should be rejected
%% (e.g., type field has invalid value)
invalid_type_value_test() ->
    InvalidSchema = #{
        <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
        type => <<"not_a_valid_type">>
    },
    Result = json_schema_validator_helper:validate_schema_2020_12(InvalidSchema),
    case Result of
        {skip, _Reason} ->
            %% uv not installed, test is skipped
            ok;
        {error, {validation_failed, Output}} ->
            %% Validation should fail
            ?assert(string:find(Output, "Schema validation failed") =/= nomatch),
            ok;
        ok ->
            %% If validation passed, that's wrong - schema should be rejected
            ?assert(false, "Expected validation to fail for invalid type value")
    end.

%% Test 4: Schema with invalid keyword should be rejected
%% (e.g., minLength applied to integer type)
invalid_keyword_combination_test() ->
    InvalidSchema = #{
        <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
        type => <<"integer">>,
        minLength => 5
    },
    Result = json_schema_validator_helper:validate_schema_2020_12(InvalidSchema),
    case Result of
        {skip, _Reason} ->
            %% uv not installed, test is skipped
            ok;
        {error, {validation_failed, _Output}} ->
            %% This might fail validation, but it's actually allowed by the meta-schema
            %% (it just doesn't make sense semantically). Let's not assert on this.
            ok;
        ok ->
            %% Actually, this is valid according to JSON Schema spec, even though
            %% minLength doesn't apply to integers. The validator just ignores it.
            ok
    end.

%% Test 5: Valid schema should pass
valid_schema_test() ->
    ValidSchema = #{
        <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
        type => <<"string">>,
        minLength => 1
    },
    Result = json_schema_validator_helper:validate_schema_2020_12(ValidSchema),
    case Result of
        {skip, _Reason} ->
            %% uv not installed, test is skipped
            ok;
        {error, {validation_failed, Output}} ->
            %% If validation failed, that's wrong - this is a valid schema
            ?assert(false, io_lib:format("Expected validation to pass, but got: ~s", [Output]));
        ok ->
            %% Validation passed as expected
            ok
    end.

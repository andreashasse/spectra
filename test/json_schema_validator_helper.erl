-module(json_schema_validator_helper).

-export([validate_schema/1, validate_schema_2020_12/1, assert_validation/3, validate_or_skip/1]).

%% @doc Validate that a schema conforms to JSON Schema 2020-12 using Python validator.
%% This function writes the schema to a temporary file, runs the validation script,
%% and returns the result.
-spec validate_schema_2020_12(map()) -> ok | {skip, string()} | {error, term()}.
validate_schema_2020_12(Schema) ->
    validate_schema(Schema).

%% @doc Validate a schema using the Python validator script.
-spec validate_schema(map()) -> ok | {skip, string()} | {error, term()}.
validate_schema(Schema) ->
    ScriptPath = filename:join([code:priv_dir(spectra), "validate_json_schema.py"]),
    case python_validator_helper:validate_with_python(ScriptPath, Schema) of
        {skip, Reason} ->
            {skip, Reason};
        {ok, Result} ->
            %% Check if validation succeeded
            case string:find(Result, "All schemas are valid") of
                nomatch ->
                    {error, {validation_failed, Result}};
                _ ->
                    ok
            end
    end.

%% @doc Helper function to validate a schema and assert on the result.
%% This is useful for test cases that need to verify schema validation.
-spec assert_validation(map(), boolean(), undefined | string()) -> ok.
assert_validation(Schema, ShouldPass, ExpectedErrorSubstring) ->
    Result = validate_schema_2020_12(Schema),
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
                    case string:find(Output, Substring) of
                        nomatch ->
                            error(
                                {assertion_failed,
                                    {expected_substring_not_found, Substring, Output}}
                            );
                        _ ->
                            ok
                    end
            end;
        {error, {validation_failed, Output}} when ShouldPass ->
            %% If validation failed but should have passed, fail the test
            error({assertion_failed, {expected_validation_to_pass, Output}});
        ok when ShouldPass ->
            %% Validation passed as expected
            ok;
        ok when not ShouldPass ->
            %% If validation passed but should have failed, fail the test
            error({assertion_failed, expected_validation_to_fail})
    end.

%% @doc Validate a schema and skip or fail with error.
%% This is useful for test cases that want to validate schemas but skip if uv is not installed.
%% If validation fails, it raises an error with the validation output.
-spec validate_or_skip(map()) -> ok.
validate_or_skip(Schema) ->
    case validate_schema_2020_12(Schema) of
        ok ->
            ok;
        {skip, Reason} ->
            io:format("Skipping Python validation: ~ts~n", [Reason]),
            ok;
        {error, {validation_failed, Output}} ->
            io:format("Python validation failed:~n~ts~n", [Output]),
            erlang:error({python_validation_failed, Output})
    end.

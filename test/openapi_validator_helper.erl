-module(openapi_validator_helper).

-export([validate_openapi/1, validate_openapi_3_1/1]).

%% @doc Validate that an OpenAPI spec conforms to OpenAPI 3.1 using Python validator.
%% This function writes the spec to a temporary file, runs the validation script,
%% and returns the result.
-spec validate_openapi_3_1(map()) -> ok | {skip, string()} | {error, term()}.
validate_openapi_3_1(Spec) ->
    validate_openapi(Spec).

%% @doc Validate an OpenAPI spec using the Python validator script.
-spec validate_openapi(map()) -> ok | {skip, string()} | {error, term()}.
validate_openapi(Spec) ->
    ScriptPath = filename:join([code:priv_dir(spectra), "validate_openapi.py"]),
    case python_validator_helper:validate_with_python(ScriptPath, Spec) of
        {skip, Reason} ->
            {skip, Reason};
        {ok, Result} ->
            %% Check if validation succeeded
            case string:find(Result, "is a valid OpenAPI 3.1 specification") of
                nomatch ->
                    {error, {validation_failed, Result}};
                _ ->
                    ok
            end
    end.

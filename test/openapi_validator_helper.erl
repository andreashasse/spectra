-module(openapi_validator_helper).

-export([validate_openapi/1, validate_openapi_3_1/1]).

%% @doc Validate that an OpenAPI spec conforms to OpenAPI 3.1 using Python validator.
%% This function writes the spec to a temporary file, runs the validation script,
%% and returns the result.
-spec validate_openapi_3_1(map()) -> ok | {error, term()}.
validate_openapi_3_1(Spec) ->
    validate_openapi(Spec).

%% @doc Validate an OpenAPI spec using the Python validator script.
-spec validate_openapi(map()) -> ok | {error, term()}.
validate_openapi(Spec) ->
    %% Check if uv is available
    case os:find_executable("uv") of
        false ->
            %% uv not found, skip validation
            {skip, "uv not installed, skipping OpenAPI 3.1 validation"};
        UvPath when is_list(UvPath) ->
            %% Create temporary file
            TempFile = temp_filename(),
            try
                %% Write spec to temporary file
                JsonBinary = json:encode(Spec),
                ok = file:write_file(TempFile, JsonBinary),

                %% Run validation script
                ScriptPath = filename:join([code:priv_dir(spectra), "validate_openapi.py"]),
                Command = lists:flatten(
                    io_lib:format("~s run ~s ~s", [UvPath, ScriptPath, TempFile])
                ),

                case os:cmd(Command) of
                    Result ->
                        %% Check if validation succeeded
                        case string:find(Result, "is a valid OpenAPI 3.1 specification") of
                            nomatch ->
                                {error, {validation_failed, Result}};
                            _ ->
                                ok
                        end
                end
            after
                %% Clean up temporary file
                file:delete(TempFile)
            end
    end.

%% Generate a temporary filename
-spec temp_filename() -> string().
temp_filename() ->
    TempDir =
        case os:type() of
            {unix, _} -> "/tmp";
            {win32, _} -> os:getenv("TEMP", "C:\\Temp")
        end,
    Timestamp = erlang:system_time(microsecond),
    Filename = lists:flatten(io_lib:format("spectra_openapi_~p.json", [Timestamp])),
    filename:join(TempDir, Filename).

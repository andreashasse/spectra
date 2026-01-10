-module(json_schema_validator_helper).

-export([validate_schema/1, validate_schema_2020_12/1]).

%% @doc Validate that a schema conforms to JSON Schema 2020-12 using Python validator.
%% This function writes the schema to a temporary file, runs the validation script,
%% and returns the result.
-spec validate_schema_2020_12(map()) -> ok | {error, term()}.
validate_schema_2020_12(Schema) ->
    validate_schema(Schema).

%% @doc Validate a schema using the Python validator script.
-spec validate_schema(map()) -> ok | {error, term()}.
validate_schema(Schema) ->
    %% Check if uv is available
    case os:find_executable("uv") of
        false ->
            %% uv not found, skip validation
            {skip, "uv not installed, skipping JSON Schema 2020-12 validation"};
        UvPath when is_list(UvPath) ->
            %% Create temporary file
            TempFile = temp_filename(),
            try
                %% Write schema to temporary file
                JsonBinary = json:encode(Schema),
                ok = file:write_file(TempFile, JsonBinary),

                %% Run validation script
                ScriptPath = filename:join([code:priv_dir(spectra), "validate_json_schema.py"]),
                Command = lists:flatten(
                    io_lib:format("~s run ~s ~s", [UvPath, ScriptPath, TempFile])
                ),

                case os:cmd(Command) of
                    Result ->
                        %% Check if validation succeeded
                        case string:find(Result, "All schemas are valid") of
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
    Filename = lists:flatten(io_lib:format("spectra_schema_~p.json", [Timestamp])),
    filename:join(TempDir, Filename).

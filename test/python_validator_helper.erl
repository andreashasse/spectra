-module(python_validator_helper).

-export([validate_with_python/2]).

%% @doc Generic function to validate data using a Python script via uv.
%% Takes a Python script path and data to validate.
%% Returns ok if the success pattern is found in the output, error otherwise.
-spec validate_with_python(ScriptPath :: string(), Data :: map()) ->
    ok | {skip, string()} | {error, term()}.
validate_with_python(ScriptPath, Data) ->
    %% Check if uv is available
    case os:find_executable("uv") of
        false ->
            %% uv not found, skip validation
            {skip, "uv not installed, skipping validation"};
        UvPath when is_list(UvPath) ->
            %% Create temporary file
            TempFile = temp_filename(),
            try
                %% Write data to temporary file
                JsonBinary = json:encode(Data),
                ok = file:write_file(TempFile, JsonBinary),

                %% Run validation script
                Command = lists:flatten(
                    io_lib:format("~s run ~s ~s", [UvPath, ScriptPath, TempFile])
                ),

                Result = os:cmd(Command),
                %% Return the result - let the caller decide what constitutes success
                {ok, Result}
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
    Filename = lists:flatten(io_lib:format("spectra_validation_~p.json", [Timestamp])),
    filename:join(TempDir, Filename).

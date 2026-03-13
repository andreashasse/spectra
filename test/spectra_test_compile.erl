-module(spectra_test_compile).

%% Shared helpers for tests that compile Erlang source strings at runtime.

-compile(nowarn_missing_spec).

-export([compile_module/1, parse_module/1, temp_beam_path/1]).

%% Compile a source string to a beam binary.
-spec compile_module(Code :: string()) ->
    {ok, module(), binary()} | {error, term()}.
compile_module(Code) ->
    compile:forms(parse_module(Code), [binary, return_errors, debug_info]).

%% Parse a source string into a list of abstract forms.
-spec parse_module(Code :: string()) -> [erl_parse:abstract_form()].
parse_module(Code) ->
    Lines = string:split(Code, "\n", all),
    {Forms, _} = lists:foldl(
        fun(Line, {Acc, LineNum}) ->
            case string:trim(Line) of
                "" ->
                    {Acc, LineNum + 1};
                TrimmedLine ->
                    {ok, Tokens, _} = erl_scan:string(TrimmedLine ++ "\n", LineNum),
                    {ok, Form} = erl_parse:parse_form(Tokens),
                    {[Form | Acc], LineNum + 1}
            end
        end,
        {[], 1},
        Lines
    ),
    lists:reverse(Forms) ++ [{eof, 999}].

%% Return a unique temp path for a .beam file.
-spec temp_beam_path(Name :: string()) -> file:filename().
temp_beam_path(Name) ->
    TempDir = filename:basedir(user_cache, "spectra_tests"),
    ok = filelib:ensure_dir(filename:join(TempDir, "dummy")),
    Unique = integer_to_list(erlang:unique_integer([positive])),
    filename:join(TempDir, Name ++ "_" ++ Unique ++ ".beam").

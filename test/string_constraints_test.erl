-module(string_constraints_test).

-compile(nowarn_missing_spec).

-include_lib("eunit/include/eunit.hrl").

%% -----------------------------------------------------------------------
%% JSON Schema generation — constraints appear in output
%% -----------------------------------------------------------------------

schema_pattern_test() ->
    Schema = spectra:schema(json_schema, string_constraints_module, {type, lowercase_binary, 0}),
    ?assertMatch(
        #{<<"type">> := <<"string">>, <<"pattern">> := <<"^[a-z]+$">>},
        json:decode(iolist_to_binary(Schema))
    ).

schema_min_max_length_test() ->
    Schema = spectra:schema(json_schema, string_constraints_module, {type, bounded_binary, 0}),
    ?assertMatch(
        #{<<"type">> := <<"string">>, <<"minLength">> := 2, <<"maxLength">> := 5},
        json:decode(iolist_to_binary(Schema))
    ).

schema_format_test() ->
    Schema = spectra:schema(json_schema, string_constraints_module, {type, date_binary, 0}),
    ?assertMatch(
        #{<<"type">> := <<"string">>, <<"format">> := <<"date">>},
        json:decode(iolist_to_binary(Schema))
    ).

schema_nonempty_min_length_overrides_baseline_test() ->
    Schema = spectra:schema(json_schema, string_constraints_module, {type, long_nonempty, 0}),
    %% min_length => 3 from params overrides the nonempty_binary baseline of 1
    ?assertMatch(
        #{<<"type">> := <<"string">>, <<"minLength">> := 3},
        json:decode(iolist_to_binary(Schema))
    ).

schema_all_constraints_test() ->
    Schema = spectra:schema(json_schema, string_constraints_module, {type, full_constraints, 0}),
    ?assertMatch(
        #{
            <<"type">> := <<"string">>,
            <<"pattern">> := <<"^[a-z]+$">>,
            <<"minLength">> := 2,
            <<"maxLength">> := 10,
            <<"format">> := <<"identifier">>
        },
        json:decode(iolist_to_binary(Schema))
    ).

%% -----------------------------------------------------------------------
%% Decode — pattern validation
%% -----------------------------------------------------------------------

decode_pattern_match_test() ->
    ?assertEqual(
        {ok, <<"hello">>},
        spectra:decode(json, string_constraints_module, {type, lowercase_binary, 0}, <<"hello">>, [
            pre_decoded
        ])
    ).

decode_pattern_no_match_test() ->
    ?assertMatch(
        {error, [_]},
        spectra:decode(json, string_constraints_module, {type, lowercase_binary, 0}, <<"Hello">>, [
            pre_decoded
        ])
    ).

%% -----------------------------------------------------------------------
%% Decode — min/max length validation
%% -----------------------------------------------------------------------

decode_within_bounds_test() ->
    ?assertEqual(
        {ok, <<"abc">>},
        spectra:decode(json, string_constraints_module, {type, bounded_binary, 0}, <<"abc">>, [
            pre_decoded
        ])
    ).

decode_too_short_test() ->
    ?assertMatch(
        {error, [_]},
        spectra:decode(json, string_constraints_module, {type, bounded_binary, 0}, <<"a">>, [
            pre_decoded
        ])
    ).

decode_too_long_test() ->
    ?assertMatch(
        {error, [_]},
        spectra:decode(
            json, string_constraints_module, {type, bounded_binary, 0}, <<"toolong">>, [pre_decoded]
        )
    ).

%% -----------------------------------------------------------------------
%% Decode — string() type (codepoint length)
%% -----------------------------------------------------------------------

decode_digit_string_match_test() ->
    %% "123" in JSON becomes the string "123" after from_json; pattern ^\d+$ should match
    ?assertMatch(
        {ok, _},
        spectra:decode(json, string_constraints_module, {type, digit_string, 0}, <<"123">>, [
            pre_decoded
        ])
    ).

decode_digit_string_no_match_test() ->
    ?assertMatch(
        {error, [_]},
        spectra:decode(json, string_constraints_module, {type, digit_string, 0}, <<"abc">>, [
            pre_decoded
        ])
    ).

%% -----------------------------------------------------------------------
%% Decode — format is schema-only, no runtime effect
%% -----------------------------------------------------------------------

decode_format_no_runtime_effect_test() ->
    %% "not-a-date" would fail a real date validator, but format is schema-only
    ?assertEqual(
        {ok, <<"not-a-date">>},
        spectra:decode(json, string_constraints_module, {type, date_binary, 0}, <<"not-a-date">>, [
            pre_decoded
        ])
    ).

%% -----------------------------------------------------------------------
%% Unknown constraint key crashes
%% -----------------------------------------------------------------------

unknown_constraint_key_crashes_test() ->
    Code =
        "-module(bad_constraints_module).\n"
        "-compile([nowarn_unused_type]).\n"
        "-spectra(#{type_parameters => #{unknown_key => 1}}).\n"
        "-type my_binary() :: binary().\n",
    {ok, bad_constraints_module, BeamBinary} = compile:forms(
        parse_module(Code), [binary, return_errors, debug_info]
    ),
    TempFile = temp_beam_path("bad_constraints_module"),
    ok = file:write_file(TempFile, BeamBinary),
    try
        %% Loading the module succeeds; error occurs at decode/schema time
        {module, bad_constraints_module} = code:load_binary(
            bad_constraints_module, TempFile, BeamBinary
        ),
        TypeInfo = spectra_abstract_code:types_in_module(bad_constraints_module),
        {ok, Type} = spectra_type_info:find_type(TypeInfo, my_binary, 0),
        ?assertError(
            {invalid_string_constraint, unknown_key, 1},
            spectra_json:from_json(TypeInfo, Type, <<"hello">>)
        )
    after
        code:purge(bad_constraints_module),
        code:delete(bad_constraints_module),
        file:delete(TempFile)
    end.

%% -----------------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------------

temp_beam_path(Name) ->
    TempDir = filename:basedir(user_cache, "spectra_tests"),
    ok = filelib:ensure_dir(filename:join(TempDir, "dummy")),
    Unique = integer_to_list(erlang:unique_integer([positive])),
    filename:join(TempDir, Name ++ "_" ++ Unique ++ ".beam").

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

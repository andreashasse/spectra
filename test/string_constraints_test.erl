-module(string_constraints_test).

-compile(nowarn_missing_spec).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra_internal.hrl").

%% -----------------------------------------------------------------------
%% JSON Schema generation — constraints appear in output
%% -----------------------------------------------------------------------

schema_pattern_test() ->
    Schema = spectra:schema(json_schema, string_constraints_module, {type, lowercase_binary, 0}, [
        pre_encoded
    ]),
    ?assertMatch(#{type := <<"string">>, pattern := <<"^[a-z]+$">>}, Schema).

schema_min_max_length_test() ->
    Schema = spectra:schema(json_schema, string_constraints_module, {type, bounded_binary, 0}, [
        pre_encoded
    ]),
    ?assertMatch(#{type := <<"string">>, minLength := 2, maxLength := 5}, Schema).

schema_format_test() ->
    Schema = spectra:schema(json_schema, string_constraints_module, {type, date_binary, 0}, [
        pre_encoded
    ]),
    ?assertMatch(#{type := <<"string">>, format := <<"date">>}, Schema).

schema_nonempty_min_length_overrides_baseline_test() ->
    Schema = spectra:schema(json_schema, string_constraints_module, {type, long_nonempty, 0}, [
        pre_encoded
    ]),
    %% min_length => 3 from params overrides the nonempty_binary baseline of 1
    ?assertMatch(#{type := <<"string">>, minLength := 3}, Schema).

schema_all_constraints_test() ->
    Schema = spectra:schema(json_schema, string_constraints_module, {type, full_constraints, 0}, [
        pre_encoded
    ]),
    ?assertMatch(
        #{
            type := <<"string">>,
            pattern := <<"^[a-z]+$">>,
            minLength := 2,
            maxLength := 10,
            format := <<"identifier">>
        },
        Schema
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

decode_unicode_pattern_match_test() ->
    ?assertEqual(
        {ok, <<"ä"/utf8>>},
        spectra:decode(
            json, string_constraints_module, {type, single_char_binary, 0}, <<"ä"/utf8>>, [
                pre_decoded
            ]
        )
    ).

decode_ucp_pattern_match_test() ->
    ?assertEqual(
        {ok, <<"münchen"/utf8>>},
        spectra:decode(
            json, string_constraints_module, {type, ucp_word_binary, 0}, <<"münchen"/utf8>>, [
                pre_decoded
            ]
        )
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
    {ok, bad_constraints_module, BeamBinary} = spectra_test_compile:compile_module(Code),
    TempFile = spectra_test_compile:temp_beam_path("bad_constraints_module"),
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
            spectra_test_util:from_json(TypeInfo, Type, <<"hello">>)
        )
    after
        code:purge(bad_constraints_module),
        code:delete(bad_constraints_module),
        file:delete(TempFile)
    end.

%% -----------------------------------------------------------------------
%% Encode — constraints are validated on encode, not just decode
%% -----------------------------------------------------------------------

encode_pattern_match_test() ->
    ?assertEqual(
        {ok, <<"hello">>},
        spectra:encode(json, string_constraints_module, {type, lowercase_binary, 0}, <<"hello">>, [
            pre_encoded
        ])
    ).

encode_pattern_no_match_test() ->
    ?assertMatch(
        {error, [_]},
        spectra:encode(json, string_constraints_module, {type, lowercase_binary, 0}, <<"Hello">>, [
            pre_encoded
        ])
    ).

encode_too_short_test() ->
    ?assertMatch(
        {error, [_]},
        spectra:encode(json, string_constraints_module, {type, bounded_binary, 0}, <<"a">>, [
            pre_encoded
        ])
    ).

encode_too_long_test() ->
    ?assertMatch(
        {error, [_]},
        spectra:encode(
            json, string_constraints_module, {type, bounded_binary, 0}, <<"toolong">>, [pre_encoded]
        )
    ).

encode_within_bounds_test() ->
    ?assertEqual(
        {ok, <<"abc">>},
        spectra:encode(json, string_constraints_module, {type, bounded_binary, 0}, <<"abc">>, [
            pre_encoded
        ])
    ).

%% -----------------------------------------------------------------------
%% Remote type aliases — constraints survive #sp_remote_type{} resolution
%% -----------------------------------------------------------------------

schema_remote_min_max_test() ->
    Schema = spectra:schema(json_schema, string_constraints_module, {type, bounded_remote, 0}, [
        pre_encoded
    ]),
    ?assertMatch(#{type := <<"string">>, minLength := 2, maxLength := 5}, Schema).

schema_remote_pattern_test() ->
    Schema = spectra:schema(json_schema, string_constraints_module, {type, pattern_remote, 0}, [
        pre_encoded
    ]),
    ?assertMatch(#{type := <<"string">>, pattern := <<"^[a-z]+$">>}, Schema).

decode_remote_within_bounds_test() ->
    ?assertEqual(
        {ok, <<"abc">>},
        spectra:decode(json, string_constraints_module, {type, bounded_remote, 0}, <<"abc">>, [
            pre_decoded
        ])
    ).

decode_remote_too_short_test() ->
    ?assertMatch(
        {error, [_]},
        spectra:decode(json, string_constraints_module, {type, bounded_remote, 0}, <<"a">>, [
            pre_decoded
        ])
    ).

decode_remote_pattern_match_test() ->
    ?assertEqual(
        {ok, <<"hello">>},
        spectra:decode(json, string_constraints_module, {type, pattern_remote, 0}, <<"hello">>, [
            pre_decoded
        ])
    ).

decode_remote_pattern_no_match_test() ->
    ?assertMatch(
        {error, [_]},
        spectra:decode(json, string_constraints_module, {type, pattern_remote, 0}, <<"Hello">>, [
            pre_decoded
        ])
    ).

encode_remote_within_bounds_test() ->
    ?assertEqual(
        {ok, <<"abc">>},
        spectra:encode(json, string_constraints_module, {type, bounded_remote, 0}, <<"abc">>, [
            pre_encoded
        ])
    ).

encode_remote_too_short_test() ->
    ?assertMatch(
        {error, [_]},
        spectra:encode(json, string_constraints_module, {type, bounded_remote, 0}, <<"a">>, [
            pre_encoded
        ])
    ).

%% -----------------------------------------------------------------------
%% Error handling — invalid UTF-8 and invalid regex patterns
%% -----------------------------------------------------------------------

decode_pattern_invalid_utf8_returns_error_test() ->
    %% Bare 0xFF is not valid UTF-8; should return {error, [...]}, not crash
    InvalidUtf8 = <<255>>,
    ?assertMatch(
        {error, [_]},
        spectra:decode(
            json, string_constraints_module, {type, lowercase_binary, 0}, InvalidUtf8, [pre_decoded]
        )
    ).

encode_pattern_invalid_utf8_returns_error_test() ->
    %% Bare 0xFF is not valid UTF-8; should return {error, [...]}, not crash
    InvalidUtf8 = <<255>>,
    ?assertMatch(
        {error, [_]},
        spectra:encode(
            json, string_constraints_module, {type, lowercase_binary, 0}, InvalidUtf8, [pre_encoded]
        )
    ).

decode_pattern_invalid_regex_returns_specific_error_test() ->
    ?assertError(
        {invalid_string_pattern, <<"[invalid">>, _},
        spectra:decode(
            json, string_constraints_module, {type, invalid_pattern_binary, 0}, <<"anything">>, [
                pre_decoded
            ]
        )
    ).

encode_pattern_invalid_regex_returns_specific_error_test() ->
    ?assertError(
        {invalid_string_pattern, <<"[invalid">>, _},
        spectra:encode(
            json, string_constraints_module, {type, invalid_pattern_binary, 0}, <<"anything">>, [
                pre_encoded
            ]
        )
    ).

-module(type_params_test).

%% nowarn_missing_spec is needed because test helper functions don't need specs.
-compile(nowarn_missing_spec).

-include_lib("eunit/include/eunit.hrl").

%% -----------------------------------------------------------------------
%% Codec registration helper
%% -----------------------------------------------------------------------

set_codec() ->
    application:set_env(spectra, codecs, #{
        {type_params_module, {type, parameterized_type, 0}} => type_params_codec,
        {type_params_module, {type, no_params_type, 0}} => type_params_codec,
        {type_params_module, {record, parameterized_rec}} => type_params_codec
    }).

unset_codec() ->
    application:unset_env(spectra, codecs).

%% -----------------------------------------------------------------------
%% AC1 — Parameters flow into decode
%% -----------------------------------------------------------------------

params_flow_into_decode_test() ->
    set_codec(),
    try
        ?assertEqual(
            {ok, {decoded, <<"^[a-z]+$">>, <<"hello">>}},
            spectra:decode(json, type_params_module, {type, parameterized_type, 0}, <<"hello">>, [
                pre_decoded
            ])
        )
    after
        unset_codec()
    end.

%% -----------------------------------------------------------------------
%% AC2 — Parameters flow into encode
%% -----------------------------------------------------------------------

params_flow_into_encode_test() ->
    set_codec(),
    try
        ?assertEqual(
            {ok, {encoded, <<"^[a-z]+$">>, <<"hello">>}},
            spectra:encode(json, type_params_module, {type, parameterized_type, 0}, <<"hello">>, [
                pre_encoded
            ])
        )
    after
        unset_codec()
    end.

%% -----------------------------------------------------------------------
%% AC3 — Parameters flow into schema
%% -----------------------------------------------------------------------

params_flow_into_schema_test() ->
    set_codec(),
    try
        Schema = spectra:schema(
            json_schema, type_params_module, {type, parameterized_type, 0}
        ),
        ?assertMatch(
            #{<<"type">> := <<"string">>, <<"params">> := <<"^[a-z]+$">>},
            json:decode(iolist_to_binary(Schema))
        )
    after
        unset_codec()
    end.

%% -----------------------------------------------------------------------
%% AC4 — No parameters → `undefined`
%% -----------------------------------------------------------------------

no_params_decode_test() ->
    set_codec(),
    try
        ?assertEqual(
            {ok, {decoded, undefined, <<"hello">>}},
            spectra:decode(json, type_params_module, {type, no_params_type, 0}, <<"hello">>, [
                pre_decoded
            ])
        )
    after
        unset_codec()
    end.

no_params_encode_test() ->
    set_codec(),
    try
        ?assertEqual(
            {ok, {encoded, undefined, <<"hello">>}},
            spectra:encode(json, type_params_module, {type, no_params_type, 0}, <<"hello">>, [
                pre_encoded
            ])
        )
    after
        unset_codec()
    end.

no_params_schema_test() ->
    set_codec(),
    try
        Schema = spectra:schema(
            json_schema, type_params_module, {type, no_params_type, 0}
        ),
        %% undefined atom is JSON-encoded as the string "undefined"
        ?assertMatch(
            #{<<"type">> := <<"string">>, <<"params">> := <<"undefined">>},
            json:decode(iolist_to_binary(Schema))
        )
    after
        unset_codec()
    end.

%% AC4 — Parameters for records
record_params_decode_test() ->
    set_codec(),
    try
        Data = #{value => <<"hello">>},
        ?assertEqual(
            {ok, {decoded, <<"^[a-z]+$">>, Data}},
            spectra:decode(json, type_params_module, {record, parameterized_rec}, Data, [
                pre_decoded
            ])
        )
    after
        unset_codec()
    end.

record_params_encode_test() ->
    set_codec(),
    try
        Data = #{value => <<"hello">>},
        ?assertEqual(
            {ok, {encoded, <<"^[a-z]+$">>, Data}},
            spectra:encode(json, type_params_module, {record, parameterized_rec}, Data, [
                pre_encoded
            ])
        )
    after
        unset_codec()
    end.

%% -----------------------------------------------------------------------
%% AC5 — Remote type uses defining module's parameters
%% -----------------------------------------------------------------------

remote_type_uses_defining_module_params_decode_test() ->
    set_codec(),
    try
        ?assertEqual(
            {ok, {decoded, <<"^[a-z]+$">>, <<"hello">>}},
            spectra:decode(
                json,
                type_params_remote_consumer,
                {type, uses_remote, 0},
                <<"hello">>,
                [pre_decoded]
            )
        )
    after
        unset_codec()
    end.

remote_type_uses_defining_module_params_encode_test() ->
    set_codec(),
    try
        ?assertEqual(
            {ok, {encoded, <<"^[a-z]+$">>, <<"hello">>}},
            spectra:encode(
                json,
                type_params_remote_consumer,
                {type, uses_remote, 0},
                <<"hello">>,
                [pre_encoded]
            )
        )
    after
        unset_codec()
    end.

%% -----------------------------------------------------------------------
%% AC7 — Unknown keys in -spectra(...) still crash
%% -----------------------------------------------------------------------

unknown_spectra_key_crashes_test() ->
    Code =
        "-module(test_unknown_spectra_key).\n"
        "-compile([nowarn_unused_type]).\n"
        "-spectra(#{unknown_key => value}).\n"
        "-type my_type() :: integer().\n",
    {ok, test_unknown_spectra_key, BeamBinary} = spectra_test_compile:compile_module(Code),
    TempFile = spectra_test_compile:temp_beam_path("test_unknown_spectra_key"),
    ok = file:write_file(TempFile, BeamBinary),
    try
        ?assertError(
            {invalid_spectra_field, unknown_key, value},
            spectra_abstract_code:types_in_module_path(TempFile)
        )
    after
        file:delete(TempFile)
    end.

%% -----------------------------------------------------------------------
%% AC8 — type_parameters on a function spec crashes
%% -----------------------------------------------------------------------

type_parameters_on_function_spec_crashes_test() ->
    Code =
        "-module(test_type_params_on_func).\n"
        "-export([my_fun/1]).\n"
        "-spectra(#{type_parameters => <<\"value\">>}).\n"
        "-spec my_fun(integer()) -> integer().\n"
        "my_fun(X) -> X.\n",
    {ok, test_type_params_on_func, BeamBinary} = spectra_test_compile:compile_module(Code),
    TempFile = spectra_test_compile:temp_beam_path("test_type_params_on_func"),
    ok = file:write_file(TempFile, BeamBinary),
    try
        ?assertError(
            {invalid_spectra_field, type_parameters, _},
            spectra_abstract_code:types_in_module_path(TempFile)
        )
    after
        file:delete(TempFile)
    end.

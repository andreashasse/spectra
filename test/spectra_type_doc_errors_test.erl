-module(spectra_type_doc_errors_test).

-include_lib("eunit/include/eunit.hrl").

%% Test error cases for position-based -spectra attribute

%% This test module verifies that the -spectra position-based attribute
%% correctly rejects invalid usage patterns

orphaned_doc_at_eof_test() ->
    %% Test that a module with -spectra at EOF without following type fails
    Code =
        "-module(test_orphaned_eof).\n"
        "-compile(nowarn_unused_type).\n"
        "-type user_id() :: integer().\n"
        "-spectra(#{title => <<\"Orphaned\">>}).\n",

    {ok, test_orphaned_eof, BeamBinary} = compile:forms(
        parse_module(Code),
        [binary, return_errors, debug_info]
    ),

    %% Write to temp file so we can read it with beam_lib
    TempFile = "/tmp/test_orphaned_eof.beam",
    ok = file:write_file(TempFile, BeamBinary),

    %% Attempting to extract types should error
    ?assertError(
        {orphaned_spectra, #{title := <<"Orphaned">>}},
        spectra_abstract_code:types_in_module_path(TempFile)
    ),

    file:delete(TempFile).

consecutive_type_docs_test() ->
    %% Test that two consecutive -spectra attributes fail
    Code =
        "-module(test_consecutive_docs).\n"
        "-compile(nowarn_unused_type).\n"
        "-spectra(#{title => <<\"First\">>}).\n"
        "-spectra(#{title => <<\"Second\">>}).\n"
        "-type my_type() :: integer().\n",

    {ok, test_consecutive_docs, BeamBinary} = compile:forms(
        parse_module(Code),
        [binary, return_errors, debug_info]
    ),

    TempFile = "/tmp/test_consecutive_docs.beam",
    ok = file:write_file(TempFile, BeamBinary),

    %% Should error when the second -spectra is encountered
    ?assertError(
        {orphaned_spectra, #{title := <<"First">>}},
        spectra_abstract_code:types_in_module_path(TempFile)
    ),

    file:delete(TempFile).

multiple_types_with_docs_test() ->
    %% Test that multiple types, each with their own doc, works correctly
    Code =
        "-module(test_multiple_docs).\n"
        "-compile(nowarn_unused_type).\n"
        "-spectra(#{title => <<\"First Type\">>}).\n"
        "-type first() :: integer().\n"
        "-spectra(#{title => <<\"Second Type\">>}).\n"
        "-type second() :: binary().\n"
        "-spectra(#{title => <<\"Third Type\">>}).\n"
        "-type third() :: atom().\n",

    {ok, test_multiple_docs, BeamBinary} = compile:forms(
        parse_module(Code),
        [binary, return_errors, debug_info]
    ),

    TempFile = "/tmp/test_multiple_docs.beam",
    ok = file:write_file(TempFile, BeamBinary),

    %% Should succeed
    TypeInfo = spectra_abstract_code:types_in_module_path(TempFile),

    %% Verify all three docs are present
    {ok, FirstDoc} = spectra_type_info:find_doc(TypeInfo, first, 0),
    ?assertEqual(#{title => <<"First Type">>}, FirstDoc),

    {ok, SecondDoc} = spectra_type_info:find_doc(TypeInfo, second, 0),
    ?assertEqual(#{title => <<"Second Type">>}, SecondDoc),

    {ok, ThirdDoc} = spectra_type_info:find_doc(TypeInfo, third, 0),
    ?assertEqual(#{title => <<"Third Type">>}, ThirdDoc),

    file:delete(TempFile).

mixed_docs_and_no_docs_test() ->
    %% Test that types with and without docs can coexist
    Code =
        "-module(test_mixed_docs).\n"
        "-compile(nowarn_unused_type).\n"
        "-type no_doc_type() :: integer().\n"
        "-spectra(#{title => <<\"Has Doc\">>}).\n"
        "-type with_doc() :: binary().\n"
        "-type another_no_doc() :: atom().\n",

    {ok, test_mixed_docs, BeamBinary} = compile:forms(
        parse_module(Code),
        [binary, return_errors, debug_info]
    ),

    TempFile = "/tmp/test_mixed_docs.beam",
    ok = file:write_file(TempFile, BeamBinary),

    TypeInfo = spectra_abstract_code:types_in_module_path(TempFile),

    %% Verify no_doc_type has no doc
    error = spectra_type_info:find_doc(TypeInfo, no_doc_type, 0),

    %% Verify with_doc has doc
    {ok, Doc} = spectra_type_info:find_doc(TypeInfo, with_doc, 0),
    ?assertEqual(#{title => <<"Has Doc">>}, Doc),

    %% Verify another_no_doc has no doc
    error = spectra_type_info:find_doc(TypeInfo, another_no_doc, 0),

    file:delete(TempFile).

record_with_doc_test() ->
    %% Test that records can have -spectra documentation
    Code =
        "-module(test_record_doc).\n"
        "-compile(nowarn_unused_record).\n"
        "-spectra(#{title => <<\"User Record\">>, description => <<\"Represents a user\">>}).\n"
        "-record(user, {id :: integer(), name :: binary()}).\n",

    {ok, test_record_doc, BeamBinary} = compile:forms(
        parse_module(Code),
        [binary, return_errors, debug_info]
    ),

    TempFile = "/tmp/test_record_doc.beam",
    ok = file:write_file(TempFile, BeamBinary),

    TypeInfo = spectra_abstract_code:types_in_module_path(TempFile),

    %% Verify record has doc
    {ok, Doc} = spectra_type_info:find_record_doc(TypeInfo, user),
    ?assertEqual(#{title => <<"User Record">>, description => <<"Represents a user">>}, Doc),

    file:delete(TempFile).

mixed_types_and_records_with_docs_test() ->
    %% Test that types and records can both have docs in the same module
    Code =
        "-module(test_mixed_types_records).\n"
        "-compile([nowarn_unused_type, nowarn_unused_record]).\n"
        "-spectra(#{title => <<\"User ID Type\">>}).\n"
        "-type user_id() :: pos_integer().\n"
        "-spectra(#{title => <<\"User Record\">>}).\n"
        "-record(user, {id :: user_id(), name :: binary()}).\n"
        "-spectra(#{title => <<\"Status Type\">>}).\n"
        "-type status() :: active | inactive.\n",

    {ok, test_mixed_types_records, BeamBinary} = compile:forms(
        parse_module(Code),
        [binary, return_errors, debug_info]
    ),

    TempFile = "/tmp/test_mixed_types_records.beam",
    ok = file:write_file(TempFile, BeamBinary),

    TypeInfo = spectra_abstract_code:types_in_module_path(TempFile),

    %% Verify type docs
    {ok, TypeDoc1} = spectra_type_info:find_doc(TypeInfo, user_id, 0),
    ?assertEqual(#{title => <<"User ID Type">>}, TypeDoc1),

    {ok, TypeDoc2} = spectra_type_info:find_doc(TypeInfo, status, 0),
    ?assertEqual(#{title => <<"Status Type">>}, TypeDoc2),

    %% Verify record doc
    {ok, RecordDoc} = spectra_type_info:find_record_doc(TypeInfo, user),
    ?assertEqual(#{title => <<"User Record">>}, RecordDoc),

    file:delete(TempFile).

%% Helper to parse module code into forms
parse_module(Code) ->
    %% Split code into lines and scan each one
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

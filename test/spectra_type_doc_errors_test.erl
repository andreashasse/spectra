-module(spectra_type_doc_errors_test).

-include_lib("eunit/include/eunit.hrl").
-include("../include/spectra_internal.hrl").

orphaned_doc_at_eof_test() ->
    Code =
        "-module(test_orphaned_eof).\n"
        "-compile(nowarn_unused_type).\n"
        "-type user_id() :: integer().\n"
        "-spectra(#{title => <<\"Orphaned\">>}).\n",

    {ok, test_orphaned_eof, BeamBinary} = compile:forms(
        parse_module(Code),
        [binary, return_errors, debug_info]
    ),

    TempFile = "/tmp/test_orphaned_eof.beam",
    ok = file:write_file(TempFile, BeamBinary),

    ?assertError(
        {orphaned_spectra, #{title := <<"Orphaned">>}},
        spectra_abstract_code:types_in_module_path(TempFile)
    ),

    file:delete(TempFile).

consecutive_type_docs_test() ->
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

    ?assertError(
        {orphaned_spectra, #{title := <<"First">>}},
        spectra_abstract_code:types_in_module_path(TempFile)
    ),

    file:delete(TempFile).

multiple_types_with_docs_test() ->
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

    TypeInfo = spectra_abstract_code:types_in_module_path(TempFile),

    {ok, FirstType} = spectra_type_info:find_type(TypeInfo, first, 0),
    #{doc := FirstDoc} = FirstType#sp_simple_type.meta,
    ?assertEqual(#{title => <<"First Type">>}, FirstDoc),

    {ok, SecondType} = spectra_type_info:find_type(TypeInfo, second, 0),
    #{doc := SecondDoc} = SecondType#sp_simple_type.meta,
    ?assertEqual(#{title => <<"Second Type">>}, SecondDoc),

    {ok, ThirdType} = spectra_type_info:find_type(TypeInfo, third, 0),
    #{doc := ThirdDoc} = ThirdType#sp_simple_type.meta,
    ?assertEqual(#{title => <<"Third Type">>}, ThirdDoc),

    file:delete(TempFile).

mixed_docs_and_no_docs_test() ->
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

    {ok, NoDocType} = spectra_type_info:find_type(TypeInfo, no_doc_type, 0),
    #{} = NoDocType#sp_simple_type.meta,  % No doc field

    {ok, WithDocType} = spectra_type_info:find_type(TypeInfo, with_doc, 0),
    #{doc := Doc} = WithDocType#sp_simple_type.meta,
    ?assertEqual(#{title => <<"Has Doc">>}, Doc),

    {ok, AnotherNoDocType} = spectra_type_info:find_type(TypeInfo, another_no_doc, 0),
    #{} = AnotherNoDocType#sp_simple_type.meta,  % No doc field

    file:delete(TempFile).

record_with_doc_test() ->
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

    {ok, Record} = spectra_type_info:find_record(TypeInfo, user),
    #{doc := Doc} = Record#sp_rec.meta,
    ?assertEqual(#{title => <<"User Record">>, description => <<"Represents a user">>}, Doc),

    file:delete(TempFile).

mixed_types_and_records_with_docs_test() ->
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

    {ok, TypeId} = spectra_type_info:find_type(TypeInfo, user_id, 0),
    #{doc := TypeDoc1} = TypeId#sp_simple_type.meta,
    ?assertEqual(#{title => <<"User ID Type">>}, TypeDoc1),

    {ok, StatusType} = spectra_type_info:find_type(TypeInfo, status, 0),
    #{doc := TypeDoc2} = StatusType#sp_union.meta,
    ?assertEqual(#{title => <<"Status Type">>}, TypeDoc2),

    {ok, UserRecord} = spectra_type_info:find_record(TypeInfo, user),
    #{doc := RecordDoc} = UserRecord#sp_rec.meta,
    ?assertEqual(#{title => <<"User Record">>}, RecordDoc),

    file:delete(TempFile).

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

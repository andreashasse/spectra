-module(interleaved_attrs_test).

-include_lib("eunit/include/eunit.hrl").
-include("../include/spectra_internal.hrl").

interleaved_attrs_test() ->
    Code =
        "-module(test_interleaved).\n"
        "-compile([nowarn_unused_type, nowarn_unused_record]).\n"
        "-spectra(#{title => <<\"Type with doc attr\">>}).\n"
        "-doc \"Some erlang doc\".\n"
        "-type my_type() :: integer().\n"
        "-spectra(#{title => <<\"Record with doc attr\">>}).\n"
        "-doc \"Record documentation\".\n"
        "-record(my_record, {field :: integer()}).\n",

    {ok, test_interleaved, BeamBinary} = compile:forms(
        parse_module(Code),
        [binary, return_errors, debug_info]
    ),

    TempFile = "/tmp/test_interleaved.beam",
    ok = file:write_file(TempFile, BeamBinary),

    TypeInfo = spectra_abstract_code:types_in_module_path(TempFile),

    {ok, Type} = spectra_type_info:find_type(TypeInfo, my_type, 0),
    #{doc := TypeDoc} = Type#sp_simple_type.meta,
    ?assertEqual(#{title => <<"Type with doc attr">>}, TypeDoc),

    {ok, Record} = spectra_type_info:find_record(TypeInfo, my_record),
    #{doc := RecordDoc} = Record#sp_rec.meta,
    ?assertEqual(#{title => <<"Record with doc attr">>}, RecordDoc),

    file:delete(TempFile).

only_doc_no_spectra_test() ->
    Code =
        "-module(test_only_doc).\n"
        "-compile([nowarn_unused_type]).\n"
        "-doc \"Just erlang doc\".\n"
        "-type my_type() :: integer().\n",

    {ok, test_only_doc, BeamBinary} = compile:forms(
        parse_module(Code),
        [binary, return_errors, debug_info]
    ),

    TempFile = "/tmp/test_only_doc.beam",
    ok = file:write_file(TempFile, BeamBinary),

    TypeInfo = spectra_abstract_code:types_in_module_path(TempFile),

    {ok, Type} = spectra_type_info:find_type(TypeInfo, my_type, 0),
    #{} = Type#sp_simple_type.meta,  % No doc field means error case

    file:delete(TempFile).

spectra_before_wrong_type_test() ->
    Code =
        "-module(test_wrong_type).\n"
        "-compile([nowarn_unused_type]).\n"
        "-spectra(#{title => <<\"Should be on first\">>}).\n"
        "-type first() :: integer().\n"
        "-type second() :: binary().\n",

    {ok, test_wrong_type, BeamBinary} = compile:forms(
        parse_module(Code),
        [binary, return_errors, debug_info]
    ),

    TempFile = "/tmp/test_wrong_type.beam",
    ok = file:write_file(TempFile, BeamBinary),

    TypeInfo = spectra_abstract_code:types_in_module_path(TempFile),

    {ok, FirstType} = spectra_type_info:find_type(TypeInfo, first, 0),
    #{doc := FirstDoc} = FirstType#sp_simple_type.meta,
    ?assertEqual(#{title => <<"Should be on first">>}, FirstDoc),

    {ok, SecondType} = spectra_type_info:find_type(TypeInfo, second, 0),
    #{} = SecondType#sp_simple_type.meta,  % No doc field

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

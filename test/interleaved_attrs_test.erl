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

    {ok, test_interleaved, BeamBinary} = spectra_test_compile:compile_module(Code),

    TempFile = spectra_test_compile:temp_beam_path("test_interleaved"),
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

    {ok, test_only_doc, BeamBinary} = spectra_test_compile:compile_module(Code),

    TempFile = spectra_test_compile:temp_beam_path("test_only_doc"),
    ok = file:write_file(TempFile, BeamBinary),

    TypeInfo = spectra_abstract_code:types_in_module_path(TempFile),

    {ok, Type} = spectra_type_info:find_type(TypeInfo, my_type, 0),
    % No doc field means error case
    #{} = Type#sp_simple_type.meta,

    file:delete(TempFile).

spectra_before_wrong_type_test() ->
    Code =
        "-module(test_wrong_type).\n"
        "-compile([nowarn_unused_type]).\n"
        "-spectra(#{title => <<\"Should be on first\">>}).\n"
        "-type first() :: integer().\n"
        "-type second() :: binary().\n",

    {ok, test_wrong_type, BeamBinary} = spectra_test_compile:compile_module(Code),

    TempFile = spectra_test_compile:temp_beam_path("test_wrong_type"),
    ok = file:write_file(TempFile, BeamBinary),

    TypeInfo = spectra_abstract_code:types_in_module_path(TempFile),

    {ok, FirstType} = spectra_type_info:find_type(TypeInfo, first, 0),
    #{doc := FirstDoc} = FirstType#sp_simple_type.meta,
    ?assertEqual(#{title => <<"Should be on first">>}, FirstDoc),

    {ok, SecondType} = spectra_type_info:find_type(TypeInfo, second, 0),
    % No doc field
    #{} = SecondType#sp_simple_type.meta,

    file:delete(TempFile).

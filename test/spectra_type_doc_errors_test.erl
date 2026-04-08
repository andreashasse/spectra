-module(spectra_type_doc_errors_test).

-include_lib("eunit/include/eunit.hrl").
-include("../include/spectra_internal.hrl").

orphaned_doc_at_eof_test() ->
    Code =
        "-module(test_orphaned_eof).\n"
        "-compile(nowarn_unused_type).\n"
        "-type user_id() :: integer().\n"
        "-spectra(#{title => <<\"Orphaned\">>}).\n",

    {ok, test_orphaned_eof, BeamBinary} = spectra_test_compile:compile_module(Code),

    TempFile = spectra_test_compile:temp_beam_path("test_orphaned_eof"),
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

    {ok, test_consecutive_docs, BeamBinary} = spectra_test_compile:compile_module(Code),

    TempFile = spectra_test_compile:temp_beam_path("test_consecutive_docs"),
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

    {ok, test_multiple_docs, BeamBinary} = spectra_test_compile:compile_module(Code),

    TempFile = spectra_test_compile:temp_beam_path("test_multiple_docs"),
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

    {ok, test_mixed_docs, BeamBinary} = spectra_test_compile:compile_module(Code),

    TempFile = spectra_test_compile:temp_beam_path("test_mixed_docs"),
    ok = file:write_file(TempFile, BeamBinary),

    TypeInfo = spectra_abstract_code:types_in_module_path(TempFile),

    {ok, NoDocType} = spectra_type_info:find_type(TypeInfo, no_doc_type, 0),
    % No doc field
    #{} = NoDocType#sp_simple_type.meta,

    {ok, WithDocType} = spectra_type_info:find_type(TypeInfo, with_doc, 0),
    #{doc := Doc} = WithDocType#sp_simple_type.meta,
    ?assertEqual(#{title => <<"Has Doc">>}, Doc),

    {ok, AnotherNoDocType} = spectra_type_info:find_type(TypeInfo, another_no_doc, 0),
    % No doc field
    #{} = AnotherNoDocType#sp_simple_type.meta,

    file:delete(TempFile).

record_with_doc_test() ->
    Code =
        "-module(test_record_doc).\n"
        "-compile(nowarn_unused_record).\n"
        "-spectra(#{title => <<\"User Record\">>, description => <<\"Represents a user\">>}).\n"
        "-record(user, {id :: integer(), name :: binary()}).\n",

    {ok, test_record_doc, BeamBinary} = spectra_test_compile:compile_module(Code),

    TempFile = spectra_test_compile:temp_beam_path("test_record_doc"),
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

    {ok, test_mixed_types_records, BeamBinary} = spectra_test_compile:compile_module(Code),

    TempFile = spectra_test_compile:temp_beam_path("test_mixed_types_records"),
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

spectra_before_function_spec_test() ->
    Code =
        "-module(test_spectra_function).\n"
        "-spectra(#{summary => <<\"My Function\">>, description => <<\"Does something\">>, deprecated => true}).\n"
        "-spec my_fun(integer()) -> binary().\n"
        "my_fun(X) -> integer_to_binary(X).\n",

    {ok, test_spectra_function, BeamBinary} = spectra_test_compile:compile_module(Code),

    TempFile = spectra_test_compile:temp_beam_path("test_spectra_function"),
    ok = file:write_file(TempFile, BeamBinary),

    TypeInfo = spectra_abstract_code:types_in_module_path(TempFile),

    {ok, [FuncSpec | _]} = spectra_type_info:find_function(TypeInfo, my_fun, 1),
    #{doc := Doc} = FuncSpec#sp_function_spec.meta,
    ?assertEqual(
        #{summary => <<"My Function">>, description => <<"Does something">>, deprecated => true},
        Doc
    ),

    file:delete(TempFile).

invalid_field_in_function_doc_test() ->
    ?assertError(
        {invalid_spectra_field, title, <<"Bad">>},
        spectra_type:normalize_function_doc(#{title => <<"Bad">>})
    ),
    ?assertError(
        {invalid_spectra_field, examples, []},
        spectra_type:normalize_function_doc(#{examples => []})
    ).

%% --- Fix #3: validate shape of 'only' in -spectra() attribute ---

only_not_a_list_errors_test() ->
    Code =
        "-module(test_only_not_a_list).\n"
        "-compile(nowarn_unused_type).\n"
        "-spectra(#{only => not_a_list}).\n"
        "-type t() :: #{name := binary()}.\n",
    {ok, test_only_not_a_list, BeamBinary} = spectra_test_compile:compile_module(Code),
    TempFile = spectra_test_compile:temp_beam_path("test_only_not_a_list"),
    ok = file:write_file(TempFile, BeamBinary),
    ?assertError(
        {invalid_spectra_field, only, not_a_list},
        spectra_abstract_code:types_in_module_path(TempFile)
    ),
    file:delete(TempFile).

only_with_non_atom_elements_errors_test() ->
    Code =
        "-module(test_only_non_atom_elements).\n"
        "-compile(nowarn_unused_type).\n"
        "-spectra(#{only => [name, 42]}).\n"
        "-type t() :: #{name := binary()}.\n",
    {ok, test_only_non_atom_elements, BeamBinary} = spectra_test_compile:compile_module(Code),
    TempFile = spectra_test_compile:temp_beam_path("test_only_non_atom_elements"),
    ok = file:write_file(TempFile, BeamBinary),
    ?assertError(
        {invalid_spectra_field, only, [name, 42]},
        spectra_abstract_code:types_in_module_path(TempFile)
    ),
    file:delete(TempFile).

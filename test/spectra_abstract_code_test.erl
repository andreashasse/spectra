-module(spectra_abstract_code_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra_internal.hrl").

spectra_function_exported_test() ->
    TypeInfo = spectra_module_types:get(spectra_test_module_with_spectra),
    ?assert(is_record(TypeInfo, type_info)),
    ?assertMatch(
        {ok, #sp_simple_type{type = string}}, spectra_type_info:find_type(TypeInfo, my_type, 0)
    ),
    ?assertMatch(
        {ok, #sp_rec{name = my_record, arity = 3}},
        spectra_type_info:find_record(TypeInfo, my_record)
    ),
    ?assertMatch({ok, [_]}, spectra_type_info:find_function(TypeInfo, my_function, 1)),
    % Check that docs are inline in the type metadata
    {ok, Type} = spectra_type_info:find_type(TypeInfo, my_type, 0),
    ?assertMatch(#sp_simple_type{meta = #{doc := #{title := <<"My Type">>}}}, Type),
    {ok, Record} = spectra_type_info:find_record(TypeInfo, my_record),
    ?assertMatch(#sp_rec{meta = #{doc := #{title := <<"My Record">>}}}, Record).

spectra_function_with_partial_data_test() ->
    TypeInfo = spectra_module_types:get(spectra_test_module_partial_spectra),
    ?assert(is_record(TypeInfo, type_info)),
    ?assertMatch({ok, _}, spectra_type_info:find_type(TypeInfo, partial_type, 0)),
    ?assertEqual(error, spectra_type_info:find_record(TypeInfo, some_record)),
    ?assertEqual(error, spectra_type_info:find_function(TypeInfo, some_function, 0)).

spectra_function_not_exported_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(other),
    ?assert(is_record(TypeInfo, type_info)),
    ?assertMatch({ok, _}, spectra_type_info:find_type(TypeInfo, account, 0)).

spectra_function_with_all_empty_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(spectra_test_module_empty_spectra),
    ?assertMatch(
        #type_info{
            types = #{},
            records = _,
            functions = #{}
        },
        TypeInfo
    ).

spectra_function_with_unloaded_module_test() ->
    code:purge(spectra_test_module_with_spectra),
    code:delete(spectra_test_module_with_spectra),
    ?assertEqual(false, code:is_loaded(spectra_test_module_with_spectra)),
    TypeInfo = spectra_module_types:get(spectra_test_module_with_spectra),
    ?assertMatch(
        {ok, #sp_simple_type{type = string}}, spectra_type_info:find_type(TypeInfo, my_type, 0)
    ).

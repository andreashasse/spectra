-module(spectra_abstract_code_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra_internal.hrl").

%% Tests for __spectra__() function in modules

spectra_function_exported_test() ->
    %% Test that when a module exports __spectra__/0, it's called to get type info
    TypeInfo = spectra_abstract_code:types_in_module(spectra_test_module_with_spectra),
    ?assert(is_record(TypeInfo, type_info)),
    
    %% Verify types are present
    ?assertMatch({ok, _}, spectra_type_info:find_type(TypeInfo, my_type, 0)),
    {ok, MyType} = spectra_type_info:find_type(TypeInfo, my_type, 0),
    ?assertMatch(#sp_simple_type{type = string}, MyType),
    
    %% Verify records are present
    ?assertMatch({ok, _}, spectra_type_info:find_record(TypeInfo, my_record)),
    {ok, MyRecord} = spectra_type_info:find_record(TypeInfo, my_record),
    ?assertMatch(#sp_rec{name = my_record, arity = 3}, MyRecord),
    
    %% Verify functions are present
    ?assertMatch({ok, _}, spectra_type_info:find_function(TypeInfo, my_function, 1)),
    {ok, MyFunction} = spectra_type_info:find_function(TypeInfo, my_function, 1),
    ?assertEqual(1, length(MyFunction)),
    
    %% Verify docs are present
    ?assertMatch({ok, _}, spectra_type_info:find_doc(TypeInfo, my_type, 0)),
    {ok, MyTypeDoc} = spectra_type_info:find_doc(TypeInfo, my_type, 0),
    ?assertMatch(#{title := <<"My Type">>}, MyTypeDoc),
    
    %% Verify record docs are present
    ?assertMatch({ok, _}, spectra_type_info:find_record_doc(TypeInfo, my_record)),
    {ok, MyRecordDoc} = spectra_type_info:find_record_doc(TypeInfo, my_record),
    ?assertMatch(#{title := <<"My Record">>}, MyRecordDoc).

spectra_function_with_partial_data_test() ->
    %% Test that missing keys in __spectra__() map default to empty maps
    TypeInfo = spectra_abstract_code:types_in_module(spectra_test_module_partial_spectra),
    ?assert(is_record(TypeInfo, type_info)),
    
    %% Verify only types are present (other fields should be empty)
    ?assertMatch({ok, _}, spectra_type_info:find_type(TypeInfo, partial_type, 0)),
    
    %% Verify other fields are empty (should return 'error')
    ?assertEqual(error, spectra_type_info:find_record(TypeInfo, some_record)),
    ?assertEqual(error, spectra_type_info:find_function(TypeInfo, some_function, 0)).

spectra_function_not_exported_test() ->
    %% Test that when __spectra__/0 is not exported, fall back to abstract code
    TypeInfo = spectra_abstract_code:types_in_module(other),
    ?assert(is_record(TypeInfo, type_info)),
    
    %% Verify it can still get types from abstract code
    ?assertMatch({ok, _}, spectra_type_info:find_type(TypeInfo, account, 0)).

spectra_function_with_all_empty_test() ->
    %% Test __spectra__/0 that returns an empty map
    TypeInfo = spectra_abstract_code:types_in_module(spectra_test_module_empty_spectra),
    ?assert(is_record(TypeInfo, type_info)),
    
    %% Verify all type-related fields are empty maps
    ?assertEqual(#{}, TypeInfo#type_info.types),
    ?assertEqual(#{}, TypeInfo#type_info.functions),
    ?assertEqual(#{}, TypeInfo#type_info.docs),
    ?assertEqual(#{}, TypeInfo#type_info.record_docs).

spectra_function_with_unloaded_module_test() ->
    %% Test that __spectra__/0 works even when module is not yet loaded
    %% This ensures code:ensure_loaded/1 is called before checking function_exported
    
    %% Purge and delete the module to ensure it's not loaded
    code:purge(spectra_test_module_with_spectra),
    code:delete(spectra_test_module_with_spectra),
    
    %% Verify module is not loaded
    ?assertEqual(false, code:is_loaded(spectra_test_module_with_spectra)),
    
    %% Now call types_in_module - it should load the module and call __spectra__/0
    TypeInfo = spectra_abstract_code:types_in_module(spectra_test_module_with_spectra),
    ?assert(is_record(TypeInfo, type_info)),
    
    %% Verify that __spectra__/0 was called by checking for the hardcoded type
    ?assertMatch({ok, _}, spectra_type_info:find_type(TypeInfo, my_type, 0)),
    {ok, MyType} = spectra_type_info:find_type(TypeInfo, my_type, 0),
    ?assertMatch(#sp_simple_type{type = string}, MyType).

-module(spectra_module_types_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra_internal.hrl").

get_module_types_without_cache_test() ->
    TypeInfo = spectra_module_types:get(other, #sp_config{module_types_cache = none}),
    ?assert(is_record(TypeInfo, type_info)),
    ?assertMatch({ok, _}, spectra_type_info:find_type(TypeInfo, account, 0)).

get_module_types_with_persistent_cache_test() ->
    spectra_module_types:clear(other),
    TypeInfo1 = spectra_module_types:get(other, #sp_config{module_types_cache = persistent}),
    ?assert(is_record(TypeInfo1, type_info)),
    TypeInfo2 = spectra_module_types:get(other, #sp_config{module_types_cache = persistent}),
    ?assertEqual(TypeInfo1, TypeInfo2),
    spectra_module_types:clear(other).

get_module_types_with_local_cache_test() ->
    spectra_module_types:clear_local(),
    TypeInfo1 = spectra_module_types:get(other, #sp_config{module_types_cache = local}),
    ?assert(is_record(TypeInfo1, type_info)),
    TypeInfo2 = spectra_module_types:get(other, #sp_config{module_types_cache = local}),
    ?assertEqual(TypeInfo1, TypeInfo2),
    spectra_module_types:clear_local().

cache_clear_test() ->
    _TypeInfo1 = spectra_module_types:get(other, #sp_config{module_types_cache = persistent}),
    ok = spectra_module_types:clear(other),
    TypeInfo2 = spectra_module_types:get(other, #sp_config{module_types_cache = persistent}),
    ?assert(is_record(TypeInfo2, type_info)),
    spectra_module_types:clear(other).

local_cache_clear_test() ->
    _TypeInfo1 = spectra_module_types:get(other, #sp_config{module_types_cache = local}),
    ok = spectra_module_types:clear_local(),
    TypeInfo2 = spectra_module_types:get(other, #sp_config{module_types_cache = local}),
    ?assert(is_record(TypeInfo2, type_info)),
    spectra_module_types:clear_local().

get_nonexistent_module_test() ->
    ?assertError(
        {module_types_not_found, nonexistent_module_xyz, nofile},
        spectra_module_types:get(nonexistent_module_xyz, #sp_config{module_types_cache = none})
    ).

get_nonexistent_module_with_persistent_cache_test() ->
    ?assertError(
        {module_types_not_found, nonexistent_module_abc, nofile},
        spectra_module_types:get(nonexistent_module_abc, #sp_config{module_types_cache = persistent})
    ).

cache_consistency_test() ->
    spectra_module_types:clear(other),
    TypeInfo1 = spectra_module_types:get(other, #sp_config{module_types_cache = persistent}),
    TypeInfo2 = spectra_module_types:get(other, #sp_config{module_types_cache = persistent}),
    TypeInfo3 = spectra_module_types:get(other, #sp_config{module_types_cache = persistent}),
    ?assertEqual(TypeInfo1, TypeInfo2),
    ?assertEqual(TypeInfo2, TypeInfo3),
    _AccountType = spectra_type_info:get_type(TypeInfo3, account, 0),
    spectra_module_types:clear(other).

local_cache_consistency_test() ->
    spectra_module_types:clear_local(),
    TypeInfo1 = spectra_module_types:get(other, #sp_config{module_types_cache = local}),
    TypeInfo2 = spectra_module_types:get(other, #sp_config{module_types_cache = local}),
    TypeInfo3 = spectra_module_types:get(other, #sp_config{module_types_cache = local}),
    ?assertEqual(TypeInfo1, TypeInfo2),
    ?assertEqual(TypeInfo2, TypeInfo3),
    spectra_module_types:clear_local().

get_module_with_type_info_fun_without_cache_test() ->
    TypeInfo = spectra_module_types:get(
        spectra_test_module_with_spectra, #sp_config{module_types_cache = none}
    ),
    ?assert(is_record(TypeInfo, type_info)),
    ?assertMatch({ok, _}, spectra_type_info:find_type(TypeInfo, my_type, 0)).

get_module_with_type_info_fun_with_persistent_cache_test() ->
    spectra_module_types:clear(spectra_test_module_with_spectra),
    TypeInfo1 = spectra_module_types:get(
        spectra_test_module_with_spectra, #sp_config{module_types_cache = persistent}
    ),
    ?assert(is_record(TypeInfo1, type_info)),
    TypeInfo2 = spectra_module_types:get(
        spectra_test_module_with_spectra, #sp_config{module_types_cache = persistent}
    ),
    ?assertEqual(TypeInfo1, TypeInfo2),
    spectra_module_types:clear(spectra_test_module_with_spectra).

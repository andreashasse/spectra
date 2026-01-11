-module(type_variable_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-compile([nowarn_unused_type]).

-type generic_container(T) :: #{value := T}.

type_variable_to_json_throws_error_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    {ok, GenericType} = spectra_type_info:get_type(TypeInfo, generic_container, 1),

    % Trying to encode a type variable directly should throw type_not_supported
    ?assertError(
        {type_not_supported, _},
        spectra_json:to_json(TypeInfo, GenericType, #{value => 42})
    ),
    ok.

type_variable_from_json_throws_error_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    {ok, GenericType} = spectra_type_info:get_type(TypeInfo, generic_container, 1),

    % Trying to decode a type variable directly should throw type_not_supported
    ?assertError(
        {type_not_supported, _},
        spectra_json:from_json(TypeInfo, GenericType, #{<<"value">> => 42})
    ),
    ok.

type_variable_to_schema_throws_error_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    {ok, GenericType} = spectra_type_info:get_type(TypeInfo, generic_container, 1),

    % Trying to generate schema for a type variable directly should throw type_not_supported
    ?assertError(
        {type_not_supported, _},
        spectra_json_schema:to_schema(TypeInfo, GenericType)
    ),
    ok.

sp_var_to_json_throws_error_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    VarType = #sp_var{name = 'T'},

    % Trying to encode an sp_var directly should throw type_not_supported
    ?assertError(
        {type_not_supported, #sp_var{name = 'T'}},
        spectra_json:to_json(TypeInfo, VarType, some_value)
    ),
    ok.

sp_var_from_json_throws_error_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    VarType = #sp_var{name = 'T'},

    % Trying to decode an sp_var directly should throw type_not_supported
    ?assertError(
        {type_not_supported, #sp_var{name = 'T'}},
        spectra_json:from_json(TypeInfo, VarType, <<"value">>)
    ),
    ok.

sp_var_to_schema_throws_error_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    VarType = #sp_var{name = 'T'},

    % Trying to generate schema for an sp_var directly should throw type_not_supported
    ?assertError(
        {type_not_supported, #sp_var{name = 'T'}},
        spectra_json_schema:to_schema(TypeInfo, VarType)
    ),
    ok.

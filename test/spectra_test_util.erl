-module(spectra_test_util).

%% Test utility module providing default-config wrappers around internal
%% codec functions. These wrappers replace the removed 3/4-arity stubs that
%% used to read application env and construct #sp_config{} inline.

-compile(nowarn_missing_spec).

-include("../include/spectra_internal.hrl").

-export([
    test_config/0,
    get_module_types/1,
    to_json/3,
    from_json/3,
    to_schema/2,
    to_string/3,
    from_string/3,
    to_binary_string/3,
    from_binary_string/3
]).

test_config() ->
    #sp_config{module_types_cache = none}.

get_module_types(Module) ->
    spectra_module_types:get(Module, test_config()).

to_json(TypeInfo, Type, Data) ->
    spectra_json:to_json(TypeInfo, Type, Data, test_config()).

from_json(TypeInfo, Type, Json) ->
    spectra_json:from_json(TypeInfo, Type, Json, test_config()).

to_schema(TypeInfo, Type) ->
    spectra_json_schema:to_schema(TypeInfo, Type, test_config()).

to_string(TypeInfo, Type, Data) ->
    spectra_string:to_string(TypeInfo, Type, Data, test_config()).

from_string(TypeInfo, Type, String) ->
    spectra_string:from_string(TypeInfo, Type, String, test_config()).

to_binary_string(TypeInfo, Type, Data) ->
    spectra_binary_string:to_binary_string(TypeInfo, Type, Data, #{}, test_config()).

from_binary_string(TypeInfo, Type, BinaryString) ->
    spectra_binary_string:from_binary_string(TypeInfo, Type, BinaryString, #{}, test_config()).

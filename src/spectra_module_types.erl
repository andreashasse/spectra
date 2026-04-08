-module(spectra_module_types).

-export([get/1, get/2, clear/1]).

%% Meant to be used when doing manual testing.
-ignore_xref([clear/1]).

-define(TYPE_INFO_FUNCTION, '__spectra_type_info__').

%% API

-doc """
Resolves type information for `Module`.

Equivalent to calling `get/2` with the default application environment config.
""".
-spec get(Module :: module()) -> spectra:type_info().
get(Module) ->
    UseCache = application:get_env(spectra, use_module_types_cache, false),
    get(Module, UseCache).

-doc """
Resolves type information for `Module`.

When `UseCache` is `true` the result is cached in `persistent_term` and
returned on subsequent calls without re-extracting the abstract code.
""".
-spec get(Module :: module(), UseCache :: boolean()) -> spectra:type_info().
get(Module, UseCache) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            ok;
        {error, Reason} ->
            erlang:error({module_types_not_found, Module, Reason})
    end,
    HasTypeInfoFun = erlang:function_exported(Module, ?TYPE_INFO_FUNCTION, 0),
    case UseCache of
        true ->
            cached_type_info(Module, HasTypeInfoFun);
        false ->
            fetch_type_info(Module, HasTypeInfoFun)
    end.

-spec clear(Module :: module()) -> ok.
clear(Module) ->
    persistent_term:erase({?MODULE, pers_types, Module}),
    ok.

%% INTERNAL

-spec cached_type_info(Module :: module(), HasTypeInfoFun :: boolean()) ->
    spectra:type_info().
cached_type_info(Module, true) ->
    TypeInfoFun = fun() -> apply(Module, ?TYPE_INFO_FUNCTION, []) end,
    do_cached_type_info(Module, TypeInfoFun);
cached_type_info(Module, false) ->
    TypeInfoFun = fun() -> spectra_abstract_code:types_in_module(Module) end,
    do_cached_type_info(Module, TypeInfoFun).

-spec do_cached_type_info(Module :: module(), TypeInfoFun :: fun(() -> spectra:type_info())) ->
    spectra:type_info().
do_cached_type_info(Module, TypeInfoFun) ->
    case pers_type(Module) of
        {ok, TypeInfo} ->
            TypeInfo;
        error ->
            TypeInfo = TypeInfoFun(),
            pers_types_set(Module, TypeInfo),
            TypeInfo
    end.

-spec fetch_type_info(Module :: module(), HasTypeInfoFun :: boolean()) ->
    spectra:type_info().
fetch_type_info(Module, true) ->
    apply(Module, ?TYPE_INFO_FUNCTION, []);
fetch_type_info(Module, false) ->
    spectra_abstract_code:types_in_module(Module).

-spec pers_type(Module :: module()) -> {ok, spectra:type_info()} | error.
pers_type(Module) ->
    case persistent_term:get({?MODULE, pers_types, Module}, undefined) of
        undefined -> error;
        TypeInfo -> {ok, TypeInfo}
    end.

-spec pers_types_set(Module :: module(), TypeInfo :: spectra:type_info()) -> ok.
pers_types_set(Module, TypeInfo) ->
    persistent_term:put({?MODULE, pers_types, Module}, TypeInfo).

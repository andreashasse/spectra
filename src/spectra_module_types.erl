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
get(Module, true) ->
    cached_type_info(Module);
get(Module, false) ->
    fetch_type_info(Module).

-spec clear(Module :: module()) -> ok.
clear(Module) ->
    persistent_term:erase({?MODULE, pers_types, Module}),
    ok.

%% INTERNAL

-spec cached_type_info(Module :: module()) ->
    spectra:type_info().
cached_type_info(Module) ->
    case pers_type(Module) of
        {ok, TypeInfo} ->
            TypeInfo;
        error ->
            TypeInfo = fetch_type_info(Module),
            pers_types_set(Module, TypeInfo),
            TypeInfo
    end.

fetch_type_info(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            ok;
        {error, Reason} ->
            erlang:error({module_types_not_found, Module, Reason})
    end,
        case erlang:function_exported(Module, ?TYPE_INFO_FUNCTION, 0) of
            true ->
                apply(Module, ?TYPE_INFO_FUNCTION, []);
            false ->
                spectra_abstract_code:types_in_module(Module)
        end.

-spec pers_type(Module :: module()) -> {ok, spectra:type_info()} | error.
pers_type(Module) ->
    case persistent_term:get({?MODULE, pers_types, Module}, undefined) of
        undefined -> error;
        TypeInfo -> {ok, TypeInfo}
    end.

-spec pers_types_set(Module :: module(), TypeInfo :: spectra:type_info()) -> ok.
pers_types_set(Module, TypeInfo) ->
    persistent_term:put({?MODULE, pers_types, Module}, TypeInfo).

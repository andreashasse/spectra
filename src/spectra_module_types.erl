-module(spectra_module_types).

-include("../include/spectra_internal.hrl").

-export([get/2, clear/1, clear_local/0]).

%% Meant to be used when doing manual testing.
-ignore_xref([clear/1, clear_local/0]).

-define(TYPE_INFO_FUNCTION, '__spectra_type_info__').
-define(LOCAL_CACHE_KEY, {?MODULE, local_cache}).

%% API

-doc """
Resolves type information for `Module` using the cache mode from `Config`.

When `Config#sp_config.module_types_cache` is `persistent` the result is
cached in `persistent_term` and returned on subsequent calls without
re-extracting the abstract code.

When the cache mode is `local` the result is cached in the process dictionary
under a single key `{spectra_module_types, local_cache}` which holds a map of
`#{module() => type_info()}`. The caller is responsible for clearing the cache
via `clear_local/0` when the top-level operation completes.

When the cache mode is `none` type information is always re-extracted.
""".
-spec get(Module :: module(), Config :: spectra:sp_config()) -> spectra:type_info().
get(Module, #sp_config{module_types_cache = persistent}) ->
    persistent_cached_type_info(Module);
get(Module, #sp_config{module_types_cache = local}) ->
    local_cached_type_info(Module);
get(Module, #sp_config{module_types_cache = none}) ->
    fetch_type_info(Module).

-doc "Removes the persistent cache entry for `Module`.".
-spec clear(Module :: module()) -> ok.
clear(Module) ->
    persistent_term:erase({?MODULE, pers_types, Module}),
    ok.

-doc "Removes all local (process-dictionary) cache entries for the calling process.".
-spec clear_local() -> ok.
clear_local() ->
    erlang:erase(?LOCAL_CACHE_KEY),
    ok.

%% INTERNAL

-spec persistent_cached_type_info(Module :: module()) -> spectra:type_info().
persistent_cached_type_info(Module) ->
    case pers_type(Module) of
        {ok, TypeInfo} ->
            TypeInfo;
        error ->
            TypeInfo = fetch_type_info(Module),
            pers_types_set(Module, TypeInfo),
            TypeInfo
    end.

-spec local_cached_type_info(Module :: module()) -> spectra:type_info().
local_cached_type_info(Module) ->
    Cache =
        case erlang:get(?LOCAL_CACHE_KEY) of
            undefined -> #{};
            Map -> Map
        end,
    case Cache of
        #{Module := TypeInfo} ->
            TypeInfo;
        #{} ->
            TypeInfo = fetch_type_info(Module),
            erlang:put(?LOCAL_CACHE_KEY, Cache#{Module => TypeInfo}),
            TypeInfo
    end.

-spec fetch_type_info(Module :: module()) -> spectra:type_info().
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

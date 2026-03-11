-module(spectra_module_types).

-export([get/1, clear/1]).

%% Meant to be used when doing manual testing.
-ignore_xref([clear/1]).

-type module_version() :: term().

-define(APPLICATION, spectra).
-define(TYPE_INFO_FUNCTION, '__spectra_type_info__').
%% API
-spec get(Module :: module()) -> spectra:type_info().
get(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            ok;
        {error, Reason} ->
            erlang:error({module_types_not_found, Module, Reason})
    end,
    HasTypeInfoFun = erlang:function_exported(Module, ?TYPE_INFO_FUNCTION, 0),
    case application:get_env(?APPLICATION, use_module_types_cache, false) of
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
    Vsn = module_vsn(Module),
    TypeInfoFun = fun() -> set_module_meta(Module, apply(Module, ?TYPE_INFO_FUNCTION, [])) end,
    do_cached_type_info(Module, Vsn, TypeInfoFun);
cached_type_info(Module, false) ->
    Vsn = module_vsn(Module),
    TypeInfoFun = fun() -> spectra_abstract_code:types_in_module(Module) end,
    do_cached_type_info(Module, Vsn, TypeInfoFun).

do_cached_type_info(Module, Vsn, TypeInfoFun) ->
    case pers_type(Module) of
        {Vsn, TypeInfo} ->
            TypeInfo;
        _ ->
            TypeInfo = TypeInfoFun(),
            pers_types_set(Module, Vsn, TypeInfo),
            TypeInfo
    end.

-spec fetch_type_info(Module :: module(), HasTypeInfoFun :: boolean()) ->
    spectra:type_info().
fetch_type_info(Module, true) ->
    set_module_meta(Module, apply(Module, ?TYPE_INFO_FUNCTION, []));
fetch_type_info(Module, false) ->
    spectra_abstract_code:types_in_module(Module).

-spec pers_type(Module :: module()) ->
    {module_version(), spectra:type_info()} | undefined.
pers_type(Module) ->
    persistent_term:get({?MODULE, pers_types, Module}, undefined).

-spec pers_types_set(
    Module :: module(),
    Vsn :: module_version(),
    TypeInfo :: spectra:type_info()
) ->
    ok.
pers_types_set(Module, Vsn, TypeInfo) ->
    persistent_term:put({?MODULE, pers_types, Module}, {Vsn, TypeInfo}).

-spec set_module_meta(Module :: module(), TypeInfo :: spectra:type_info()) ->
    spectra:type_info().
set_module_meta(Module, TypeInfo) ->
    Attrs = Module:module_info(attributes),
    IsBehaviour = lists:member(
        spectra_codec, lists:flatten(proplists:get_all_values(behaviour, Attrs))
    ),
    spectra_type_info:set_implements_codec(TypeInfo, IsBehaviour).

-spec ensure_module(Module :: module()) -> boolean().
ensure_module(Module) ->
    erlang:module_loaded(Module) orelse code:which(Module) =/= non_existing.

-spec module_vsn(Module :: module()) ->
    Version :: module_version().
module_vsn(Module) ->
    case ensure_module(Module) of
        true ->
            case erlang:get_module_info(Module, attributes) of
                Attrs when is_list(Attrs) ->
                    {vsn, Vsn} = lists:keyfind(vsn, 1, Attrs),
                    Vsn
            end;
        false ->
            erlang:error({module_types_not_found, Module, non_existing})
    end.

-module(spectra_transform).

-export([parse_transform/2]).
-ignore_xref([parse_transform/2]).

-define(TYPE_INFO_FUNCTION, '__spectra_type_info__').

-type forms() :: [erl_parse:abstract_form() | erl_parse:form_info()].

-doc """
Parse transform that injects `__spectra_type_info__/0` into the module being
compiled. Opt in per module with `-compile({parse_transform, spectra_transform}).`.

The generated function returns the same `spectra:type_info()` value that
`spectra_abstract_code:types_in_module/1` would produce at runtime, but without
requiring the BEAM to carry `abstract_code` debug info.

If the module already defines `__spectra_type_info__/0` (via an `-export`
attribute or a function clause), the forms are returned unchanged so
hand-written implementations take precedence.
""".
-spec parse_transform(forms(), list()) -> forms().
parse_transform(Forms, _Opts) ->
    maybe
        {ok, Module} ?= module_name(Forms),
        false ?= skip_module(Module) orelse already_defines(Forms),
        {ok, Injected} ?= try_inject(Forms, Module),
        Injected
    else
        {error, Reason} ->
            [{error, {0, ?MODULE, Reason}} | Forms];
        true ->
            Forms
    end.

-spec try_inject(forms(), module()) -> {ok, forms()} | {error, term()}.
try_inject(Forms, Module) ->
    try spectra_abstract_code:types_in_forms(Module, Forms) of
        TypeInfo ->
            {ok, inject(Forms, TypeInfo)}
    catch
        Class:Reason:Stack ->
            {error, {transform_failed, Module, Class, Reason, Stack}}
    end.

-spec module_name(forms()) -> {ok, module()} | {error, missing_module_attribute}.
module_name([{attribute, _, module, M} | _]) when is_atom(M) -> {ok, M};
module_name([_ | Rest]) -> module_name(Rest);
module_name([]) -> {error, missing_module_attribute}.

-spec skip_module(module()) -> boolean().
skip_module(spectra_transform) -> true;
skip_module(spectra_abstract_code) -> true;
skip_module(spectra_type) -> true;
skip_module(spectra_type_info) -> true;
skip_module(_) -> false.

-spec already_defines(forms()) -> boolean().
already_defines(Forms) ->
    lists:any(fun is_type_info_definition/1, Forms).

-spec is_type_info_definition(term()) -> boolean().
is_type_info_definition({attribute, _, export, List}) when is_list(List) ->
    lists:member({?TYPE_INFO_FUNCTION, 0}, List);
is_type_info_definition({function, _, ?TYPE_INFO_FUNCTION, 0, _}) ->
    true;
is_type_info_definition(_) ->
    false.

-spec inject(forms(), spectra:type_info()) -> forms().
inject(Forms, TypeInfo) ->
    Anno = erl_anno:new(0),
    ExportForm = {attribute, Anno, export, [{?TYPE_INFO_FUNCTION, 0}]},
    SpecForm =
        {attribute, Anno, spec,
            {{?TYPE_INFO_FUNCTION, 0}, [
                {type, Anno, 'fun', [
                    {type, Anno, product, []},
                    {remote_type, Anno, [
                        {atom, Anno, spectra},
                        {atom, Anno, type_info},
                        []
                    ]}
                ]}
            ]}},
    Body = erl_parse:abstract(TypeInfo, [{line, erl_anno:line(Anno)}]),
    FunctionForm =
        {function, Anno, ?TYPE_INFO_FUNCTION, 0, [
            {clause, Anno, [], [], [Body]}
        ]},
    insert_export_and_function(Forms, ExportForm, SpecForm, FunctionForm).

-spec insert_export_and_function(
    forms(),
    erl_parse:abstract_form(),
    erl_parse:abstract_form(),
    erl_parse:abstract_form()
) -> forms().
insert_export_and_function([{attribute, _, module, _} = ModForm | Rest], Export, Spec, Fun) ->
    [ModForm, Export | insert_before_eof(Rest, Spec, Fun)];
insert_export_and_function([Form | Rest], Export, Spec, Fun) ->
    [Form | insert_export_and_function(Rest, Export, Spec, Fun)];
insert_export_and_function([], _Export, Spec, Fun) ->
    [Spec, Fun].

-spec insert_before_eof(forms(), erl_parse:abstract_form(), erl_parse:abstract_form()) ->
    forms().
insert_before_eof([], Spec, Fun) ->
    [Spec, Fun];
insert_before_eof([Form | Rest] = All, Spec, Fun) ->
    case Form of
        {eof, _} ->
            [Spec, Fun | All];
        _ ->
            [Form | insert_before_eof(Rest, Spec, Fun)]
    end.

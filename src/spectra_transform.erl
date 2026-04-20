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

If the module already exports `__spectra_type_info__/0`, the forms are returned
unchanged so hand-written implementations take precedence.
""".
-spec parse_transform(forms(), list()) -> forms().
parse_transform(Forms, _Opts) ->
    Module = module_name(Forms),
    case skip_module(Module) orelse already_exports(Forms) of
        true ->
            Forms;
        false ->
            try spectra_abstract_code:types_in_forms(Module, Forms) of
                TypeInfo ->
                    inject(Forms, TypeInfo)
            catch
                Class:Reason:Stack ->
                    [
                        {error, {
                            0,
                            ?MODULE,
                            {transform_failed, Module, Class, Reason, Stack}
                        }}
                        | Forms
                    ]
            end
    end.

-spec module_name(forms()) -> module().
module_name([{attribute, _, module, M} | _]) when is_atom(M) -> M;
module_name([_ | Rest]) -> module_name(Rest).

-spec skip_module(module()) -> boolean().
skip_module(spectra_transform) -> true;
skip_module(spectra_abstract_code) -> true;
skip_module(spectra_type) -> true;
skip_module(spectra_type_info) -> true;
skip_module(_) -> false.

-spec already_exports(forms()) -> boolean().
already_exports(Forms) ->
    lists:any(fun is_type_info_export/1, Forms).

-spec is_type_info_export(term()) -> boolean().
is_type_info_export({attribute, _, export, List}) when is_list(List) ->
    lists:member({?TYPE_INFO_FUNCTION, 0}, List);
is_type_info_export(_) ->
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

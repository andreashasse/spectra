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

The export attribute, spec and function body are injected independently so
that hand-written pieces of `__spectra_type_info__/0` are always preserved:
if the function body is already defined (plus an `-export` or
`-compile(export_all)`), the forms are returned unchanged; otherwise only
the missing pieces are added.
""".
-spec parse_transform(forms(), list()) -> forms().
parse_transform(Forms, _Opts) ->
    maybe
        {ok, Module} ?= module_name(Forms),
        false ?= skip_module(Module) orelse fully_defined(Forms),
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

-spec fully_defined(forms()) -> boolean().
fully_defined(Forms) ->
    has_function_form(Forms) andalso has_export(Forms).

-spec has_function_form(forms()) -> boolean().
has_function_form(Forms) ->
    lists:any(
        fun
            ({function, _, ?TYPE_INFO_FUNCTION, 0, _}) -> true;
            (_) -> false
        end,
        Forms
    ).

-spec has_spec(forms()) -> boolean().
has_spec(Forms) ->
    lists:any(
        fun
            ({attribute, _, spec, {{?TYPE_INFO_FUNCTION, 0}, _}}) -> true;
            (_) -> false
        end,
        Forms
    ).

-spec has_export(forms()) -> boolean().
has_export(Forms) ->
    lists:any(fun is_type_info_export/1, Forms).

-spec is_type_info_export(term()) -> boolean().
is_type_info_export({attribute, _, export, List}) when is_list(List) ->
    lists:member({?TYPE_INFO_FUNCTION, 0}, List);
is_type_info_export({attribute, _, compile, Opts}) ->
    compile_exports_all(Opts);
is_type_info_export(_) ->
    false.

-spec compile_exports_all(term()) -> boolean().
compile_exports_all(export_all) -> true;
compile_exports_all(Opts) when is_list(Opts) -> lists:member(export_all, Opts);
compile_exports_all(_) -> false.

-spec inject(forms(), spectra:type_info()) -> forms().
inject(Forms, TypeInfo) ->
    Anno = erl_anno:new(0),
    ExportForms = [
        {attribute, Anno, export, [{?TYPE_INFO_FUNCTION, 0}]}
     || not has_export(Forms)
    ],
    SpecForms = [
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
            ]}}
     || not has_spec(Forms)
    ],
    FunctionForms =
        case has_function_form(Forms) of
            true ->
                [];
            false ->
                Body = erl_parse:abstract(TypeInfo, [{line, erl_anno:line(Anno)}]),
                [
                    {function, Anno, ?TYPE_INFO_FUNCTION, 0, [
                        {clause, Anno, [], [], [Body]}
                    ]}
                ]
        end,
    insert_forms(Forms, ExportForms, SpecForms ++ FunctionForms).

-spec insert_forms(forms(), [erl_parse:abstract_form()], [erl_parse:abstract_form()]) ->
    forms().
insert_forms([{attribute, _, module, _} = ModForm | Rest], ExportForms, TrailingForms) ->
    [ModForm | ExportForms ++ insert_before_eof(Rest, TrailingForms)];
insert_forms([Form | Rest], ExportForms, TrailingForms) ->
    [Form | insert_forms(Rest, ExportForms, TrailingForms)];
insert_forms([], _ExportForms, TrailingForms) ->
    TrailingForms.

-spec insert_before_eof(forms(), [erl_parse:abstract_form()]) -> forms().
insert_before_eof([], TrailingForms) ->
    TrailingForms;
insert_before_eof([{eof, _} | _] = All, TrailingForms) ->
    TrailingForms ++ All;
insert_before_eof([Form | Rest], TrailingForms) ->
    [Form | insert_before_eof(Rest, TrailingForms)].

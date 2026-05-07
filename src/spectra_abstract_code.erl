-module(spectra_abstract_code).

-include("../include/spectra_internal.hrl").

-export([
    types_in_module/1,
    types_in_module_path/1,
    types_in_forms/2,
    apply_only/2,
    apply_field_aliases/2,
    apply_ref_meta/2
]).
-ignore_xref([types_in_module_path/1, apply_only/2, apply_field_aliases/2, apply_ref_meta/2]).

-define(TYPE_INFO_FUNCTION, '__spectra_type_info__').

-define(is_primary_type(PrimaryType),
    PrimaryType =:= string orelse
        PrimaryType =:= nonempty_string orelse
        PrimaryType =:= integer orelse
        PrimaryType =:= boolean orelse
        PrimaryType =:= atom orelse
        PrimaryType =:= float orelse
        PrimaryType =:= binary orelse
        PrimaryType =:= nonempty_binary orelse
        PrimaryType =:= number orelse
        PrimaryType =:= term
).
-define(is_predefined_int_range(_Type),
    _Type =:= non_neg_integer orelse _Type =:= neg_integer orelse _Type =:= pos_integer
).

%% Due to erl_parse:af_wild_attribute() I can't use some of the types.
-type erl_parse__af_field_decl() :: term().

-spec types_in_module(atom()) -> spectra:type_info().
types_in_module(Module) ->
    case code:which(Module) of
        cover_compiled ->
            {_, _, FilePath} = code:get_object_code(Module),
            types_in_module_path(FilePath);
        Error when Error =:= non_existing orelse Error =:= preloaded ->
            erlang:error({module_types_not_found, Module, Error});
        FilePath ->
            types_in_module_path(FilePath)
    end.

-spec types_in_module_path(file:filename()) -> spectra:type_info().
types_in_module_path(FilePath) ->
    case beam_lib:chunks(FilePath, [abstract_code]) of
        {ok, {Module, [{abstract_code, {_, Forms}}]}} ->
            types_in_forms(Module, Forms);
        {ok, {Module, [{abstract_code, no_abstract_code}]}} ->
            erlang:error({module_not_compiled_with_debug_info, Module, FilePath});
        {error, beam_lib, Reason} ->
            erlang:error({beam_lib_error, FilePath, Reason})
    end.

-spec types_in_forms(module(), [erl_parse:abstract_form() | erl_parse:form_info()]) ->
    spectra:type_info().
types_in_forms(Module, Forms) ->
    %% Process the original form stream so any -spectra(...) doc attribute
    %% preceding __spectra_type_info__/0 is consumed normally, then drop the
    %% generated entry so the extracted type_info is identical whether or
    %% not spectra_transform was applied.
    ExtractedNamedTypes = process_forms_with_docs(Forms),
    NamedTypes = [T || T <- ExtractedNamedTypes, not is_generated_type_info_named_type(T)],
    Behaviours = [B || {attribute, _, behaviour, B} <- Forms],
    IsBehaviour =
        lists:member(spectra_codec, Behaviours) orelse
            lists:member('Elixir.Spectral.Codec', Behaviours),
    build_type_info(Module, IsBehaviour, NamedTypes).

-spec is_generated_type_info_named_type(term()) -> boolean().
is_generated_type_info_named_type({{function, ?TYPE_INFO_FUNCTION, 0}, _}) ->
    true;
is_generated_type_info_named_type(_) ->
    false.

-spec process_forms_with_docs(list()) -> [type_form_result()].
process_forms_with_docs(Forms) ->
    process_forms_with_docs(Forms, undefined, []).

-spec process_forms_with_docs(list(), undefined | map(), [type_form_result()]) ->
    [type_form_result()].
process_forms_with_docs([], PendingDoc, NamedTypes) ->
    case PendingDoc of
        undefined ->
            lists:reverse(NamedTypes);
        _ ->
            erlang:error({orphaned_spectra, PendingDoc})
    end;
process_forms_with_docs([{attribute, _, spectra, DocMap} | Rest], PendingDoc, NamedTypes) when
    is_map(DocMap)
->
    case PendingDoc of
        undefined ->
            process_forms_with_docs(Rest, DocMap, NamedTypes);
        _ ->
            erlang:error({orphaned_spectra, PendingDoc})
    end;
process_forms_with_docs([Form | Rest], PendingDoc, NamedTypes) ->
    case type_in_form(Form) of
        {true, TypeWithKey} ->
            process_type_form(TypeWithKey, PendingDoc, Rest, NamedTypes);
        false ->
            process_forms_with_docs(Rest, PendingDoc, NamedTypes)
    end.

-spec process_type_form(type_form_result(), undefined | map(), list(), [type_form_result()]) ->
    [type_form_result()].
process_type_form(TypeWithKey, PendingDoc, Rest, NamedTypes) ->
    case {PendingDoc, TypeWithKey} of
        {undefined, _} ->
            process_forms_with_docs(Rest, undefined, [TypeWithKey | NamedTypes]);
        {_, _} ->
            TypeInfoWithDoc = attach_doc(TypeWithKey, PendingDoc),
            process_forms_with_docs(Rest, undefined, [TypeInfoWithDoc | NamedTypes])
    end.

-spec attach_doc(type_form_result(), map()) -> type_form_result().
attach_doc({{type, _Name, _Arity} = Key, Type}, DocMap) ->
    Aliases = maps:get(field_aliases, DocMap, #{}),
    OnlyFiltered =
        case DocMap of
            #{only := Only} -> apply_only(Type, validate_only(Only));
            #{} -> Type
        end,
    FinalType = apply_field_aliases(OnlyFiltered, validate_field_aliases(Aliases)),
    CleanDocMap = maps:without([field_aliases, only, type_parameters], DocMap),
    {Key, apply_type_parameters(spectra_type:add_doc_to_type(FinalType, CleanDocMap), DocMap)};
attach_doc({{record, _Name} = Key, Record}, DocMap) ->
    Aliases = maps:get(field_aliases, DocMap, #{}),
    AliasedRecord = apply_field_aliases(Record, validate_field_aliases(Aliases)),
    CleanDocMap = maps:remove(field_aliases, maps:remove(type_parameters, DocMap)),
    {Key,
        apply_type_parameters(
            spectra_type:add_doc_to_type(AliasedRecord, CleanDocMap), DocMap
        )};
attach_doc({{function, _Name, _Arity} = Key, FuncSpecs}, DocMap) ->
    Doc = spectra_type:normalize_function_doc(DocMap),
    Tagged = [
        FS#sp_function_spec{meta = (FS#sp_function_spec.meta)#{doc => Doc}}
     || FS <- FuncSpecs
    ],
    {Key, Tagged}.

-spec validate_only([atom()]) -> [atom()].
validate_only(Only) when is_list(Only) ->
    case lists:all(fun erlang:is_atom/1, Only) of
        true -> Only;
        false -> erlang:error({invalid_spectra_field, only, Only})
    end;
validate_only(Only) ->
    erlang:error({invalid_spectra_field, only, Only}).

-spec validate_field_aliases(term()) -> #{atom() | integer() => binary()}.
validate_field_aliases(Aliases) when is_map(Aliases) ->
    maps:fold(
        fun
            (K, V, Acc) when (is_atom(K) orelse is_integer(K)), is_binary(V) -> Acc#{K => V};
            (K, V, _Acc) -> erlang:error({invalid_spectra_field, field_aliases, {K, V}})
        end,
        #{},
        Aliases
    );
validate_field_aliases(Aliases) ->
    erlang:error({invalid_spectra_field, field_aliases, Aliases}).

-spec apply_field_aliases(spectra:sp_type(), #{atom() | integer() => binary()}) ->
    spectra:sp_type().
apply_field_aliases(#sp_map{fields = Fields} = Map, Aliases) ->
    Updated = [alias_map_field(F, Aliases) || F <- Fields],
    check_unique_binary_names([BN || #literal_map_field{binary_name = BN} <- Updated]),
    Map#sp_map{fields = Updated};
apply_field_aliases(#sp_rec{fields = Fields, meta = Meta} = Rec, Aliases) ->
    Updated = [alias_rec_field(F, Aliases) || F <- Fields],
    Only = maps:get(only, Meta, all),
    IncludedNames = [
        BN
     || #sp_rec_field{binary_name = BN, name = N} <- Updated,
        Only =:= all orelse lists:member(N, Only)
    ],
    check_unique_binary_names(IncludedNames),
    Rec#sp_rec{fields = Updated};
apply_field_aliases(#sp_union{types = Types} = Union, Aliases) ->
    Union#sp_union{types = [apply_field_aliases(T, Aliases) || T <- Types]};
apply_field_aliases(#sp_type_with_variables{type = Inner} = TWV, Aliases) ->
    TWV#sp_type_with_variables{type = apply_field_aliases(Inner, Aliases)};
apply_field_aliases(#sp_remote_type{meta = Meta} = Remote, Aliases) when map_size(Aliases) > 0 ->
    Merged = maps:merge(maps:get(field_aliases, Meta, #{}), Aliases),
    Remote#sp_remote_type{meta = Meta#{field_aliases => Merged}};
apply_field_aliases(#sp_user_type_ref{meta = Meta} = Ref, Aliases) when map_size(Aliases) > 0 ->
    Merged = maps:merge(maps:get(field_aliases, Meta, #{}), Aliases),
    Ref#sp_user_type_ref{meta = Meta#{field_aliases => Merged}};
apply_field_aliases(#sp_rec_ref{meta = Meta} = RecRef, Aliases) when map_size(Aliases) > 0 ->
    Merged = maps:merge(maps:get(field_aliases, Meta, #{}), Aliases),
    RecRef#sp_rec_ref{meta = Meta#{field_aliases => Merged}};
apply_field_aliases(Other, _Aliases) ->
    Other.

-spec alias_map_field(spectra:map_field(), #{atom() | integer() => binary()}) ->
    spectra:map_field().
alias_map_field(#literal_map_field{name = Name} = F, Aliases) ->
    case Aliases of
        #{Name := Alias} -> F#literal_map_field{binary_name = Alias};
        _ -> F
    end;
alias_map_field(F, _Aliases) ->
    F.

-spec alias_rec_field(#sp_rec_field{}, #{atom() | integer() => binary()}) -> #sp_rec_field{}.
alias_rec_field(#sp_rec_field{name = Name} = F, Aliases) ->
    case Aliases of
        #{Name := Alias} -> F#sp_rec_field{binary_name = Alias};
        _ -> F
    end.

-spec check_unique_binary_names([binary()]) -> ok.
check_unique_binary_names(Names) ->
    Sorted = lists:sort(Names),
    case Sorted -- lists:usort(Sorted) of
        [] ->
            ok;
        [Dup | _] ->
            erlang:error({invalid_spectra_field, field_aliases, {duplicate_json_name, Dup}})
    end.

-doc """
Filters the fields of a map type to only those named in `Only`.

Propagates through `#sp_union{}` members so that types like `MyStruct | nil`
work correctly. For `#sp_user_type_ref{}` and `#sp_remote_type{}` nodes the
filter list is stored in the node's `meta` map and applied after the type is
resolved at encode/decode/schema time.

**Note:** When `only` is used, the produced or accepted maps may not fully conform
to the declared Erlang type. This is intentional.
""".
-spec intersect_only([atom()], [atom()] | all) -> [atom()].
intersect_only(New, all) -> New;
intersect_only(New, Existing) -> [F || F <- New, lists:member(F, Existing)].

-spec apply_only(spectra:sp_type(), [atom()]) -> spectra:sp_type().
apply_only(#sp_map{fields = Fields} = Map, Only) ->
    FilteredFields = [F || #literal_map_field{name = N} = F <- Fields, lists:member(N, Only)],
    Map#sp_map{fields = FilteredFields};
apply_only(#sp_rec{meta = Meta} = Rec, Only) ->
    Intersected = intersect_only(Only, maps:get(only, Meta, all)),
    Rec#sp_rec{meta = Meta#{only => Intersected}};
apply_only(#sp_union{types = Types} = Union, Only) ->
    Union#sp_union{types = [apply_only(T, Only) || T <- Types]};
apply_only(#sp_type_with_variables{type = Inner} = TypeWithVars, Only) ->
    TypeWithVars#sp_type_with_variables{type = apply_only(Inner, Only)};
apply_only(#sp_remote_type{meta = Meta} = Remote, Only) ->
    Intersected = intersect_only(Only, maps:get(only, Meta, all)),
    Remote#sp_remote_type{meta = Meta#{only => Intersected}};
apply_only(#sp_user_type_ref{meta = Meta} = Ref, Only) ->
    Intersected = intersect_only(Only, maps:get(only, Meta, all)),
    Ref#sp_user_type_ref{meta = Meta#{only => Intersected}};
apply_only(#sp_rec_ref{meta = Meta} = RecRef, Only) ->
    Intersected = intersect_only(Only, maps:get(only, Meta, all)),
    RecRef#sp_rec_ref{meta = Meta#{only => Intersected}};
apply_only(Other, _Only) ->
    Other.

-doc "Applies all structural transforms stored in a type-ref meta map (`only`, `field_aliases`) to a resolved type. Call after resolving a `#sp_user_type_ref{}`, `#sp_remote_type{}`, or `#sp_rec_ref{}` to honour transforms declared at the alias site.".
-spec apply_ref_meta(spectra:sp_type(), spectra:sp_type_meta()) -> spectra:sp_type().
apply_ref_meta(Type, Meta) ->
    OnlyFiltered =
        case Meta of
            #{only := Only} -> apply_only(Type, Only);
            _ -> Type
        end,
    case Meta of
        #{field_aliases := Aliases} -> apply_field_aliases(OnlyFiltered, Aliases);
        _ -> OnlyFiltered
    end.

-spec apply_type_parameters(spectra:sp_type(), map()) -> spectra:sp_type().
apply_type_parameters(Type, #{type_parameters := Params}) ->
    spectra_type:update_meta(Type, #{parameters => Params});
apply_type_parameters(Type, #{}) ->
    Type.

build_type_info(Module, IsBehaviour, NamedTypes) ->
    lists:foldl(fun build_type_info_fold/2, spectra_type_info:new(Module, IsBehaviour), NamedTypes).

build_type_info_fold({{type, Name, Arity}, Type}, TypeInfo) ->
    TaggedType = spectra_type:update_meta(Type, #{name => {type, Name, Arity}}),
    spectra_type_info:add_type(TypeInfo, Name, Arity, TaggedType);
build_type_info_fold({{record, Name}, Record}, TypeInfo) ->
    #sp_rec{} = TaggedRecord = spectra_type:update_meta(Record, #{name => {record, Name}}),
    spectra_type_info:add_record(TypeInfo, Name, TaggedRecord);
build_type_info_fold({{function, Name, Arity}, FuncSpec}, TypeInfo) ->
    spectra_type_info:add_function(TypeInfo, Name, Arity, FuncSpec).

-spec type_in_form(erl_parse:abstract_form() | erl_parse:form_info()) ->
    false | {true, type_form_result()}.
-type type_form_result() ::
    {{type, atom(), arity()}, spectra:sp_type()}
    | {{record, atom()}, spectra:sp_type()}
    | {{function, atom(), arity()}, [spectra:sp_function_spec()]}.

type_in_form({attribute, _, record, {RecordName, Fields}}) when
    is_list(Fields) andalso is_atom(RecordName)
->
    FieldInfos = lists:map(fun record_field_info/1, Fields),
    {true,
        {{record, RecordName}, #sp_rec{
            name = RecordName,
            fields = FieldInfos,
            arity = length(FieldInfos) + 1
        }}};
type_in_form({attribute, _, TypeOrOpaque, {TypeName, {_, _, record, Attrs}, [] = Args}}) when
    is_atom(TypeName) andalso
        (TypeOrOpaque =:= type orelse TypeOrOpaque =:= opaque orelse TypeOrOpaque =:= nominal)
->
    %% FIXME: Sort out why this function clause differs from all others.
    true = is_list(Attrs),
    {RecordName, FieldTypes} = record_field_types(Attrs),
    TypeArity = length(Args),
    Record = #sp_rec_ref{record_name = RecordName, field_types = FieldTypes},
    {true, {{type, TypeName, TypeArity}, Record}};
type_in_form({attribute, _, TypeOrOpaque, {TypeName, Type, Args}}) when
    is_atom(TypeName) andalso
        is_list(Args) andalso
        (TypeOrOpaque =:= type orelse TypeOrOpaque =:= opaque orelse TypeOrOpaque =:= nominal)
->
    [FieldInfo] = field_info_to_type(Type),
    Vars = lists:map(fun({var, _, VarName}) when is_atom(VarName) -> VarName end, Args),
    TypeArity = length(Args),
    case Vars of
        [] ->
            {true, {{type, TypeName, TypeArity}, FieldInfo}};
        _ ->
            {true,
                {{type, TypeName, TypeArity}, #sp_type_with_variables{type = FieldInfo, vars = Vars}}}
    end;
type_in_form({attribute, _, spec, {{FunctionName, Arity}, FunctionTypes}}) when
    is_atom(FunctionName) andalso is_integer(Arity) andalso is_list(FunctionTypes)
->
    ProcessedSpecs =
        lists:map(
            fun(FuncType) ->
                case FuncType of
                    {type, _, 'fun', [{type, _, product, Args}, ReturnType]} when
                        is_list(Args)
                    ->
                        ArgTypes =
                            lists:map(
                                fun(Arg) ->
                                    [ArgType] = field_info_to_type(Arg),
                                    ArgType
                                end,
                                Args
                            ),
                        [ReturnTypeProcessed] = field_info_to_type(ReturnType),
                        #sp_function_spec{args = ArgTypes, return = ReturnTypeProcessed};
                    {type, _, bounded_fun, [
                        {type, _, 'fun', [{type, _, product, Args}, ReturnType]}, Constraints
                    ]} when
                        is_list(Args) andalso is_list(Constraints)
                    ->
                        ConstraintMap = bound_fun_constraints(Constraints),
                        ArgTypes =
                            lists:map(
                                fun(Arg) ->
                                    [ArgType] =
                                        field_info_to_type(
                                            bound_fun_substitute_vars(
                                                Arg,
                                                ConstraintMap
                                            )
                                        ),
                                    ArgType
                                end,
                                Args
                            ),
                        [ReturnTypeProcessed] =
                            field_info_to_type(
                                bound_fun_substitute_vars(
                                    ReturnType,
                                    ConstraintMap
                                )
                            ),
                        #sp_function_spec{args = ArgTypes, return = ReturnTypeProcessed}
                end
            end,
            FunctionTypes
        ),
    {true, {{function, FunctionName, Arity}, ProcessedSpecs}};
type_in_form({attribute, _, spec, Spec}) ->
    error({bug_spec_not_handled, Spec});
type_in_form({attribute, _, TypeOrOpaque, _} = T) when
    TypeOrOpaque =:= opaque orelse TypeOrOpaque =:= type orelse TypeOrOpaque =:= nominal
->
    error({not_supported, T});
type_in_form(_) ->
    false.

-spec record_field_types(list()) -> {atom(), [spectra:record_field_arg()]}.
record_field_types(Attrs) ->
    [{atom, _, RecordName} | FieldInfo] = Attrs,
    true = is_atom(RecordName),
    FieldTypes =
        lists:map(
            fun({type, _, field_type, [{atom, _, FieldName}, RowFieldInfo]}) when
                is_atom(FieldName)
            ->
                [FieldType] = field_info_to_type(RowFieldInfo),
                {FieldName, FieldType}
            end,
            FieldInfo
        ),
    {RecordName, FieldTypes}.

-spec field_info_to_type(term()) -> [spectra:sp_type()].
field_info_to_type({ann_type, _, [{var, _, _VarName}, Type]}) ->
    field_info_to_type(Type);
field_info_to_type({atom, _, Value}) when is_atom(Value) ->
    [#sp_literal{value = Value, binary_value = atom_to_binary(Value, utf8)}];
field_info_to_type({char, _, Value}) when is_integer(Value) ->
    [#sp_literal{value = Value, binary_value = integer_to_binary(Value)}];
field_info_to_type({integer, _, Value}) when is_integer(Value) ->
    [#sp_literal{value = Value, binary_value = integer_to_binary(Value)}];
field_info_to_type(Op) when element(1, Op) =:= op ->
    Value = integer_value(Op),
    [#sp_literal{value = Value, binary_value = integer_to_binary(Value)}];
field_info_to_type({var, _, VarName}) when is_atom(VarName) ->
    [#sp_var{name = VarName}];
field_info_to_type({remote_type, _, [{atom, _, Module}, {atom, _, Type}, Args]}) when
    is_atom(Module) andalso is_atom(Type) andalso is_list(Args)
->
    MyArgs =
        lists:map(
            fun(Arg) ->
                [ArgType] = field_info_to_type(Arg),
                ArgType
            end,
            Args
        ),
    [#sp_remote_type{mfargs = {Module, Type, MyArgs}, arity = length(MyArgs)}];
field_info_to_type({Type, _, map, any}) when
    Type =:= type orelse Type =:= opaque orelse Type =:= nominal
->
    MapFields =
        [
            #typed_map_field{
                kind = assoc,
                key_type = #sp_simple_type{type = term},
                val_type = #sp_simple_type{type = term}
            }
        ],
    [#sp_map{fields = MapFields, struct_name = undefined}];
field_info_to_type({Type, _, tuple, any}) when
    Type =:= type orelse Type =:= opaque orelse Type =:= nominal
->
    [#sp_tuple{fields = any}];
field_info_to_type({user_type, _, Type, TypeAttrs}) when
    is_atom(Type) andalso is_list(TypeAttrs)
->
    TAttrs = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
    [#sp_user_type_ref{type_name = Type, variables = TAttrs, arity = length(TAttrs)}];
field_info_to_type({TypeOrOpaque, _, Type, TypeAttrs}) when
    is_list(TypeAttrs) andalso
        (TypeOrOpaque =:= type orelse TypeOrOpaque =:= opaque orelse TypeOrOpaque =:= nominal)
->
    case Type of
        record ->
            {SubTypeRecordName, FieldTypes} = record_field_types(TypeAttrs),
            [#sp_rec_ref{record_name = SubTypeRecordName, field_types = FieldTypes}];
        map ->
            MapFields = lists:flatmap(fun map_field_info/1, TypeAttrs),
            {StructName, NewMapFields} = extract_struct_name(MapFields),
            [#sp_map{fields = NewMapFields, struct_name = StructName}];
        tuple ->
            TupleFields = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [#sp_tuple{fields = TupleFields}];
        union ->
            UnionFields = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [#sp_union{types = UnionFields}];
        Fun when Fun =:= 'fun' orelse Fun =:= function ->
            case TypeAttrs of
                [] ->
                    [#sp_function{args = any, return = #sp_simple_type{type = term}}];
                [{type, _, any}, ReturnType] ->
                    [AReturnType] = field_info_to_type(ReturnType),
                    [#sp_function{args = any, return = AReturnType}];
                [{type, _, product, FunArgTypes}, ReturnType] ->
                    true = is_list(FunArgTypes),
                    AFunArgTypes = lists:flatmap(fun field_info_to_type/1, FunArgTypes),
                    [AReturnType] = field_info_to_type(ReturnType),
                    [#sp_function{args = AFunArgTypes, return = AReturnType}]
            end;
        arity ->
            [
                #sp_range{
                    type = integer,
                    lower_bound = 0,
                    upper_bound = 255
                }
            ];
        byte ->
            [
                #sp_range{
                    type = integer,
                    lower_bound = 0,
                    upper_bound = 255
                }
            ];
        char ->
            [
                #sp_range{
                    type = integer,
                    lower_bound = 0,
                    upper_bound = 16#10ffff
                }
            ];
        mfa ->
            [
                #sp_tuple{
                    fields =
                        [
                            #sp_simple_type{type = atom},
                            #sp_simple_type{type = atom},
                            #sp_range{
                                type = integer,
                                lower_bound = 0,
                                upper_bound = 255
                            }
                        ]
                }
            ];
        any ->
            [#sp_simple_type{type = term}];
        timeout ->
            [
                #sp_union{
                    types =
                        [
                            #sp_simple_type{type = non_neg_integer},
                            #sp_literal{value = infinity, binary_value = <<"infinity">>}
                        ]
                }
            ];
        pid ->
            [#sp_simple_type{type = pid}];
        iodata ->
            [#sp_simple_type{type = iodata}];
        iolist ->
            [#sp_simple_type{type = iolist}];
        port ->
            [#sp_simple_type{type = port}];
        reference ->
            [#sp_simple_type{type = reference}];
        node ->
            [#sp_simple_type{type = atom}];
        identifier ->
            [
                #sp_union{
                    types =
                        [
                            #sp_simple_type{type = pid},
                            #sp_simple_type{type = port},
                            #sp_simple_type{type = reference}
                        ]
                }
            ];
        range ->
            [MinValue, MaxValue] = TypeAttrs,
            Min = integer_value(MinValue),
            Max = integer_value(MaxValue),
            [
                #sp_range{
                    type = integer,
                    lower_bound = Min,
                    upper_bound = Max
                }
            ];
        list ->
            case lists:flatmap(fun field_info_to_type/1, TypeAttrs) of
                [ListType] ->
                    [#sp_list{type = ListType}];
                [] ->
                    [#sp_list{type = #sp_simple_type{type = term}}]
            end;
        nonempty_list ->
            case lists:flatmap(fun field_info_to_type/1, TypeAttrs) of
                [ListType] ->
                    [#sp_nonempty_list{type = ListType}];
                [] ->
                    [#sp_nonempty_list{type = #sp_simple_type{type = term}}]
            end;
        maybe_improper_list ->
            case lists:flatmap(fun field_info_to_type/1, TypeAttrs) of
                [Elements, Tail] ->
                    [#sp_maybe_improper_list{elements = Elements, tail = Tail}];
                [] ->
                    [
                        #sp_maybe_improper_list{
                            elements = #sp_simple_type{type = term},
                            tail = #sp_simple_type{type = term}
                        }
                    ]
            end;
        nonempty_improper_list ->
            [Elements, Tail] = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [#sp_nonempty_improper_list{elements = Elements, tail = Tail}];
        module ->
            [#sp_simple_type{type = atom}];
        PrimaryType when ?is_primary_type(PrimaryType) ->
            [#sp_simple_type{type = PrimaryType}];
        PartailRangeInteger when ?is_predefined_int_range(PartailRangeInteger) ->
            [#sp_simple_type{type = PartailRangeInteger}];
        bitstring ->
            [#sp_simple_type{type = bitstring}];
        nonempty_bitstring ->
            [#sp_simple_type{type = nonempty_bitstring}];
        dynamic ->
            [#sp_simple_type{type = term}];
        nil ->
            %% FIXME: Need to investigate nil. is this the same nil as in elixir?
            [#sp_literal{value = [], binary_value = <<"[]">>}];
        none ->
            [#sp_simple_type{type = none}];
        no_return ->
            [#sp_simple_type{type = none}]
    end.

integer_value({char, _, Value}) when is_integer(Value) ->
    Value;
integer_value({integer, _, Value}) when is_integer(Value) ->
    Value;
integer_value({op, _, Operator, Left, Right}) ->
    case Operator of
        '-' ->
            integer_value(Left) - integer_value(Right);
        '+' ->
            integer_value(Left) + integer_value(Right);
        '*' ->
            integer_value(Left) * integer_value(Right);
        'div' ->
            integer_value(Left) div integer_value(Right);
        'rem' ->
            integer_value(Left) rem integer_value(Right);
        'band' ->
            integer_value(Left) band integer_value(Right);
        'bor' ->
            integer_value(Left) bor integer_value(Right);
        'bxor' ->
            integer_value(Left) bxor integer_value(Right);
        'bsl' ->
            integer_value(Left) bsl integer_value(Right);
        'bsr' ->
            integer_value(Left) bsr integer_value(Right)
    end;
integer_value({op, _, Operator, Unary}) ->
    case Operator of
        '-' ->
            -integer_value(Unary);
        '+' ->
            integer_value(Unary);
        'bnot' ->
            bnot integer_value(Unary)
    end.

-spec map_field_info(term()) ->
    [spectra:map_field()].
map_field_info({_TypeOfType, _, Type, TypeAttrs}) ->
    Kind =
        case Type of
            map_field_assoc ->
                assoc;
            map_field_exact ->
                exact
        end,
    case TypeAttrs of
        [{integer, _, MapFieldName}, FieldInfo] when is_integer(MapFieldName) ->
            [AType] = field_info_to_type(FieldInfo),
            [
                #literal_map_field{
                    kind = Kind,
                    name = MapFieldName,
                    binary_name = integer_to_binary(MapFieldName),
                    val_type = AType
                }
            ];
        [{atom, _, MapFieldName}, FieldInfo] when is_atom(MapFieldName) ->
            [AType] = field_info_to_type(FieldInfo),
            [
                #literal_map_field{
                    kind = Kind,
                    name = MapFieldName,
                    binary_name = atom_to_binary(MapFieldName, utf8),
                    val_type = AType
                }
            ];
        [KeyFieldInfo, ValueFieldInfo] ->
            [KeyType] = field_info_to_type(KeyFieldInfo),
            [ValueType] = field_info_to_type(ValueFieldInfo),
            [
                #typed_map_field{
                    kind = Kind,
                    key_type = KeyType,
                    val_type = ValueType
                }
            ]
    end.

-spec record_field_info(erl_parse__af_field_decl()) -> #sp_rec_field{}.
record_field_info({record_field, _, {atom, _, FieldName}, Default}) when
    is_atom(FieldName)
->
    #sp_rec_field{
        name = FieldName,
        binary_name = atom_to_binary(FieldName, utf8),
        type = #sp_simple_type{type = term},
        default = eval_record_default(Default)
    };
record_field_info({record_field, _, {atom, _, FieldName}}) when is_atom(FieldName) ->
    #sp_rec_field{
        name = FieldName,
        binary_name = atom_to_binary(FieldName, utf8),
        type = #sp_simple_type{type = term}
    };
record_field_info({typed_record_field, {record_field, _, {atom, _, FieldName}}, Type}) when
    is_atom(FieldName)
->
    [TypeInfo] = field_info_to_type(Type),
    #sp_rec_field{
        name = FieldName,
        binary_name = atom_to_binary(FieldName, utf8),
        type = TypeInfo
    };
record_field_info(
    {typed_record_field, {record_field, _, {atom, _, FieldName}, Default}, Type}
) when
    is_atom(FieldName)
->
    [TypeInfo] = field_info_to_type(Type),
    #sp_rec_field{
        name = FieldName,
        binary_name = atom_to_binary(FieldName, utf8),
        type = TypeInfo,
        default = eval_record_default(Default)
    }.

-spec eval_record_default(dynamic()) -> undefined | {value, term()}.
eval_record_default({atom, _, Value}) when is_atom(Value) -> {value, Value};
eval_record_default({float, _, Value}) when is_float(Value) -> {value, Value};
eval_record_default({nil, _}) ->
    {value, []};
eval_record_default({string, _, Value}) when is_list(Value) -> {value, Value};
eval_record_default({bin, _, Segments}) ->
    try
        Parts = [bin_segment_value(S) || S <- Segments],
        {value, iolist_to_binary(Parts)}
    catch
        _:_ -> undefined
    end;
eval_record_default(Expr) ->
    try
        {value, integer_value(Expr)}
    catch
        _:_ -> undefined
    end.

-spec bin_segment_value(dynamic()) -> iodata().
bin_segment_value({bin_element, _, {string, _, S}, default, default}) -> S;
bin_segment_value({bin_element, _, {char, _, V}, default, default}) -> V;
bin_segment_value({bin_element, _, Expr, default, default}) -> integer_value(Expr).

%% Helper functions for bounded_fun handling
-spec bound_fun_constraints(list()) -> #{atom() => term()}.
bound_fun_constraints(Constraints) ->
    lists:foldl(fun bound_fun_constraint_aux/2, #{}, Constraints).

bound_fun_constraint_aux(
    {type, _, constraint, [{atom, _, is_subtype}, [{var, _, VarName}, TypeDef]]},
    Acc
) when
    is_atom(VarName)
->
    Acc#{VarName => TypeDef};
bound_fun_constraint_aux(Constraint, _Acc) ->
    error({unsupported_constraint, Constraint}).

bound_fun_substitute_vars({var, _, VarName} = Var, ConstraintMap) when is_atom(VarName) ->
    maps:get(VarName, ConstraintMap, Var);
bound_fun_substitute_vars(Term, _ConstraintMap) ->
    Term.

%% Helper function to extract struct name from map fields for Elixir structs
-spec extract_struct_name([spectra:map_field()]) ->
    {undefined | atom(), [spectra:map_field()]}.
extract_struct_name(MapFields) ->
    case
        lists:partition(
            fun
                (#literal_map_field{name = '__struct__', kind = exact}) ->
                    true;
                (#literal_map_field{name = '__struct__', kind = assoc}) ->
                    true;
                (_) ->
                    false
            end,
            MapFields
        )
    of
        {[#literal_map_field{val_type = #sp_literal{value = SName}}], OtherMapFields} when
            is_atom(SName)
        ->
            {SName, OtherMapFields};
        {[], _} ->
            {undefined, MapFields}
    end.

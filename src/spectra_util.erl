-module(spectra_util).

-export([
    test_abs_code/1,
    fold_until_error/3,
    map_until_error/2,
    type_replace_vars/3,
    record_replace_vars/2
]).

-ignore_xref([test_abs_code/1]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

-include_lib("spectra/include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-ifdef(TEST).

not_handled_modules_test() ->
    Modules = erlang:loaded(),
    Errors =
        lists:filtermap(
            fun(Module) ->
                case test_abs_code(Module) of
                    {ok, _Types} ->
                        false;
                    {error, [#sp_error{} | _]} ->
                        false;
                    {error, {error, {beam_lib_error, _ModuleName, _Details}, _Stack}} ->
                        false;
                    {error, {error, {module_types_not_found, _ModuleName, _State}, _Stack}} ->
                        false;
                    {error, Reason} ->
                        {true, {Module, Reason}}
                end
            end,
            Modules
        ),
    ?assertEqual([], Errors).

-endif.

-spec test_abs_code(module()) ->
    {ok, spectra:type_info()} | {error, {atom(), term(), erlang:stacktrace()}}.
test_abs_code(Module) ->
    try
        {ok, spectra_abstract_code:types_in_module(Module)}
    catch
        Class:Reason:Stacktrace ->
            {error, {Class, Reason, Stacktrace}}
    end.

-spec fold_until_error(
    Fun ::
        fun(
            (Elem :: dynamic(), Acc :: dynamic()) ->
                {error, Err :: dynamic()} | {ok, Acc :: dynamic()}
        ),
    Acc :: dynamic(),
    List :: [Elem :: dynamic()]
) ->
    {ok, Acc :: dynamic()} | {error, Err :: dynamic()}.
fold_until_error(Fun, Acc, [H | T]) ->
    case Fun(H, Acc) of
        {error, _} = Error ->
            Error;
        {ok, NewAcc} ->
            fold_until_error(Fun, NewAcc, T)
    end;
fold_until_error(Fun, Acc, []) when is_function(Fun, 2) ->
    {ok, Acc}.

-spec map_until_error(
    fun((Elem :: dynamic()) -> {error, Err :: dynamic()} | {ok, ResElem :: dynamic()}),
    [Elem :: dynamic()]
) ->
    {ok, [ResElem :: dynamic()]} | {error, Err :: dynamic()}.
map_until_error(Fun, List) when is_function(Fun, 1) ->
    map_until_error(Fun, List, []).

map_until_error(Fun, [], Acc) when is_function(Fun, 1) ->
    {ok, lists:reverse(Acc)};
map_until_error(Fun, [H | T], Acc) ->
    case Fun(H) of
        {error, _} = Error ->
            Error;
        {ok, Value} ->
            map_until_error(Fun, T, [Value | Acc])
    end.

-spec record_replace_vars(
    RecordInfo :: [#sp_rec_field{}],
    TypeArgs :: [spectra:record_field_arg()]
) -> [#sp_rec_field{}].
record_replace_vars(RecordInfo, TypeArgs) ->
    lists:foldl(
        fun({FieldName, Type}, Fields) ->
            lists:map(
                fun
                    (#sp_rec_field{name = Name} = Field) when Name =:= FieldName ->
                        Field#sp_rec_field{type = Type};
                    (Field) ->
                        Field
                end,
                Fields
            )
        end,
        RecordInfo,
        TypeArgs
    ).

-spec type_replace_vars(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type(),
    NamedTypes :: #{atom() => spectra:sp_type()}
) ->
    spectra:sp_type().
type_replace_vars(_TypeInfo, #sp_var{name = Name}, NamedTypes) ->
    case maps:find(Name, NamedTypes) of
        {ok, Type} ->
            Type;
        error ->
            erlang:error({type_variable_not_found, Name})
    end;
type_replace_vars(TypeInfo, #sp_type_with_variables{type = Type}, NamedTypes) ->
    case Type of
        #sp_union{types = UnionTypes} ->
            #sp_union{
                types =
                    lists:map(
                        fun(UnionType) ->
                            type_replace_vars(TypeInfo, UnionType, NamedTypes)
                        end,
                        UnionTypes
                    )
            };
        #sp_map{fields = Fields} = Map ->
            Map#sp_map{
                fields =
                    lists:map(
                        fun
                            (#literal_map_field{val_type = FieldType} = Field) ->
                                Field#literal_map_field{
                                    val_type = type_replace_vars(TypeInfo, FieldType, NamedTypes)
                                };
                            (#typed_map_field{key_type = KeyType, val_type = ValueType} = Field) ->
                                Field#typed_map_field{
                                    key_type = type_replace_vars(TypeInfo, KeyType, NamedTypes),
                                    val_type = type_replace_vars(TypeInfo, ValueType, NamedTypes)
                                }
                        end,
                        Fields
                    )
            };
        #sp_rec_ref{record_name = RecordName, field_types = RefFieldTypes} ->
            {ok, #sp_rec{fields = Fields} = Rec} = spectra_type_info:find_record(
                TypeInfo, RecordName
            ),
            NewRec = Rec#sp_rec{fields = record_replace_vars(Fields, RefFieldTypes)},
            type_replace_vars(TypeInfo, NewRec, NamedTypes);
        #sp_remote_type{mfargs = {Module, TypeName, Args}} ->
            % Replace variables in Args with their actual types from NamedTypes
            ResolvedArgs = lists:map(
                fun(Arg) -> type_replace_vars(TypeInfo, Arg, NamedTypes) end,
                Args
            ),
            % Return the remote type with resolved args, preserving module context
            #sp_remote_type{mfargs = {Module, TypeName, ResolvedArgs}};
        #sp_list{type = ListType} ->
            #sp_list{type = type_replace_vars(TypeInfo, ListType, NamedTypes)}
    end;
type_replace_vars(TypeInfo, #sp_rec{fields = Fields} = Rec, NamedTypes) ->
    Rec#sp_rec{
        fields =
            lists:map(
                fun(#sp_rec_field{type = NType} = Field) ->
                    Field#sp_rec_field{
                        type = type_replace_vars(TypeInfo, NType, NamedTypes)
                    }
                end,
                Fields
            )
    };
type_replace_vars(TypeInfo, #sp_list{type = ItemType}, NamedTypes) ->
    #sp_list{type = type_replace_vars(TypeInfo, ItemType, NamedTypes)};
type_replace_vars(TypeInfo, #sp_nonempty_list{type = ItemType}, NamedTypes) ->
    #sp_nonempty_list{type = type_replace_vars(TypeInfo, ItemType, NamedTypes)};
type_replace_vars(TypeInfo, #sp_union{types = Types}, NamedTypes) ->
    #sp_union{
        types =
            lists:map(
                fun(T) -> type_replace_vars(TypeInfo, T, NamedTypes) end,
                Types
            )
    };
type_replace_vars(
    TypeInfo, #sp_remote_type{mfargs = {Module, TypeName, Args}}, NamedTypes
) ->
    % Replace variables in Args with their actual types from NamedTypes
    ResolvedArgs = lists:map(
        fun(Arg) -> type_replace_vars(TypeInfo, Arg, NamedTypes) end,
        Args
    ),
    % Return the remote type with resolved args, preserving module context
    #sp_remote_type{mfargs = {Module, TypeName, ResolvedArgs}};
type_replace_vars(_TypeInfo, Type, _NamedTypes) ->
    Type.

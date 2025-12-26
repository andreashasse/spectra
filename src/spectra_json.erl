-module(spectra_json).

-export([to_json/3, from_json/3]).

-ignore_xref([to_json/3, from_json/3]).

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

%% API

-spec to_json(
    spectra:type_info() | module(),
    spectra:sp_type_or_ref(),
    Data :: dynamic()
) ->
    {ok, json:encode_value()} | {error, [spectra:error()]}.
to_json(Module, TypeRef, Data) when is_atom(Module) ->
    TypeInfo = spectra_module_types:get(Module),
    to_json(TypeInfo, TypeRef, Data);
to_json(TypeInfo, Type, Data) ->
    case do_to_json(TypeInfo, Type, Data) of
        {ok, Json} ->
            {ok, Json};
        {error, Errs} ->
            {error, Errs}
    end.

%% ERROR HELPERS

%% Helper to create type error
%% PrimType: atom like int, float, binary, etc.
%% Input: the actual value that failed
%% ExpectedType: the sp_type() record
%% Location: path to the error
-spec type_error(atom(), term(), spectra:sp_type(), [atom() | integer()]) -> spectra:error().
type_error(PrimType, Input, ExpectedType, Location) ->
    #sp_error{
        type = {type_error, PrimType},
        location = Location,
        msg = undefined,
        input = Input,
        ctx = #{expected_type => ExpectedType},
        url = undefined
    }.

%% Helper to create parse error (TODO: use in json decode and parsing functions)
%%  -spec parse_error(atom(), term(), spectra:sp_type(), [atom() | integer()]) -> spectra:error().
%% parse_error(PrimType, Input, ExpectedType, Location) ->
%%     #sp_error{
%%         type = {parse_error, PrimType},
%%         location = Location,
%%         msg = undefined,
%%         input = Input,
%%         ctx = #{expected_type => ExpectedType},
%%         url = undefined
%%     }.

%% Helper to create constraint error (TODO: use in range validation)
%% -spec constraint_error(atom(), term(), spectra:sp_type(), [atom() | integer()]) -> spectra:error().
%% constraint_error(ConstraintType, Input, ExpectedType, Location) ->
%%     #sp_error{
%%         type = {constraint_error, ConstraintType},
%%         location = Location,
%%         msg = undefined,
%%         input = Input,
%%         ctx = #{expected_type => ExpectedType},
%%         url = undefined
%%     }.

%% Helper to create missing field error
-spec missing_field_error(atom(), [atom() | integer()]) -> spectra:error().
missing_field_error(FieldName, Location) ->
    #sp_error{
        type = missing_field,
        location = Location ++ [FieldName],
        msg = undefined,
        input = undefined,
        ctx = #{field_name => FieldName},
        url = undefined
    }.

%% Helper to convert simple type atom to primitive type atom for errors
-spec simple_type_to_prim_atom(atom()) -> atom().
simple_type_to_prim_atom(integer) -> int;
simple_type_to_prim_atom(non_neg_integer) -> int;
simple_type_to_prim_atom(pos_integer) -> int;
simple_type_to_prim_atom(neg_integer) -> int;
simple_type_to_prim_atom(float) -> float;
simple_type_to_prim_atom(number) -> float;
simple_type_to_prim_atom(boolean) -> bool;
simple_type_to_prim_atom(binary) -> binary;
simple_type_to_prim_atom(nonempty_binary) -> nonempty_binary;
simple_type_to_prim_atom(string) -> string;
simple_type_to_prim_atom(nonempty_string) -> nonempty_string;
simple_type_to_prim_atom(atom) -> atom;
simple_type_to_prim_atom(map) -> map;
simple_type_to_prim_atom(Other) -> Other.

%% INTERNAL
-spec do_to_json(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type_or_ref(),
    Data :: dynamic()
) ->
    {ok, json:encode_value()} | {error, [spectra:error()]}.
do_to_json(TypeInfo, {record, RecordName}, Record) when is_atom(RecordName) ->
    record_to_json(TypeInfo, RecordName, Record, []);
do_to_json(TypeInfo, #sp_rec{} = RecordInfo, Record) when is_tuple(Record) ->
    record_to_json(TypeInfo, RecordInfo, Record, []);
do_to_json(
    TypeInfo,
    #sp_rec_ref{record_name = RecordName, field_types = TypeArgs},
    Record
) when
    is_atom(RecordName)
->
    record_to_json(TypeInfo, RecordName, Record, TypeArgs);
do_to_json(TypeInfo, #sp_user_type_ref{type_name = TypeName, variables = TypeArgs}, Data) when
    is_atom(TypeName)
->
    TypeArity = length(TypeArgs),
    {ok, Type} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, TypeArgs),
    do_to_json(TypeInfo, TypeWithoutVars, Data);
do_to_json(_TypeInfo, #sp_simple_type{type = NotSupported} = Type, _Data) when
    NotSupported =:= pid orelse
        NotSupported =:= port orelse
        NotSupported =:= reference orelse
        NotSupported =:= bitstring orelse
        NotSupported =:= nonempty_bitstring orelse
        NotSupported =:= none
->
    erlang:error({type_not_supported, Type});
do_to_json(_TypeInfo, #sp_simple_type{} = Type, Value) ->
    prim_type_to_json(Type, Value);
do_to_json(
    _TypeInfo,
    #sp_range{
        type = integer,
        lower_bound = Min,
        upper_bound = Max
    },
    Value
) when
    is_integer(Value) andalso Min =< Value, Value =< Max
->
    {ok, Value};
do_to_json(_TypeInfo, #sp_literal{value = Value, binary_value = BinaryValue}, Value) when
    is_atom(Value)
->
    {ok, BinaryValue};
do_to_json(_TypeInfo, #sp_literal{value = Value}, Value) ->
    {ok, Value};
do_to_json(TypeInfo, #sp_union{} = Type, Data) ->
    union(fun do_to_json/3, TypeInfo, Type, Data);
do_to_json(TypeInfo, #sp_nonempty_list{type = Type}, Data) ->
    nonempty_list_to_json(TypeInfo, Type, Data);
do_to_json(TypeInfo, #sp_list{type = Type}, Data) when is_list(Data) ->
    list_to_json(TypeInfo, Type, Data);
do_to_json(TypeInfo, {type, TypeName, TypeArity}, Data) when is_atom(TypeName) ->
    %% FIXME: For simple types without arity, default to 0
    {ok, Type} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    do_to_json(TypeInfo, Type, Data);
do_to_json(TypeInfo, #sp_map{struct_name = StructName} = Map, Data) ->
    case StructName of
        undefined ->
            map_to_json(TypeInfo, Map, Data);
        _ ->
            %% For Elixir structs, we expect the data to have the __struct__ field
            case maps:get('__struct__', Data, undefined) of
                StructName ->
                    map_to_json(TypeInfo, Map, Data);
                _ ->
                    {error, [
                        #sp_error{
                            type = {type_error, struct},
                            location = [],
                            msg = undefined,
                            input = Data,
                            ctx = #{expected_struct => StructName},
                            url = undefined
                        }
                    ]}
            end
    end;
do_to_json(_TypeInfo, #sp_remote_type{mfargs = {Module, TypeName, Args}}, Data) ->
    TypeInfo = spectra_module_types:get(Module),
    TypeArity = length(Args),
    {ok, Type} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, Args),
    do_to_json(TypeInfo, TypeWithoutVars, Data);
do_to_json(_TypeInfo, #sp_maybe_improper_list{} = Type, _Data) ->
    erlang:error({type_not_implemented, Type});
do_to_json(_TypeInfo, #sp_nonempty_improper_list{} = Type, _Data) ->
    erlang:error({type_not_implemented, Type});
%% Not supported types
do_to_json(_TypeInfo, #sp_tuple{} = Type, _Data) ->
    erlang:error({type_not_supported, Type});
do_to_json(_TypeInfo, #sp_function{} = Type, _Data) ->
    erlang:error({type_not_supported, Type});
do_to_json(_TypeInfo, _Type, OtherValue) ->
    %% Generic fallback type error - use old format for type references
    {error, [
        #sp_error{
            type = {type_error, unknown},
            location = [],
            msg = undefined,
            input = OtherValue,
            ctx = #{},
            url = undefined
        }
    ]}.

-spec prim_type_to_json(Type :: spectra:sp_type(), Value :: term()) ->
    {ok, json:encode_value()} | {error, [spectra:error()]}.
prim_type_to_json(#sp_simple_type{type = Type} = T, Value) ->
    case check_type_to_json(Type, Value) of
        {true, NewValue} ->
            {ok, NewValue};
        {error, Reason} ->
            {error, Reason};
        false ->
            PrimType = simple_type_to_prim_atom(Type),
            {error, [type_error(PrimType, Value, T, [])]}
    end.

nonempty_list_to_json(TypeInfo, Type, Data) when is_list(Data) andalso Data =/= [] ->
    list_to_json(TypeInfo, Type, Data);
nonempty_list_to_json(_TypeInfo, Type, Data) ->
    NonEmptyListType = #sp_nonempty_list{type = Type},
    {error, [type_error(nonempty_list, Data, NonEmptyListType, [])]}.

-spec list_to_json(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type_or_ref(),
    Data :: [term()]
) ->
    {ok, [json:encode_value()]} | {error, [spectra:error()]}.
list_to_json(TypeInfo, Type, Data) when is_list(Data) ->
    spectra_util:map_until_error(
        fun({Nr, Item}) ->
            case do_to_json(TypeInfo, Type, Item) of
                {ok, Json} ->
                    {ok, Json};
                {error, Errs} ->
                    Errs2 =
                        lists:map(
                            fun(Err) -> err_append_location(Err, Nr) end,
                            Errs
                        ),
                    {error, Errs2}
            end
        end,
        lists:enumerate(Data)
    ).

-spec map_to_json(
    TypeInfo :: spectra:type_info(),
    MapFieldTypes :: #sp_map{},
    Data :: map()
) ->
    {ok, json:encode_value()} | {error, [spectra:error()]}.
map_to_json(TypeInfo, #sp_map{fields = Fields}, Data) when
    is_map(Data)
->
    %% Check if this is an Elixir struct and remove __struct__ field for JSON serialization
    DataWithoutStruct =
        case maps:take('__struct__', Data) of
            {_StructName, CleanData} ->
                CleanData;
            error ->
                Data
        end,
    case map_fields_to_json(TypeInfo, Fields, DataWithoutStruct) of
        {ok, MapFields} ->
            {ok, maps:from_list(MapFields)};
        {error, Errors} ->
            {error, Errors}
    end;
map_to_json(_TypeInfo, _MapFieldTypes, Data) ->
    {error, [type_error(map, Data, #sp_simple_type{type = map}, [])]}.

-spec map_fields_to_json(
    TypeInfo :: spectra:type_info(),
    MapFieldTypes :: [spectra:map_field()],
    Data :: map()
) ->
    {ok, [{binary(), json:encode_value()}]} | {error, [spectra:error()]}.
map_fields_to_json(TypeInfo, MapFieldTypes, Data) ->
    Fun = fun
        (
            #literal_map_field{
                kind = assoc, name = FieldName, binary_name = BinaryFieldName, val_type = FieldType
            },
            {FieldsAcc, DataAcc}
        ) ->
            case
                {maps:take(FieldName, DataAcc), spectra_type:can_be_missing(TypeInfo, FieldType)}
            of
                {{MissingValue, NewDataAcc}, {true, MissingValue}} ->
                    {ok, {FieldsAcc, NewDataAcc}};
                {{FieldData, NewDataAcc}, _} ->
                    case do_to_json(TypeInfo, FieldType, FieldData) of
                        {ok, FieldJson} ->
                            {ok, {
                                [{BinaryFieldName, FieldJson}] ++ FieldsAcc,
                                NewDataAcc
                            }};
                        {error, Errs} ->
                            Errs2 =
                                lists:map(
                                    fun(Err) -> err_append_location(Err, FieldName) end,
                                    Errs
                                ),
                            {error, Errs2}
                    end;
                {error, _} ->
                    {ok, {FieldsAcc, DataAcc}}
            end;
        (
            #typed_map_field{kind = assoc, key_type = KeyType, val_type = ValueType},
            {FieldsAcc, DataAcc}
        ) ->
            case map_typed_field_to_json(TypeInfo, KeyType, ValueType, DataAcc) of
                {ok, {NewFields, NewDataAcc}} ->
                    {ok, {NewFields ++ FieldsAcc, NewDataAcc}};
                {error, _} = Err ->
                    Err
            end;
        (
            #typed_map_field{kind = exact, key_type = KeyType, val_type = ValueType},
            {FieldsAcc, DataAcc}
        ) ->
            case map_typed_field_to_json(TypeInfo, KeyType, ValueType, DataAcc) of
                {ok, {[], _}} ->
                    ExpectedFieldType = #typed_map_field{
                        kind = exact, key_type = KeyType, val_type = ValueType
                    },
                    NoExactMatch =
                        #sp_error{
                            type = exact_field_mismatch,
                            location = [],
                            msg = undefined,
                            input = undefined,
                            ctx = #{expected_type => ExpectedFieldType},
                            url = undefined
                        },
                    {error, [NoExactMatch]};
                {ok, {NewFields, NewDataAcc}} ->
                    {ok, {NewFields ++ FieldsAcc, NewDataAcc}};
                {error, _} = Err ->
                    Err
            end;
        (
            #literal_map_field{
                kind = exact, name = FieldName, binary_name = BinaryFieldName, val_type = FieldType
            },
            {FieldsAcc, DataAcc}
        ) ->
            case
                {maps:take(FieldName, DataAcc), spectra_type:can_be_missing(TypeInfo, FieldType)}
            of
                {{MissingValue, NewDataAcc}, {true, MissingValue}} ->
                    {ok, {FieldsAcc, NewDataAcc}};
                {{FieldData, NewDataAcc}, _} ->
                    case do_to_json(TypeInfo, FieldType, FieldData) of
                        {ok, FieldJson} ->
                            {ok, {[{BinaryFieldName, FieldJson}] ++ FieldsAcc, NewDataAcc}};
                        {error, Errs} ->
                            Errs2 =
                                lists:map(
                                    fun(Err) -> err_append_location(Err, FieldName) end,
                                    Errs
                                ),
                            {error, Errs2}
                    end;
                {error, _} ->
                    case spectra_type:can_be_missing(TypeInfo, FieldType) of
                        {true, _} ->
                            {ok, {FieldsAcc, DataAcc}};
                        false ->
                            {error, [missing_field_error(FieldName, [])]}
                    end
            end
    end,
    case spectra_util:fold_until_error(Fun, {[], Data}, MapFieldTypes) of
        {ok, {MapFields, _FinalData}} ->
            {ok, MapFields};
        % TODO: Add config option to optionally error on extra fields
        % case maps:to_list(FinalData) of
        %     [] ->
        %         {ok, MapFields};
        %     L ->
        %         {error,
        %             lists:map(
        %                 fun({Key, Value}) ->
        %                     #sp_error{
        %                         type = not_matched_fields,
        %                         location = [],
        %                         ctx = #{key => Key, value => Value}
        %                     }
        %                 end,
        %                 L
        %             )}
        % end;
        {error, _} = Err ->
            Err
    end.

-spec map_typed_field_to_json(
    TypeInfo :: spectra:type_info(),
    KeyType :: spectra:sp_type(),
    ValueType :: spectra:sp_type(),
    Data :: map()
) ->
    {ok, {[{json:encode_value(), json:encode_value()}], map()}}
    | {error, [spectra:error()]}.
map_typed_field_to_json(TypeInfo, KeyType, ValueType, Data) ->
    Fun = fun({Key, Value}, {FieldsAcc, DataAcc}) ->
        case do_to_json(TypeInfo, KeyType, Key) of
            {ok, KeyJson} ->
                case {Value, spectra_type:can_be_missing(TypeInfo, ValueType)} of
                    {MissingValue, {true, MissingValue}} ->
                        {ok, {FieldsAcc, maps:remove(Key, DataAcc)}};
                    _ ->
                        case do_to_json(TypeInfo, ValueType, Value) of
                            {ok, ValueJson} ->
                                {ok, {
                                    FieldsAcc ++ [{KeyJson, ValueJson}], maps:remove(Key, DataAcc)
                                }};
                            {error, Errs} ->
                                Errs2 = lists:map(
                                    fun(Err) -> err_append_location(Err, Key) end, Errs
                                ),
                                {error, Errs2}
                        end
                end;
            {error, _Errs} ->
                {ok, {FieldsAcc, DataAcc}}
        end
    end,
    spectra_util:fold_until_error(Fun, {[], Data}, maps:to_list(Data)).

-spec record_to_json(
    TypeInfo :: spectra:type_info(),
    RecordName :: atom() | #sp_rec{},
    Record :: term(),
    TypeArgs :: [{atom(), spectra:sp_type()}]
) ->
    {ok, #{atom() => json:encode_value()}} | {error, [spectra:error()]}.
record_to_json(TypeInfo, RecordName, Record, TypeArgs) when is_atom(RecordName) ->
    {ok, RecordInfo} = spectra_type_info:get_record(TypeInfo, RecordName),
    record_to_json(TypeInfo, RecordInfo, Record, TypeArgs);
record_to_json(
    TypeInfo,
    #sp_rec{
        name = RecordName,
        fields = Fields,
        arity = Arity
    },
    Record,
    TypeArgs
) when
    is_tuple(Record) andalso
        element(1, Record) =:= RecordName andalso
        tuple_size(Record) =:= Arity
->
    [RecordName | FieldsData] = tuple_to_list(Record),
    RecFieldTypes = record_replace_vars(Fields, TypeArgs),
    RecFieldTypesWithData = lists:zip(RecFieldTypes, FieldsData),
    do_record_to_json(TypeInfo, RecFieldTypesWithData);
record_to_json(_TypeInfo, RecordName, Record, _TypeArgs) ->
    {error, [
        #sp_error{
            type = {type_error, record},
            location = [],
            msg = undefined,
            input = Record,
            ctx = #{record_name => RecordName},
            url = undefined
        }
    ]}.

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

-spec do_record_to_json(
    spectra:type_info(),
    [{#sp_rec_field{}, Value :: term()}]
) ->
    {ok, #{atom() => json}} | {error, [spectra:error()]}.
do_record_to_json(TypeInfo, RecFieldTypesWithData) ->
    Fun = fun(
        {
            #sp_rec_field{name = FieldName, binary_name = BinaryFieldName, type = FieldType},
            RecordFieldData
        },
        FieldsAcc
    ) ->
        case {RecordFieldData, spectra_type:can_be_missing(TypeInfo, FieldType)} of
            {MissingValue, {true, MissingValue}} ->
                {ok, FieldsAcc};
            _ ->
                case do_to_json(TypeInfo, FieldType, RecordFieldData) of
                    {ok, FieldJson} ->
                        {ok, [{BinaryFieldName, FieldJson}] ++ FieldsAcc};
                    {error, Errors} ->
                        {error,
                            lists:map(
                                fun(Error) -> err_append_location(Error, FieldName) end, Errors
                            )}
                end
        end
    end,

    case spectra_util:fold_until_error(Fun, [], RecFieldTypesWithData) of
        {ok, Fields} ->
            {ok, maps:from_list(Fields)};
        {error, _} = Err ->
            Err
    end.

err_append_location(Err, FieldName) ->
    Err#sp_error{location = [FieldName | Err#sp_error.location]}.

-spec from_json(
    TypeInfo :: spectra:type_info() | module(),
    Type :: spectra:sp_type_or_ref(),
    Json :: json:decode_value()
) ->
    {ok, term()} | {error, [spectra:error()]}.
from_json(Module, Type, Json) when is_atom(Module) ->
    TypeInfo = spectra_module_types:get(Module),
    do_from_json(TypeInfo, Type, Json);
from_json(TypeInfo, Type, Json) ->
    do_from_json(TypeInfo, Type, Json).

-spec do_from_json(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type_or_ref(),
    Json :: json:decode_value()
) ->
    {ok, term()} | {error, [spectra:error()]}.
do_from_json(TypeInfo, {record, RecordName}, Json) when is_atom(RecordName) ->
    record_from_json(TypeInfo, RecordName, Json, []);
do_from_json(TypeInfo, #sp_rec{} = Rec, Json) ->
    record_from_json(TypeInfo, Rec, Json, []);
do_from_json(_TypeInfo, #sp_remote_type{mfargs = {Module, TypeName, Args}}, Data) ->
    TypeInfo = spectra_module_types:get(Module),
    TypeArity = length(Args),
    {ok, Type} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, Args),
    do_from_json(TypeInfo, TypeWithoutVars, Data);
do_from_json(
    TypeInfo,
    #sp_rec_ref{record_name = RecordName, field_types = TypeArgs},
    Json
) when
    is_atom(RecordName)
->
    record_from_json(TypeInfo, RecordName, Json, TypeArgs);
do_from_json(
    TypeInfo, #sp_map{fields = Fields, struct_name = StructName}, Json
) ->
    case map_from_json(TypeInfo, Fields, Json) of
        {ok, MapResult} when StructName =/= undefined ->
            %% Add back the __struct__ field for Elixir structs
            {ok, maps:put('__struct__', StructName, MapResult)};
        Result ->
            Result
    end;
do_from_json(
    TypeInfo,
    #sp_user_type_ref{type_name = TypeName, variables = TypeArgs},
    Json
) when
    is_atom(TypeName)
->
    type_from_json(TypeInfo, TypeName, length(TypeArgs), TypeArgs, Json);
do_from_json(TypeInfo, #sp_nonempty_list{type = Type}, Data) ->
    nonempty_list_from_json(TypeInfo, Type, Data);
do_from_json(TypeInfo, #sp_list{type = Type}, Data) ->
    list_from_json(TypeInfo, Type, Data);
do_from_json(_TypeInfo, #sp_simple_type{type = NotSupported} = T, _Value) when
    NotSupported =:= pid orelse
        NotSupported =:= port orelse
        NotSupported =:= reference orelse
        NotSupported =:= bitstring orelse
        NotSupported =:= nonempty_bitstring orelse
        NotSupported =:= none
->
    erlang:error({type_not_supported, T});
do_from_json(_TypeInfo, #sp_simple_type{type = PrimaryType} = T, Json) ->
    case check_type_from_json(PrimaryType, Json) of
        {true, NewValue} ->
            {ok, NewValue};
        {error, Reason} ->
            {error, Reason};
        false ->
            PrimType = simple_type_to_prim_atom(PrimaryType),
            {error, [type_error(PrimType, Json, T, [])]}
    end;
do_from_json(_TypeInfo, #sp_literal{value = Literal}, Literal) ->
    {ok, Literal};
do_from_json(_TypeInfo, #sp_literal{} = Type, Value) ->
    case try_convert_to_literal(Type, Value) of
        {ok, Literal} ->
            {ok, Literal};
        false ->
            {error, [
                #sp_error{
                    type = literal_no_match,
                    location = [],
                    msg = undefined,
                    input = Value,
                    ctx = #{expected_type => Type},
                    url = undefined
                }
            ]}
    end;
do_from_json(TypeInfo, {type, TypeName, TypeArity}, Json) when is_atom(TypeName) ->
    type_from_json(TypeInfo, TypeName, TypeArity, [], Json);
do_from_json(TypeInfo, #sp_union{} = Type, Json) ->
    union(fun do_from_json/3, TypeInfo, Type, Json);
do_from_json(
    _TypeInfo,
    #sp_range{
        type = integer,
        lower_bound = Min,
        upper_bound = Max
    },
    Value
) when
    Min =< Value, Value =< Max, is_integer(Value)
->
    {ok, Value};
do_from_json(
    _TypeInfo,
    #sp_range{
        type = integer,
        lower_bound = Min,
        upper_bound = Max
    } =
        Range,
    Value
) when
    is_integer(Value)
->
    %% Value is an integer but out of range
    ErrorType =
        if
            Value < Min -> {constraint_error, too_small};
            Value > Max -> {constraint_error, too_large};
            % Shouldn't happen but fallback
            true -> {type_error, int}
        end,
    {error, [
        #sp_error{
            type = ErrorType,
            location = [],
            msg = undefined,
            input = Value,
            ctx = #{expected_type => Range},
            url = undefined
        }
    ]};
do_from_json(_TypeInfo, #sp_maybe_improper_list{} = Type, _Value) ->
    erlang:error({type_not_implemented, Type});
do_from_json(_TypeInfo, #sp_nonempty_improper_list{} = Type, _Value) ->
    erlang:error({type_not_implemented, Type});
do_from_json(_TypeInfo, #sp_function{} = Type, _Value) ->
    erlang:error({type_not_supported, Type});
do_from_json(_TypeInfo, #sp_tuple{} = Type, _Value) ->
    erlang:error({type_not_supported, Type});
do_from_json(_TypeInfo, _Type, Value) ->
    %% Generic fallback error
    {error, [
        #sp_error{
            type = {type_error, unknown},
            location = [],
            msg = undefined,
            input = Value,
            ctx = #{},
            url = undefined
        }
    ]}.

-spec try_convert_to_literal(
    Type :: #sp_literal{},
    Value :: term()
) -> {ok, integer() | atom() | []} | false.
try_convert_to_literal(
    #sp_literal{value = LiteralValue}, null
) when LiteralValue =:= nil orelse LiteralValue =:= undefined ->
    {ok, LiteralValue};
try_convert_to_literal(
    #sp_literal{value = LiteralValue, binary_value = BinaryLiteralValue}, Value
) when Value =:= BinaryLiteralValue ->
    {ok, LiteralValue};
try_convert_to_literal(#sp_literal{}, _Value) ->
    false.

nonempty_list_from_json(TypeInfo, Type, Data) when is_list(Data) andalso Data =/= [] ->
    list_from_json(TypeInfo, Type, Data);
nonempty_list_from_json(_TypeInfo, Type, Data) ->
    NonEmptyListType = #sp_nonempty_list{type = Type},
    {error, [type_error(nonempty_list, Data, NonEmptyListType, [])]}.

list_from_json(TypeInfo, Type, Data) when is_list(Data) ->
    Fun = fun({Nr, Item}) ->
        case do_from_json(TypeInfo, Type, Item) of
            {ok, Json} ->
                {ok, Json};
            {error, Errs} ->
                Errs2 = lists:map(fun(Err) -> err_append_location(Err, Nr) end, Errs),

                {error, Errs2}
        end
    end,
    spectra_util:map_until_error(Fun, lists:enumerate(Data));
list_from_json(_TypeInfo, Type, Data) ->
    ListType = #sp_list{type = Type},
    {error, [type_error(list, Data, ListType, [])]}.

string_from_json(Type, Json) ->
    case unicode:characters_to_list(Json) of
        StringValue when is_list(StringValue) ->
            {true, StringValue};
        _Other ->
            ExpectedType = #sp_simple_type{type = Type},
            {error, [
                #sp_error{
                    type = {parse_error, string},
                    location = [],
                    msg = undefined,
                    input = Json,
                    ctx = #{expected_type => ExpectedType},
                    url = undefined
                }
            ]}
    end.

check_type_from_json(string, Json) when is_binary(Json) ->
    string_from_json(string, Json);
check_type_from_json(nonempty_string, Json) when is_binary(Json), byte_size(Json) > 0 ->
    string_from_json(nonempty_string, Json);
check_type_from_json(iodata, Json) when is_binary(Json) ->
    {true, Json};
check_type_from_json(iolist, Json) when is_binary(Json) ->
    {true, [Json]};
check_type_from_json(atom, Json) when is_binary(Json) ->
    try
        {true, binary_to_existing_atom(Json, utf8)}
    catch
        error:badarg ->
            false
    end;
check_type_from_json(Type, Json) ->
    check_type(Type, Json).

check_type_to_json(iodata, Json) when is_binary(Json) ->
    {true, Json};
check_type_to_json(iodata, Json) when is_list(Json) ->
    {true, iolist_to_binary(Json)};
check_type_to_json(iolist, Json) when is_list(Json) ->
    {true, iolist_to_binary(Json)};
check_type_to_json(nonempty_string, Json) when is_list(Json), Json =/= [] ->
    case unicode:characters_to_binary(Json) of
        {Err, _, _} when Err =:= error orelse Err =:= incomplete ->
            ExpectedType = #sp_simple_type{type = nonempty_string},
            {error, [
                #sp_error{
                    type = {parse_error, string},
                    location = [],
                    msg = undefined,
                    input = Json,
                    ctx = #{expected_type => ExpectedType},
                    url = undefined
                }
            ]};
        Bin when is_binary(Bin) ->
            {true, Bin}
    end;
check_type_to_json(string, Json) when is_list(Json) ->
    case unicode:characters_to_binary(Json) of
        {Err, _, _} when Err =:= error orelse Err =:= incomplete ->
            ExpectedType = #sp_simple_type{type = string},
            {error, [
                #sp_error{
                    type = {parse_error, string},
                    location = [],
                    msg = undefined,
                    input = Json,
                    ctx = #{expected_type => ExpectedType},
                    url = undefined
                }
            ]};
        Bin when is_binary(Bin) ->
            {true, Bin}
    end;
check_type_to_json(Type, Json) ->
    check_type(Type, Json).

check_type(integer, Json) when is_integer(Json) ->
    {true, Json};
check_type(boolean, Json) when is_boolean(Json) ->
    {true, Json};
check_type(float, Json) when is_float(Json) ->
    {true, Json};
check_type(number, Json) when is_integer(Json) orelse is_float(Json) ->
    {true, Json};
check_type(non_neg_integer, Json) when is_integer(Json) andalso Json >= 0 ->
    {true, Json};
check_type(pos_integer, Json) when is_integer(Json) andalso Json > 0 ->
    {true, Json};
check_type(neg_integer, Json) when is_integer(Json) andalso Json < 0 ->
    {true, Json};
check_type(binary, Json) when is_binary(Json) ->
    {true, Json};
check_type(nonempty_binary, Json) when is_binary(Json), byte_size(Json) > 0 ->
    {true, Json};
check_type(atom, Json) when is_atom(Json) ->
    {true, Json};
check_type(term, Json) ->
    {true, Json};
check_type(_Type, _Json) ->
    false.

union(Fun, TypeInfo, #sp_union{types = Types} = UnionType, Json) ->
    case do_first(Fun, TypeInfo, Types, Json) of
        {error, no_match} ->
            {error, [
                #sp_error{
                    type = union_no_match,
                    location = [],
                    msg = undefined,
                    input = Json,
                    ctx = #{expected_type => UnionType},
                    url = undefined
                }
            ]};
        Result ->
            Result
    end.

do_first(_Fun, _TypeInfo, [], _Json) ->
    {error, no_match};
do_first(Fun, TypeInfo, [Type | Rest], Json) ->
    case Fun(TypeInfo, Type, Json) of
        {ok, Result} ->
            {ok, Result};
        {error, _} ->
            do_first(Fun, TypeInfo, Rest, Json)
    end.

-spec type_from_json(
    TypeInfo :: spectra:type_info(),
    TypeName :: atom(),
    TypeArity :: non_neg_integer(),
    TypeArgs :: [spectra:sp_type()],
    Json :: json:decode_value()
) ->
    {ok, term()} | {error, [spectra:error()]}.
type_from_json(TypeInfo, TypeName, TypeArity, TypeArgs, Json) ->
    {ok, Type} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, TypeArgs),
    do_from_json(TypeInfo, TypeWithoutVars, Json).

apply_args(TypeInfo, Type, TypeArgs) when is_list(TypeArgs) ->
    ArgNames = arg_names(Type),
    NamedTypes =
        maps:from_list(
            lists:zip(ArgNames, TypeArgs)
        ),
    type_replace_vars(TypeInfo, Type, NamedTypes).

arg_names(#sp_type_with_variables{vars = Args}) ->
    Args;
arg_names(_) ->
    [].

-spec type_replace_vars(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type(),
    NamedTypes :: #{atom() => spectra:sp_type()}
) ->
    spectra:sp_type().
type_replace_vars(_TypeInfo, #sp_var{name = Name}, NamedTypes) ->
    maps:get(Name, NamedTypes);
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
                                %% ADD TESTS
                                Field#typed_map_field{
                                    key_type = type_replace_vars(TypeInfo, KeyType, NamedTypes),
                                    val_type = type_replace_vars(TypeInfo, ValueType, NamedTypes)
                                }
                        end,
                        Fields
                    )
            };
        #sp_rec_ref{record_name = RecordName, field_types = RefFieldTypes} ->
            {ok, #sp_rec{fields = Fields} = Rec} = spectra_type_info:get_record(
                TypeInfo, RecordName
            ),
            NewRec = Rec#sp_rec{fields = record_replace_vars(Fields, RefFieldTypes)},
            type_replace_vars(TypeInfo, NewRec, NamedTypes);
        #sp_remote_type{mfargs = {Module, TypeName, Args}} ->
            TypeInfo = spectra_module_types:get(Module),
            TypeArity = length(Args),
            {ok, Type} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
            type_replace_vars(TypeInfo, Type, NamedTypes);
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
type_replace_vars(_TypeInfo, Type, _NamedTypes) ->
    Type.

-spec map_from_json(
    spectra:type_info(),
    [spectra:map_field()],
    json:decode_value()
) ->
    {ok, #{json:encode_value() => json:encode_value()}}
    | {error, [spectra:error()]}.
map_from_json(TypeInfo, MapFieldType, Json) when is_map(Json) ->
    Fun = fun
        (
            #literal_map_field{
                kind = assoc, name = FieldName, binary_name = BinaryName, val_type = FieldType
            },
            {FieldsAcc, JsonAcc}
        ) ->
            case maps:take(BinaryName, JsonAcc) of
                {FieldData, NewJsonAcc} ->
                    case do_from_json(TypeInfo, FieldType, FieldData) of
                        {ok, FieldJson} ->
                            {ok, {[{FieldName, FieldJson}] ++ FieldsAcc, NewJsonAcc}};
                        {error, Errs} ->
                            Errs2 =
                                lists:map(
                                    fun(Err) -> err_append_location(Err, FieldName) end,
                                    Errs
                                ),
                            {error, Errs2}
                    end;
                error ->
                    {ok, {FieldsAcc, JsonAcc}}
            end;
        (
            #literal_map_field{
                kind = exact, name = FieldName, binary_name = BinaryName, val_type = FieldType
            },
            {FieldsAcc, JsonAcc}
        ) ->
            case maps:take(BinaryName, JsonAcc) of
                {FieldData, NewJsonAcc} ->
                    case do_from_json(TypeInfo, FieldType, FieldData) of
                        {ok, FieldJson} ->
                            {ok, {[{FieldName, FieldJson}] ++ FieldsAcc, NewJsonAcc}};
                        {error, Errs} ->
                            Errs2 =
                                lists:map(
                                    fun(Err) -> err_append_location(Err, FieldName) end,
                                    Errs
                                ),
                            {error, Errs2}
                    end;
                error ->
                    case spectra_type:can_be_missing(TypeInfo, FieldType) of
                        {true, MissingValue} ->
                            {ok, {[{FieldName, MissingValue}] ++ FieldsAcc, JsonAcc}};
                        false ->
                            {error, [missing_field_error(FieldName, [])]}
                    end
            end;
        (
            #typed_map_field{kind = assoc, key_type = KeyType, val_type = ValueType},
            {FieldsAcc, JsonAcc}
        ) ->
            case map_field_type_from_json(TypeInfo, KeyType, ValueType, JsonAcc) of
                {ok, {NewFields, NewJsonAcc}} ->
                    {ok, {NewFields ++ FieldsAcc, NewJsonAcc}};
                {error, Reason} ->
                    {error, Reason}
            end;
        (
            #typed_map_field{kind = exact, key_type = KeyType, val_type = ValueType},
            {FieldsAcc, JsonAcc}
        ) ->
            case map_field_type_from_json(TypeInfo, KeyType, ValueType, JsonAcc) of
                {ok, {NewFields, NewJsonAcc}} ->
                    case NewFields of
                        [] ->
                            ExpectedFieldType = #typed_map_field{
                                kind = exact,
                                key_type = KeyType,
                                val_type = ValueType
                            },
                            NoExactMatch =
                                #sp_error{
                                    type = exact_field_mismatch,
                                    location = [],
                                    msg = undefined,
                                    input = undefined,
                                    ctx = #{expected_type => ExpectedFieldType},
                                    url = undefined
                                },
                            {error, [NoExactMatch]};
                        _ ->
                            {ok, {NewFields ++ FieldsAcc, NewJsonAcc}}
                    end;
                {error, _} = Err ->
                    Err
            end
    end,

    case spectra_util:fold_until_error(Fun, {[], Json}, MapFieldType) of
        {ok, {Fields, _NotMapped}} ->
            {ok, maps:from_list(Fields)};
        % TODO: Add config option to optionally error on extra fields
        % case maps:size(NotMapped) of
        %     0 ->
        %         {ok, maps:from_list(Fields)};
        %     _ ->
        %         {error,
        %             lists:map(
        %                 fun({Key, Value}) ->
        %                     #sp_error{
        %                         type = not_matched_fields,
        %                         location = [],
        %                         ctx = #{key => Key, value => Value}
        %                     }
        %                 end,
        %                 maps:to_list(NotMapped)
        %             )}
        % end;
        {error, _} = Err ->
            Err
    end;
map_from_json(_TypeInfo, _MapFieldType, Json) ->
    %% Return error when Json is not a map
    ExpectedType = #sp_simple_type{type = map},
    {error, [type_error(map, Json, ExpectedType, [])]}.

map_field_type_from_json(TypeInfo, KeyType, ValueType, Json) ->
    spectra_util:fold_until_error(
        fun({Key, Value}, {FieldsAcc, JsonAcc}) ->
            case do_from_json(TypeInfo, KeyType, Key) of
                {ok, KeyJson} ->
                    case do_from_json(TypeInfo, ValueType, Value) of
                        {ok, ValueJson} ->
                            {ok, {FieldsAcc ++ [{KeyJson, ValueJson}], maps:remove(Key, JsonAcc)}};
                        {error, Errs} ->
                            Errs2 =
                                lists:map(
                                    fun(Err) ->
                                        err_append_location(
                                            Err,
                                            Key
                                        )
                                    end,
                                    Errs
                                ),
                            {error, Errs2}
                    end;
                {error, _Errs} ->
                    {ok, {FieldsAcc, JsonAcc}}
            end
        end,
        {[], Json},
        maps:to_list(Json)
    ).

-spec record_from_json(
    TypeInfo :: spectra:type_info(),
    RecordName :: atom() | #sp_rec{},
    Json :: json:decode_value(),
    TypeArgs :: [spectra:record_field_arg()]
) ->
    {ok, term()} | {error, list()}.
record_from_json(TypeInfo, RecordName, Json, TypeArgs) when is_atom(RecordName) ->
    {ok, Record} = spectra_type_info:get_record(TypeInfo, RecordName),
    record_from_json(TypeInfo, Record, Json, TypeArgs);
record_from_json(
    TypeInfo, #sp_rec{name = RecordName} = ARec, Json, TypeArgs
) ->
    RecordInfo = record_replace_vars(ARec#sp_rec.fields, TypeArgs),
    do_record_from_json(TypeInfo, RecordName, RecordInfo, Json).

-spec do_record_from_json(
    TypeInfo :: spectra:type_info(),
    RecordName :: atom(),
    RecordInfo :: [#sp_rec_field{}],
    Json :: json:decode_value()
) ->
    {ok, term()} | {error, list()}.
do_record_from_json(TypeInfo, RecordName, RecordInfo, Json) when is_map(Json) ->
    Fun = fun(
        #sp_rec_field{name = FieldName, binary_name = BinaryName, type = FieldType},
        {FieldsAcc, JsonAcc}
    ) ->
        case maps:take(BinaryName, JsonAcc) of
            {RecordFieldData, NewJsonAcc} ->
                case do_from_json(TypeInfo, FieldType, RecordFieldData) of
                    {ok, FieldJson} ->
                        {ok, {[FieldJson | FieldsAcc], NewJsonAcc}};
                    {error, Errs} ->
                        Errs2 =
                            lists:map(
                                fun(Err) -> err_append_location(Err, FieldName) end,
                                Errs
                            ),
                        {error, Errs2}
                end;
            error ->
                case spectra_type:can_be_missing(TypeInfo, FieldType) of
                    {true, MissingValue} ->
                        {ok, {[MissingValue | FieldsAcc], JsonAcc}};
                    false ->
                        {error, [missing_field_error(FieldName, [])]}
                end
        end
    end,
    case spectra_util:fold_until_error(Fun, {[], Json}, RecordInfo) of
        {ok, {Fields, _NotMapped}} ->
            {ok, list_to_tuple([RecordName | lists:reverse(Fields)])};
        % TODO: Add config option to optionally error on extra fields
        % case maps:size(NotMapped) of
        %     0 ->
        %         {ok, list_to_tuple([RecordName | lists:reverse(Fields)])};
        %     _ ->
        %         {error,
        %             lists:map(
        %                 fun({Key, Value}) ->
        %                     #sp_error{
        %                         type = not_matched_fields,
        %                         location = [],
        %                         ctx = #{key => Key, value => Value}
        %                     }
        %                 end,
        %                 maps:to_list(NotMapped)
        %             )}
        % end;
        {error, Errs} ->
            {error, Errs}
    end;
do_record_from_json(_TypeInfo, RecordName, _RecordInfo, Json) ->
    ExpectedType = #sp_rec{name = RecordName, fields = [], arity = 0},
    {error, [type_error(record, Json, ExpectedType, [])]}.

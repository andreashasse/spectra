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

%% INTERNAL
-spec do_to_json(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type_or_ref(),
    Data :: dynamic()
) ->
    {ok, json:encode_value()} | {error, [spectra:error()]}.
%% Unwrap annotated types - they carry source info but should be processed as their inner type
do_to_json(TypeInfo, #sp_annotated_type{type = InnerType}, Data) ->
    do_to_json(TypeInfo, InnerType, Data);
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
    Type = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
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
do_to_json(TypeInfo, #sp_nonempty_list{} = Type, Data) ->
    nonempty_list_to_json(TypeInfo, Type, Data);
do_to_json(TypeInfo, #sp_list{type = Type}, Data) when is_list(Data) ->
    list_to_json(TypeInfo, Type, Data);
do_to_json(TypeInfo, {type, TypeName, TypeArity}, Data) when is_atom(TypeName) ->
    %% FIXME: For simple types without arity, default to 0
    Type = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
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
                    {error, [sp_error:type_mismatch(Map, Data, #{message => "Struct mismatch"})]}
            end
    end;
do_to_json(_TypeInfo, #sp_remote_type{mfargs = {Module, TypeName, Args}}, Data) ->
    TypeInfo = spectra_module_types:get(Module),
    TypeArity = length(Args),
    Type = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, Args),
    do_to_json(TypeInfo, TypeWithoutVars, Data);
do_to_json(_TypeInfo, #sp_maybe_improper_list{} = Type, _Data) ->
    erlang:error({type_not_supported, Type});
do_to_json(_TypeInfo, #sp_nonempty_improper_list{} = Type, _Data) ->
    erlang:error({type_not_supported, Type});
do_to_json(_TypeInfo, #sp_tuple{} = Type, _Data) ->
    erlang:error({type_not_supported, Type});
do_to_json(_TypeInfo, #sp_function{} = Type, _Data) ->
    erlang:error({type_not_supported, Type});
do_to_json(_TypeInfo, Type, OtherValue) ->
    {error, [sp_error:type_mismatch(Type, OtherValue)]}.

-spec prim_type_to_json(Type :: spectra:sp_type(), Value :: dynamic()) ->
    {ok, json:encode_value()} | {error, [spectra:error()]}.
prim_type_to_json(#sp_simple_type{type = Type} = T, Value) ->
    case check_type_to_json(Type, Value) of
        {true, NewValue} ->
            {ok, NewValue};
        {error, Reason} ->
            {error, Reason};
        false ->
            {error, [sp_error:type_mismatch(T, Value)]}
    end.

nonempty_list_to_json(TypeInfo, #sp_nonempty_list{type = Type}, Data) when
    is_list(Data) andalso Data =/= []
->
    list_to_json(TypeInfo, Type, Data);
nonempty_list_to_json(_TypeInfo, Type, Data) ->
    {error, [sp_error:type_mismatch(Type, Data)]}.

-spec list_to_json(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type_or_ref(),
    Data :: [dynamic()]
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
                            fun(Err) -> sp_error:append_location(Err, Nr) end,
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
map_to_json(_TypeInfo, MapType, Data) ->
    {error, [sp_error:type_mismatch(MapType, Data)]}.

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
                                    fun(Err) -> sp_error:append_location(Err, FieldName) end,
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
            #typed_map_field{kind = exact, key_type = KeyType, val_type = ValueType} = Type,
            {FieldsAcc, DataAcc}
        ) ->
            case map_typed_field_to_json(TypeInfo, KeyType, ValueType, DataAcc) of
                {ok, {[], _}} ->
                    {error, [sp_error:not_matched_fields(Type, DataAcc)]};
                {ok, {NewFields, NewDataAcc}} ->
                    {ok, {NewFields ++ FieldsAcc, NewDataAcc}};
                {error, _} = Err ->
                    Err
            end;
        (
            #literal_map_field{
                kind = exact, name = FieldName, binary_name = BinaryFieldName, val_type = FieldType
            } = Type,
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
                                    fun(Err) -> sp_error:append_location(Err, FieldName) end,
                                    Errs
                                ),
                            {error, Errs2}
                    end;
                {error, _} ->
                    case spectra_type:can_be_missing(TypeInfo, FieldType) of
                        {true, _} ->
                            {ok, {FieldsAcc, DataAcc}};
                        false ->
                            {error, [sp_error:missing_data(Type, DataAcc, [FieldName])]}
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
                                    fun(Err) -> sp_error:append_location(Err, Key) end, Errs
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
    Record :: dynamic(),
    TypeArgs :: [{atom(), spectra:sp_type()}]
) ->
    {ok, #{atom() => json:encode_value()}} | {error, [spectra:error()]}.
record_to_json(TypeInfo, RecordName, Record, TypeArgs) when is_atom(RecordName) ->
    case spectra_type_info:find_record(TypeInfo, RecordName) of
        {ok, RecordInfo} ->
            record_to_json(TypeInfo, RecordInfo, Record, TypeArgs);
        error ->
            erlang:error({record_not_found, RecordName})
    end;
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
    RecFieldTypes = spectra_util:record_replace_vars(Fields, TypeArgs),
    RecFieldTypesWithData = lists:zip(RecFieldTypes, FieldsData),
    do_record_to_json(TypeInfo, RecFieldTypesWithData);
record_to_json(_TypeInfo, RecordType, Record, TypeArgs) ->
    {error, [sp_error:type_mismatch(RecordType, Record, #{type_args => TypeArgs})]}.

-spec do_record_to_json(
    spectra:type_info(),
    [{#sp_rec_field{}, Value :: dynamic()}]
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
                                fun(Error) -> sp_error:append_location(Error, FieldName) end, Errors
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

-spec from_json(
    TypeInfo :: spectra:type_info() | module(),
    Type :: spectra:sp_type_or_ref(),
    Json :: json:decode_value()
) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
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
    {ok, dynamic()} | {error, [spectra:error()]}.
%% Unwrap annotated types - they carry source info but should be processed as their inner type
do_from_json(TypeInfo, #sp_annotated_type{type = InnerType}, Json) ->
    do_from_json(TypeInfo, InnerType, Json);
do_from_json(TypeInfo, {record, RecordName}, Json) when is_atom(RecordName) ->
    record_from_json(TypeInfo, RecordName, Json, []);
do_from_json(TypeInfo, #sp_rec{} = Rec, Json) ->
    record_from_json(TypeInfo, Rec, Json, []);
do_from_json(_TypeInfo, #sp_remote_type{mfargs = {Module, TypeName, Args}}, Data) ->
    TypeInfo = spectra_module_types:get(Module),
    TypeArity = length(Args),
    Type = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
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
    TypeInfo, #sp_map{struct_name = StructName} = Type, Json
) ->
    case map_from_json(TypeInfo, Type, Json) of
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
do_from_json(TypeInfo, #sp_nonempty_list{} = Type, Data) ->
    nonempty_list_from_json(TypeInfo, Type, Data);
do_from_json(TypeInfo, #sp_list{type = ListType} = Type, Data) ->
    list_from_json(TypeInfo, ListType, Data, Type);
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
            {error, [sp_error:type_mismatch(T, Json)]}
    end;
do_from_json(_TypeInfo, #sp_literal{value = Literal}, Literal) ->
    {ok, Literal};
do_from_json(_TypeInfo, #sp_literal{} = Type, Value) ->
    case try_convert_to_literal(Type, Value) of
        {ok, Literal} ->
            {ok, Literal};
        false ->
            {error, [sp_error:type_mismatch(Type, Value)]}
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
        lower_bound = _Min,
        upper_bound = _Max
    } =
        Range,
    Value
) when
    is_integer(Value)
->
    {error, [sp_error:type_mismatch(Range, Value)]};
do_from_json(_TypeInfo, #sp_maybe_improper_list{} = Type, _Value) ->
    erlang:error({type_not_supported, Type});
do_from_json(_TypeInfo, #sp_nonempty_improper_list{} = Type, _Value) ->
    erlang:error({type_not_supported, Type});
do_from_json(_TypeInfo, #sp_function{} = Type, _Value) ->
    erlang:error({type_not_supported, Type});
do_from_json(_TypeInfo, #sp_tuple{} = Type, _Value) ->
    erlang:error({type_not_supported, Type});
do_from_json(_TypeInfo, Type, Value) ->
    {error, [sp_error:type_mismatch(Type, Value)]}.

-spec try_convert_to_literal(
    Type :: #sp_literal{},
    Value :: dynamic()
) -> {ok, integer() | atom() | []} | false.
try_convert_to_literal(
    #sp_literal{value = []}, _Value
) ->
    false;
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

nonempty_list_from_json(TypeInfo, #sp_nonempty_list{type = ListType} = Type, Data) when
    is_list(Data) andalso Data =/= []
->
    list_from_json(TypeInfo, ListType, Data, Type);
nonempty_list_from_json(_TypeInfo, Type, Data) ->
    {error, [sp_error:type_mismatch(Type, Data)]}.

list_from_json(TypeInfo, Type, Data, _) when is_list(Data) ->
    Fun = fun({Nr, Item}) ->
        case do_from_json(TypeInfo, Type, Item) of
            {ok, Json} ->
                {ok, Json};
            {error, Errs} ->
                Errs2 = lists:map(fun(Err) -> sp_error:append_location(Err, Nr) end, Errs),

                {error, Errs2}
        end
    end,
    spectra_util:map_until_error(Fun, lists:enumerate(Data));
list_from_json(_TypeInfo, _ListType, Data, Type) ->
    {error, [sp_error:type_mismatch(Type, Data)]}.

string_from_json(Type, Json) ->
    case unicode:characters_to_list(Json) of
        StringValue when is_list(StringValue) ->
            {true, StringValue};
        _Other ->
            {error, [
                sp_error:type_mismatch(#sp_simple_type{type = Type}, Json, #{
                    message => "unicode conversion failed"
                })
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
check_type_from_json(map, Json) when is_map(Json) ->
    {true, Json};
check_type_from_json(Type, Json) ->
    check_type(Type, Json).

check_type_to_json(iodata, Json) when is_binary(Json) ->
    {true, Json};
check_type_to_json(iodata, Json) when is_list(Json) ->
    {true, iolist_to_binary(Json)};
check_type_to_json(iolist, Json) when is_list(Json) ->
    {true, iolist_to_binary(Json)};
check_type_to_json(nonempty_string, Json) when is_list(Json), Json =/= [] ->
    do_string_to_json(nonempty_string, Json);
check_type_to_json(string, Json) when is_list(Json) ->
    do_string_to_json(string, Json);
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
check_type(map, Json) when is_map(Json) ->
    {true, Json};
check_type(_Type, _Json) ->
    false.

do_string_to_json(Type, Json) ->
    case unicode:characters_to_binary(Json) of
        {Err, _, _} when Err =:= error orelse Err =:= incomplete ->
            {error, [
                sp_error:type_mismatch(#sp_simple_type{type = Type}, Json, #{
                    message => "non printable"
                })
            ]};
        Bin when is_binary(Bin) ->
            {true, Bin}
    end.

union(Fun, TypeInfo, #sp_union{types = Types} = T, Json) ->
    case do_first(Fun, TypeInfo, Types, Json, []) of
        {error, UnionErrors} ->
            {error, [sp_error:no_match(T, Json, UnionErrors)]};
        Result ->
            Result
    end.

do_first(_Fun, _TypeInfo, [], _Json, Errors) ->
    {error, Errors};
do_first(Fun, TypeInfo, [Type | Rest], Json, ErrorsAcc) ->
    case Fun(TypeInfo, Type, Json) of
        {ok, Result} ->
            {ok, Result};
        {error, Errors} ->
            do_first(Fun, TypeInfo, Rest, Json, [{Type, Errors} | ErrorsAcc])
    end.

-spec type_from_json(
    TypeInfo :: spectra:type_info(),
    TypeName :: atom(),
    TypeArity :: non_neg_integer(),
    TypeArgs :: [spectra:sp_type()],
    Json :: json:decode_value()
) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
type_from_json(TypeInfo, TypeName, TypeArity, TypeArgs, Json) ->
    Type = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, TypeArgs),
    do_from_json(TypeInfo, TypeWithoutVars, Json).

apply_args(TypeInfo, Type, TypeArgs) when is_list(TypeArgs) ->
    ArgNames = arg_names(Type),
    NamedTypes =
        maps:from_list(
            lists:zip(ArgNames, TypeArgs)
        ),
    spectra_util:type_replace_vars(TypeInfo, Type, NamedTypes).

arg_names(#sp_type_with_variables{vars = Args}) ->
    Args;
arg_names(_) ->
    [].

-spec map_from_json(
    spectra:type_info(),
    #sp_map{},
    json:decode_value()
) ->
    {ok, #{json:encode_value() => json:encode_value()}}
    | {error, [spectra:error()]}.
map_from_json(TypeInfo, #sp_map{fields = MapFieldType}, Json) when is_map(Json) ->
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
                                    fun(Err) -> sp_error:append_location(Err, FieldName) end,
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
            } = Type,
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
                                    fun(Err) -> sp_error:append_location(Err, FieldName) end,
                                    Errs
                                ),
                            {error, Errs2}
                    end;
                error ->
                    case spectra_type:can_be_missing(TypeInfo, FieldType) of
                        {true, MissingValue} ->
                            {ok, {[{FieldName, MissingValue}] ++ FieldsAcc, JsonAcc}};
                        false ->
                            {error, [sp_error:missing_data(Type, JsonAcc, [FieldName])]}
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
            #typed_map_field{kind = exact, key_type = KeyType, val_type = ValueType} = Type,
            {FieldsAcc, JsonAcc}
        ) ->
            case map_field_type_from_json(TypeInfo, KeyType, ValueType, JsonAcc) of
                {ok, {NewFields, NewJsonAcc}} ->
                    case NewFields of
                        [] ->
                            {error, [sp_error:not_matched_fields(Type, JsonAcc)]};
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
map_from_json(_TypeInfo, MapType, Json) ->
    %% Return error when Json is not a map
    {error, [sp_error:type_mismatch(MapType, Json)]}.

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
                                        sp_error:append_location(
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
    {ok, dynamic()} | {error, list()}.
record_from_json(TypeInfo, RecordName, Json, TypeArgs) when is_atom(RecordName) ->
    case spectra_type_info:find_record(TypeInfo, RecordName) of
        {ok, Record} ->
            record_from_json(TypeInfo, Record, Json, TypeArgs);
        error ->
            error({record_not_found, RecordName})
    end;
record_from_json(
    TypeInfo, #sp_rec{} = ARec, Json, TypeArgs
) ->
    RecordInfo = spectra_util:record_replace_vars(ARec#sp_rec.fields, TypeArgs),
    NewRec = ARec#sp_rec{fields = RecordInfo},
    do_record_from_json(TypeInfo, NewRec, Json).

-spec do_record_from_json(
    TypeInfo :: spectra:type_info(),
    #sp_rec{},
    Json :: json:decode_value()
) ->
    {ok, dynamic()} | {error, list()}.
do_record_from_json(TypeInfo, #sp_rec{name = RecordName, fields = RecordInfo}, Json) when
    is_map(Json)
->
    Fun = fun(
        #sp_rec_field{name = FieldName, binary_name = BinaryName, type = FieldType} = Type,
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
                                fun(Err) -> sp_error:append_location(Err, FieldName) end,
                                Errs
                            ),
                        {error, Errs2}
                end;
            error ->
                case spectra_type:can_be_missing(TypeInfo, FieldType) of
                    {true, MissingValue} ->
                        {ok, {[MissingValue | FieldsAcc], JsonAcc}};
                    false ->
                        {error, [sp_error:missing_data(Type, JsonAcc, [FieldName])]}
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
do_record_from_json(_TypeInfo, Type, Json) ->
    {error, [sp_error:type_mismatch(Type, Json)]}.

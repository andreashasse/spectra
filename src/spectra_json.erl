-module(spectra_json).

-moduledoc """
JSON encode and decode traversal for `sp_type()` values.

`to_json/3` walks an Erlang term against an `sp_type()` and produces a
`json:encode_value()` (a map/list/scalar tree accepted by `json:encode/1`).
`from_json/3` does the inverse: it walks a decoded JSON value and produces
the corresponding Erlang term.

Neither function calls `json:encode/1` or `json:decode/1` — that is the
responsibility of the caller (typically `spectra.erl`).

Codec dispatch is handled mid-traversal at `#sp_user_type_ref{}`,
`#sp_remote_type{}`, and `#sp_rec_ref{}` nodes via
`spectra_codec:try_codec_encode/5` and `try_codec_decode/5`.
""".

-export([to_json/3, from_json/3]).

-ignore_xref([to_json/3, from_json/3]).

-include("../include/spectra_internal.hrl").

-doc """
Encodes `Data` to a JSON-compatible value according to `Type`.

Walks the type tree recursively, converting each node. Records become maps
with binary keys, lists stay lists, atoms in unions become their binary
representations. Optional fields whose value equals the missing sentinel
(`nil` / `undefined`) are omitted from the output.

Returns `{ok, Value}` on success or `{error, Errors}` with a list of
structured `#sp_error{}` values describing every mismatch found.
""".
-spec to_json(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type(),
    Data :: dynamic()
) ->
    {ok, json:encode_value()} | {error, [spectra:error()]}.
to_json(
    TypeInfo,
    #sp_user_type_ref{type_name = TypeName, variables = Args, arity = Arity} = UserTypeRef,
    Data
) when
    is_atom(TypeName)
->
    Mod = spectra_type_info:get_module(TypeInfo),
    Type = spectra_type_info:get_type(TypeInfo, TypeName, Arity),
    case spectra_codec:try_codec_encode(Mod, json, Type, Data, UserTypeRef) of
        continue ->
            TypeWithoutVars = apply_args(TypeInfo, Type, Args),
            to_json(TypeInfo, TypeWithoutVars, Data);
        Result ->
            Result
    end;
to_json(
    _TypeInfo,
    #sp_remote_type{mfargs = {Module, TypeName, Args}, arity = TypeArity} = RemoteRef,
    Data
) ->
    RemoteTypeInfo = spectra_module_types:get(Module),
    RemoteType = spectra_type_info:get_type(RemoteTypeInfo, TypeName, TypeArity),
    case spectra_codec:try_codec_encode(Module, json, RemoteType, Data, RemoteRef) of
        continue ->
            TypeResolved = spectra_type:propagate_params(
                RemoteRef, apply_args(RemoteTypeInfo, RemoteType, Args)
            ),
            to_json(RemoteTypeInfo, TypeResolved, Data);
        Result ->
            Result
    end;
to_json(TypeInfo, #sp_rec{} = RecordInfo, Record) when is_tuple(Record) ->
    record_to_json(TypeInfo, RecordInfo, Record, []);
to_json(
    TypeInfo,
    #sp_rec_ref{record_name = RecordName, field_types = TypeArgs} = RecordRef,
    Record
) when
    is_atom(RecordName)
->
    Mod = spectra_type_info:get_module(TypeInfo),
    RecordType = spectra_type_info:get_record(TypeInfo, RecordName),
    case spectra_codec:try_codec_encode(Mod, json, RecordType, Record, RecordRef) of
        continue -> record_to_json(TypeInfo, RecordType, Record, TypeArgs);
        Result -> Result
    end;
to_json(_TypeInfo, #sp_simple_type{type = NotSupported} = Type, _Data) when
    NotSupported =:= pid orelse
        NotSupported =:= port orelse
        NotSupported =:= reference orelse
        NotSupported =:= bitstring orelse
        NotSupported =:= nonempty_bitstring orelse
        NotSupported =:= none
->
    erlang:error({type_not_supported, Type});
to_json(_TypeInfo, #sp_simple_type{} = Type, Value) ->
    prim_type_to_json(Type, Value);
to_json(
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
to_json(_TypeInfo, #sp_literal{value = Value, binary_value = BinaryValue}, Value) when
    is_atom(Value)
->
    {ok, BinaryValue};
to_json(_TypeInfo, #sp_literal{value = Value}, Value) when
    is_integer(Value) orelse Value =:= []
->
    {ok, Value};
to_json(TypeInfo, #sp_union{} = Type, Data) ->
    union(fun to_json/3, TypeInfo, Type, Data);
to_json(TypeInfo, #sp_nonempty_list{} = Type, Data) ->
    nonempty_list_to_json(TypeInfo, Type, Data);
to_json(TypeInfo, #sp_list{} = ListType, Data) when is_list(Data) ->
    list_to_json(TypeInfo, ListType, Data);
to_json(TypeInfo, #sp_map{struct_name = StructName} = Map, Data) ->
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
to_json(_TypeInfo, #sp_maybe_improper_list{} = Type, _Data) ->
    erlang:error({type_not_supported, Type});
to_json(_TypeInfo, #sp_nonempty_improper_list{} = Type, _Data) ->
    erlang:error({type_not_supported, Type});
to_json(_TypeInfo, #sp_tuple{} = Type, _Data) ->
    erlang:error({type_not_supported, Type});
to_json(_TypeInfo, #sp_function{} = Type, _Data) ->
    erlang:error({type_not_supported, Type});
to_json(_TypeInfo, Type, OtherValue) ->
    {error, [sp_error:type_mismatch(Type, OtherValue)]}.

-spec prim_type_to_json(Type :: spectra:sp_type(), Value :: dynamic()) ->
    {ok, json:encode_value()} | {error, [spectra:error()]}.
prim_type_to_json(#sp_simple_type{type = Type} = T, Value) ->
    case check_type_to_json(Type, Value) of
        {true, NewValue} when
            Type =:= binary orelse
                Type =:= nonempty_binary orelse
                Type =:= string orelse
                Type =:= nonempty_string
        ->
            Params = spectra_type:parameters(T),
            check_string_params(T, Params, NewValue);
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
    list_to_json(TypeInfo, #sp_list{type = Type}, Data);
nonempty_list_to_json(_TypeInfo, Type, Data) ->
    {error, [sp_error:type_mismatch(Type, Data)]}.

-spec list_to_json(
    TypeInfo :: spectra:type_info(),
    ListType :: #sp_list{},
    Data :: [dynamic()]
) ->
    {ok, [json:encode_value()]} | {error, [spectra:error()]}.
list_to_json(TypeInfo, #sp_list{type = Type} = ListType, Data) when is_list(Data) ->
    case safe_enumerate(Data) of
        {ok, EnumeratedData} ->
            spectra_util:map_until_error(
                fun({Nr, Item}) ->
                    case to_json(TypeInfo, Type, Item) of
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
                EnumeratedData
            );
        {error, improper_list} ->
            {error, [sp_error:type_mismatch(ListType, Data)]}
    end.

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
                    case to_json(TypeInfo, FieldType, FieldData) of
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
                    case to_json(TypeInfo, FieldType, FieldData) of
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
        case to_json(TypeInfo, KeyType, Key) of
            {ok, KeyJson} ->
                case {Value, spectra_type:can_be_missing(TypeInfo, ValueType)} of
                    {MissingValue, {true, MissingValue}} ->
                        {ok, {FieldsAcc, maps:remove(Key, DataAcc)}};
                    _ ->
                        case to_json(TypeInfo, ValueType, Value) of
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
    RecordType :: #sp_rec{},
    Record :: dynamic(),
    TypeArgs :: [{atom(), spectra:sp_type()}]
) ->
    {ok, #{atom() => json:encode_value()}} | {error, [spectra:error()]}.
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
                case to_json(TypeInfo, FieldType, RecordFieldData) of
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

-doc """
Decodes `Json` into an Erlang term according to `Type`.

The inverse of `to_json/3`. Binary map keys become atom keys, binary atom
values are converted via `binary_to_existing_atom/2`, JSON `null` maps to
`nil` or `undefined` where the type allows it, and JSON arrays become lists.

Returns `{ok, Term}` on success or `{error, Errors}` with structured errors.
""".
-spec from_json(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type(),
    Json :: json:decode_value()
) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
from_json(TypeInfo, Type, Json) ->
    do_from_json(TypeInfo, Type, Json).

-spec do_from_json(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type(),
    Json :: json:decode_value()
) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
do_from_json(
    TypeInfo,
    #sp_user_type_ref{type_name = TypeName, variables = Args, arity = Arity} = UserTypeRef,
    Json
) when
    is_atom(TypeName)
->
    Mod = spectra_type_info:get_module(TypeInfo),
    Type = spectra_type_info:get_type(TypeInfo, TypeName, Arity),
    case spectra_codec:try_codec_decode(Mod, json, Type, Json, UserTypeRef) of
        continue ->
            TypeWithoutVars = apply_args(TypeInfo, Type, Args),
            do_from_json(TypeInfo, TypeWithoutVars, Json);
        Result ->
            Result
    end;
do_from_json(
    _TypeInfo,
    #sp_remote_type{mfargs = {Module, TypeName, Args}, arity = TypeArity} = RemoteRef,
    Json
) ->
    RemoteTypeInfo = spectra_module_types:get(Module),
    RemoteType = spectra_type_info:get_type(RemoteTypeInfo, TypeName, TypeArity),
    case spectra_codec:try_codec_decode(Module, json, RemoteType, Json, RemoteRef) of
        continue ->
            TypeResolved = spectra_type:propagate_params(
                RemoteRef, apply_args(RemoteTypeInfo, RemoteType, Args)
            ),
            do_from_json(RemoteTypeInfo, TypeResolved, Json);
        Result ->
            Result
    end;
do_from_json(TypeInfo, #sp_rec{} = Rec, Json) ->
    record_from_json(TypeInfo, Rec, Json, []);
do_from_json(
    TypeInfo,
    #sp_rec_ref{record_name = RecordName, field_types = TypeArgs} = RecordRef,
    Json
) when
    is_atom(RecordName)
->
    Mod = spectra_type_info:get_module(TypeInfo),
    RecordType = spectra_type_info:get_record(TypeInfo, RecordName),
    case spectra_codec:try_codec_decode(Mod, json, RecordType, Json, RecordRef) of
        continue -> record_from_json(TypeInfo, RecordType, Json, TypeArgs);
        Result -> Result
    end;
do_from_json(TypeInfo, #sp_map{} = Type, Json) ->
    map_from_json(TypeInfo, Type, Json);
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
do_from_json(_TypeInfo, #sp_simple_type{type = PrimaryType} = Type, Json) when
    PrimaryType =:= binary orelse
        PrimaryType =:= nonempty_binary orelse
        PrimaryType =:= string orelse
        PrimaryType =:= nonempty_string
->
    case check_type_from_json(PrimaryType, Json) of
        {true, NewValue} ->
            Params = spectra_type:parameters(Type),
            check_string_params(Type, Params, NewValue);
        {error, Reason} ->
            {error, Reason};
        false ->
            {error, [sp_error:type_mismatch(Type, Json)]}
    end;
do_from_json(_TypeInfo, #sp_simple_type{type = PrimaryType} = Type, Json) ->
    case check_type_from_json(PrimaryType, Json) of
        {true, NewValue} ->
            {ok, NewValue};
        {error, Reason} ->
            {error, Reason};
        false ->
            {error, [sp_error:type_mismatch(Type, Json)]}
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

list_from_json(TypeInfo, Type, Data, ListType) when is_list(Data) ->
    case safe_enumerate(Data) of
        {ok, EnumeratedData} ->
            Fun = fun({Nr, Item}) ->
                case do_from_json(TypeInfo, Type, Item) of
                    {ok, Json} ->
                        {ok, Json};
                    {error, Errs} ->
                        Errs2 = lists:map(fun(Err) -> sp_error:append_location(Err, Nr) end, Errs),

                        {error, Errs2}
                end
            end,
            spectra_util:map_until_error(Fun, EnumeratedData);
        {error, improper_list} ->
            %% Improper lists cannot be decoded from JSON
            {error, [sp_error:type_mismatch(ListType, Data)]}
    end;
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
    try unicode:characters_to_binary(Json) of
        {Err, _, _} when Err =:= error orelse Err =:= incomplete ->
            {error, [
                sp_error:type_mismatch(#sp_simple_type{type = Type}, Json, #{
                    message => "non printable"
                })
            ]};
        Bin when is_binary(Bin) ->
            {true, Bin}
    catch
        error:badarg ->
            {error, [sp_error:type_mismatch(#sp_simple_type{type = Type}, Json)]}
    end.

-spec check_string_params(
    Type :: #sp_simple_type{},
    Params :: term(),
    Value :: dynamic()
) -> {ok, dynamic()} | {error, [spectra:error()]}.
check_string_params(_Type, undefined, Value) ->
    {ok, Value};
check_string_params(Type, Params, Value) when is_map(Params) ->
    spectra_util:fold_until_error(
        fun
            ({min_length, MinLen}, V) when is_integer(MinLen), MinLen >= 0 ->
                case string:length(V) >= MinLen of
                    true -> {ok, V};
                    false -> {error, [sp_error:type_mismatch(Type, V, #{min_length => MinLen})]}
                end;
            ({max_length, MaxLen}, V) when is_integer(MaxLen), MaxLen >= 0 ->
                case string:length(V) =< MaxLen of
                    true -> {ok, V};
                    false -> {error, [sp_error:type_mismatch(Type, V, #{max_length => MaxLen})]}
                end;
            ({pattern, Pat}, V) when is_binary(Pat) ->
                case to_binary_for_pattern(Type, Pat, V) of
                    {ok, Bin} ->
                        case re:run(Bin, Pat, [{capture, none}]) of
                            match -> {ok, V};
                            nomatch -> {error, [sp_error:type_mismatch(Type, V, #{pattern => Pat})]}
                        end;
                    {error, _} = Err ->
                        Err
                end;
            ({format, _Format}, V) ->
                %% format is schema-only metadata; no runtime validation
                {ok, V};
            ({Key, Val}, _V) ->
                erlang:error({invalid_string_constraint, Key, Val})
        end,
        Value,
        maps:to_list(Params)
    );
check_string_params(_Type, Params, _Value) when not is_map(Params) ->
    erlang:error({invalid_string_constraints, Params}).

-spec to_binary_for_pattern(
    Type :: #sp_simple_type{},
    Pat :: binary(),
    Value :: dynamic()
) -> {ok, binary()} | {error, [spectra:error()]}.
to_binary_for_pattern(_Type, _Pat, V) when is_binary(V) ->
    {ok, V};
to_binary_for_pattern(Type, Pat, V) when is_list(V) ->
    case unicode:characters_to_binary(V) of
        Bin when is_binary(Bin) -> {ok, Bin};
        _ -> {error, [sp_error:type_mismatch(Type, V, #{pattern => Pat})]}
    end.

safe_enumerate(List) ->
    try lists:enumerate(List) of
        EnumeratedList -> {ok, EnumeratedList}
    catch
        error:function_clause -> {error, improper_list}
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
map_from_json(TypeInfo, #sp_map{fields = MapFieldType, struct_name = StructName}, Json) when
    is_map(Json)
->
    Defaults =
        case StructName of
            undefined -> undefined;
            _ -> StructName:'__struct__'()
        end,
    %% Partition fields: exact literal fields should be processed first to claim their keys
    %% before typed fields try to match them
    {ExactLiteralFields, OtherFields} = lists:partition(
        fun
            (#literal_map_field{kind = exact}) -> true;
            (_) -> false
        end,
        MapFieldType
    ),
    SortedFields = ExactLiteralFields ++ OtherFields,

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
                        {true, MissingValue} when Defaults =:= undefined ->
                            {ok, {[{FieldName, MissingValue}] ++ FieldsAcc, JsonAcc}};
                        {true, _} ->
                            {ok, {FieldsAcc, JsonAcc}};
                        false ->
                            case struct_default_value(Defaults, FieldName) of
                                {ok, _} ->
                                    {ok, {FieldsAcc, JsonAcc}};
                                error ->
                                    {error, [sp_error:missing_data(Type, JsonAcc, [FieldName])]}
                            end
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

    case spectra_util:fold_until_error(Fun, {[], Json}, SortedFields) of
        {ok, {Fields, _NotMapped}} ->
            Decoded = maps:from_list(Fields),
            Result =
                case Defaults of
                    undefined -> Decoded;
                    _ -> maps:merge(Defaults, Decoded)
                end,
            {ok, Result};
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

-spec struct_default_value(undefined | map(), atom()) -> {ok, term()} | error.
struct_default_value(undefined, _FieldName) ->
    error;
struct_default_value(Defaults, FieldName) ->
    case maps:find(FieldName, Defaults) of
        {ok, V} when V =/= nil, V =/= undefined -> {ok, V};
        _ -> error
    end.

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
    RecordType :: #sp_rec{},
    Json :: json:decode_value(),
    TypeArgs :: [spectra:record_field_arg()]
) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
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
    {ok, dynamic()} | {error, [spectra:error()]}.
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

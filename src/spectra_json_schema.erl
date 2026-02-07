-module(spectra_json_schema).

-export([to_schema/2]).

-ignore_xref([to_schema/2]).

-include("../include/spectra_internal.hrl").

-type json_schema() :: #{binary() => json:encode_value()}.
-type json_schema_object() :: #{binary() => json:encode_value()}.

-export_type([json_schema/0, json_schema_object/0]).

%% API

-spec to_schema(module() | spectra:type_info(), spectra:sp_type_or_ref()) -> json_schema().
to_schema(Module, Type) when is_atom(Module) ->
    TypeInfo = spectra_module_types:get(Module),
    to_schema(TypeInfo, Type);
%% Type references
to_schema(TypeInfo, {type, TypeName, TypeArity}) when is_atom(TypeName) ->
    Type = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, []),
    to_schema_for_sp_type(TypeInfo, TypeWithoutVars);
to_schema(TypeInfo, {record, RecordName}) when is_atom(RecordName) ->
    Record = spectra_type_info:get_record(TypeInfo, RecordName),
    to_schema_for_sp_type(TypeInfo, Record);
to_schema(TypeInfo, Type) ->
    to_schema_for_sp_type(TypeInfo, Type).

-spec to_schema_for_sp_type(spectra:type_info(), spectra:sp_type()) -> json_schema().
to_schema_for_sp_type(TypeInfo, Type) ->
    % Check if type has inline documentation in metadata (before do_to_schema which may error)
    MaybeDoc = get_inline_doc(Type),
    Schema = do_to_schema(TypeInfo, Type),
    SchemaWithDoc =
        case MaybeDoc of
            {ok, Doc} ->
                maps:merge(Schema, normalize_doc_for_json_schema(TypeInfo, Type, Doc));
            error ->
                Schema
        end,
    add_schema_version(SchemaWithDoc).

-spec do_to_schema(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type()
) ->
    json_schema_object().
%% Simple types
do_to_schema(_TypeInfo, #sp_simple_type{type = integer}) ->
    #{<<"type">> => <<"integer">>};
do_to_schema(_TypeInfo, #sp_simple_type{type = string}) ->
    #{<<"type">> => <<"string">>};
do_to_schema(_TypeInfo, #sp_simple_type{type = iodata}) ->
    #{<<"type">> => <<"string">>};
do_to_schema(_TypeInfo, #sp_simple_type{type = iolist}) ->
    #{<<"type">> => <<"string">>};
do_to_schema(_TypeInfo, #sp_simple_type{type = boolean}) ->
    #{<<"type">> => <<"boolean">>};
do_to_schema(_TypeInfo, #sp_simple_type{type = number}) ->
    #{<<"type">> => <<"number">>};
do_to_schema(_TypeInfo, #sp_simple_type{type = float}) ->
    #{<<"type">> => <<"number">>, <<"format">> => <<"float">>};
do_to_schema(_TypeInfo, #sp_simple_type{type = atom}) ->
    #{<<"type">> => <<"string">>};
do_to_schema(_TypeInfo, #sp_simple_type{type = binary}) ->
    #{<<"type">> => <<"string">>};
do_to_schema(_TypeInfo, #sp_simple_type{type = nonempty_binary}) ->
    #{<<"type">> => <<"string">>, <<"minLength">> => 1};
do_to_schema(_TypeInfo, #sp_simple_type{type = nonempty_string}) ->
    #{<<"type">> => <<"string">>, <<"minLength">> => 1};
do_to_schema(_TypeInfo, #sp_simple_type{type = pos_integer}) ->
    #{<<"type">> => <<"integer">>, <<"minimum">> => 1};
do_to_schema(_TypeInfo, #sp_simple_type{type = non_neg_integer}) ->
    #{<<"type">> => <<"integer">>, <<"minimum">> => 0};
do_to_schema(_TypeInfo, #sp_simple_type{type = neg_integer}) ->
    #{<<"type">> => <<"integer">>, <<"maximum">> => -1};
do_to_schema(_TypeInfo, #sp_simple_type{type = term}) ->
    % any type
    #{};
do_to_schema(_TypeInfo, #sp_simple_type{type = map}) ->
    % generic map type - allows any keys and values
    #{<<"type">> => <<"object">>};
%% Range types
do_to_schema(
    _TypeInfo,
    #sp_range{
        type = integer,
        lower_bound = Min,
        upper_bound = Max
    }
) ->
    #{
        <<"type">> => <<"integer">>,
        <<"minimum">> => Min,
        <<"maximum">> => Max
    };
%% Literal types
do_to_schema(_TypeInfo, #sp_literal{value = Value}) when
    Value =:= undefined orelse Value =:= nil
->
    #{<<"enum">> => [null]};
do_to_schema(_TypeInfo, #sp_literal{value = Value, binary_value = BinaryValue}) when
    is_atom(Value)
->
    #{<<"enum">> => [BinaryValue]};
do_to_schema(_TypeInfo, #sp_literal{value = Value}) ->
    #{<<"enum">> => [Value]};
%% List types
do_to_schema(TypeInfo, #sp_list{type = ItemType}) ->
    ItemSchema = do_to_schema(TypeInfo, ItemType),
    #{<<"type">> => <<"array">>, <<"items">> => ItemSchema};
do_to_schema(TypeInfo, #sp_nonempty_list{type = ItemType}) ->
    ItemSchema = do_to_schema(TypeInfo, ItemType),
    #{
        <<"type">> => <<"array">>,
        <<"items">> => ItemSchema,
        <<"minItems">> => 1
    };
%% Union types
do_to_schema(TypeInfo, #sp_union{types = Types}) ->
    case
        lists:partition(
            fun
                (#sp_literal{value = Value}) when Value =:= undefined orelse Value =:= nil ->
                    true;
                (_) ->
                    false
            end,
            Types
        )
    of
        {[_MissingLiteral], [SingleType]} ->
            do_to_schema(TypeInfo, SingleType);
        {[], NonMissingTypes} ->
            case try_generate_enum_schema(NonMissingTypes, TypeInfo) of
                not_all_literals ->
                    generate_oneof_schema(TypeInfo, NonMissingTypes);
                EnumSchema ->
                    EnumSchema
            end;
        {[_MissingLiteral], OtherTypes} when length(OtherTypes) > 1 ->
            case try_generate_enum_schema(OtherTypes, TypeInfo) of
                not_all_literals ->
                    generate_oneof_schema(TypeInfo, Types);
                EnumSchema ->
                    EnumSchema
            end
    end;
%% Map types
do_to_schema(TypeInfo, #sp_map{fields = Fields}) ->
    map_fields_to_schema(TypeInfo, Fields);
%% Record types
do_to_schema(TypeInfo, #sp_rec{} = RecordInfo) ->
    record_to_schema_internal(TypeInfo, RecordInfo);
%% Record references
do_to_schema(TypeInfo, #sp_rec_ref{record_name = RecordName}) ->
    record_to_schema_internal(TypeInfo, RecordName);
%% User type references
do_to_schema(TypeInfo, #sp_user_type_ref{type_name = TypeName, variables = TypeArgs}) ->
    TypeArity = length(TypeArgs),
    Type = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, TypeArgs),
    do_to_schema(TypeInfo, TypeWithoutVars);
%% Remote types
do_to_schema(_TypeInfo, #sp_remote_type{mfargs = {Module, TypeName, Args}}) ->
    TypeInfo = spectra_module_types:get(Module),
    TypeArity = length(Args),
    Type = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, Args),
    do_to_schema(TypeInfo, TypeWithoutVars);
%% Unsupported types
do_to_schema(_TypeInfo, #sp_simple_type{type = NotSupported} = Type) when
    NotSupported =:= pid orelse
        NotSupported =:= port orelse
        NotSupported =:= reference orelse
        NotSupported =:= bitstring orelse
        NotSupported =:= nonempty_bitstring orelse
        NotSupported =:= none
->
    erlang:error({type_not_supported, Type});
do_to_schema(_TypeInfo, #sp_tuple{} = Type) ->
    erlang:error({type_not_supported, Type});
do_to_schema(_TypeInfo, #sp_function{} = Type) ->
    erlang:error({type_not_supported, Type});
do_to_schema(_TypeInfo, #sp_maybe_improper_list{} = Type) ->
    erlang:error({type_not_supported, Type});
do_to_schema(_TypeInfo, #sp_nonempty_improper_list{} = Type) ->
    erlang:error({type_not_supported, Type}).

%% Helper functions

%% Check if a type can be used as a JSON object key (must be string-like)
-spec can_be_json_key(spectra:type_info(), spectra:sp_type()) -> boolean().
can_be_json_key(_TypeInfo, #sp_simple_type{type = Type}) when
    Type =:= string orelse
        Type =:= binary orelse
        Type =:= nonempty_string orelse
        Type =:= nonempty_binary orelse
        Type =:= atom
->
    true;
can_be_json_key(_TypeInfo, #sp_literal{value = Value}) when is_atom(Value) ->
    true;
can_be_json_key(TypeInfo, #sp_union{types = Types}) ->
    lists:all(fun(T) -> can_be_json_key(TypeInfo, T) end, Types);
can_be_json_key(TypeInfo, #sp_user_type_ref{type_name = TypeName, variables = TypeArgs}) ->
    TypeArity = length(TypeArgs),
    Type = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, TypeArgs),
    can_be_json_key(TypeInfo, TypeWithoutVars);
can_be_json_key(_TypeInfo, _Type) ->
    false.

%% Add JSON Schema version to the schema
-spec add_schema_version(json_schema_object()) -> json_schema().
add_schema_version(Schema) ->
    Schema#{<<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>}.

arg_names(#sp_type_with_variables{vars = Args}) ->
    Args;
arg_names(_) ->
    [].

apply_args(TypeInfo, Type, TypeArgs) when is_list(TypeArgs) ->
    ArgNames = arg_names(Type),
    NamedTypes =
        maps:from_list(
            lists:zip(ArgNames, TypeArgs)
        ),
    spectra_util:type_replace_vars(TypeInfo, Type, NamedTypes).

-spec map_fields_to_schema(spectra:type_info(), [spectra:map_field()]) ->
    json_schema_object().
map_fields_to_schema(TypeInfo, Fields) ->
    {Properties, Required, HasAdditional} = process_map_fields(TypeInfo, Fields, #{}, [], false),
    lists:foldl(
        fun({Key, Value, SkipValue}, Acc) ->
            map_add_if_not_value(Acc, Key, Value, SkipValue)
        end,
        #{<<"type">> => <<"object">>, <<"additionalProperties">> => HasAdditional},
        [{<<"properties">>, Properties, #{}}, {<<"required">>, Required, []}]
    ).

-spec process_map_fields(
    spectra:type_info(),
    [spectra:map_field()],
    map(),
    [binary()],
    boolean()
) ->
    {map(), [binary()], boolean()}.
process_map_fields(_TypeInfo, [], Properties, Required, HasAdditional) ->
    {Properties, Required, HasAdditional};
process_map_fields(
    TypeInfo,
    [#literal_map_field{kind = Kind, binary_name = BinaryName, val_type = FieldType} | Rest],
    Properties,
    Required,
    HasAdditional
) ->
    FieldSchema = do_to_schema(TypeInfo, FieldType),
    NewProperties = maps:put(BinaryName, FieldSchema, Properties),
    NewRequired =
        case Kind of
            exact -> [BinaryName | Required];
            assoc -> Required
        end,
    process_map_fields(TypeInfo, Rest, NewProperties, NewRequired, HasAdditional);
process_map_fields(
    TypeInfo,
    [#typed_map_field{key_type = KeyType, val_type = ValType} = Field | Rest],
    Properties,
    Required,
    _HasAdditional
) ->
    case can_be_json_key(TypeInfo, KeyType) of
        false ->
            erlang:error({type_not_supported, Field});
        true ->
            %% Validate that key and value types can generate JSON schemas (will crash if not)
            _ = do_to_schema(TypeInfo, KeyType),
            _ = do_to_schema(TypeInfo, ValType),
            process_map_fields(TypeInfo, Rest, Properties, Required, true)
    end.

-spec record_to_schema_internal(spectra:type_info(), atom() | #sp_rec{}) ->
    json_schema_object().
record_to_schema_internal(TypeInfo, RecordName) when is_atom(RecordName) ->
    case spectra_type_info:find_record(TypeInfo, RecordName) of
        {ok, RecordInfo} ->
            record_to_schema_internal(TypeInfo, RecordInfo);
        error ->
            erlang:error({record_not_found, RecordName})
    end;
record_to_schema_internal(TypeInfo, #sp_rec{fields = Fields, meta = Meta} = Record) ->
    {Properties, Required} = process_record_fields(TypeInfo, Fields, #{}, []),
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => Properties,
        <<"required">> => Required
    },
    % Add documentation from inline metadata if available
    case Meta of
        #{doc := Doc} ->
            maps:merge(Schema, normalize_doc_for_json_schema(TypeInfo, Record, Doc));
        #{} ->
            Schema
    end.

-spec process_record_fields(
    spectra:type_info(),
    [#sp_rec_field{}],
    map(),
    [binary()]
) ->
    {map(), [binary()]}.
process_record_fields(_TypeInfo, [], Properties, Required) ->
    {Properties, lists:reverse(Required)};
process_record_fields(
    TypeInfo,
    [#sp_rec_field{binary_name = BinaryName, type = FieldType} | Rest],
    Properties,
    Required
) ->
    FieldSchema = do_to_schema(TypeInfo, FieldType),
    NewProperties = Properties#{BinaryName => FieldSchema},
    NewRequired =
        case spectra_type:can_be_missing(TypeInfo, FieldType) of
            {true, _} ->
                Required;
            false ->
                [BinaryName | Required]
        end,
    process_record_fields(TypeInfo, Rest, NewProperties, NewRequired).

%% Helper function to generate oneOf schemas
generate_oneof_schema(TypeInfo, Types) ->
    Schemas = lists:map(fun(T) -> do_to_schema(TypeInfo, T) end, Types),
    #{<<"oneOf">> => Schemas}.

try_generate_enum_schema(Types, TypeInfo) ->
    %% First, expand all types to their base forms (resolving references)
    ExpandResults = lists:map(fun(T) -> expand_to_literals(T, TypeInfo) end, Types),
    %% Check if all types could be expanded to literals
    case
        lists:all(
            fun
                ({ok, _}) -> true;
                (_) -> false
            end,
            ExpandResults
        )
    of
        false ->
            not_all_literals;
        true ->
            %% Flatten all the literal lists
            ExpandedTypes = lists:flatmap(
                fun
                    ({ok, Lits}) -> Lits;
                    (_) -> []
                end,
                ExpandResults
            ),
            %% Now try to extract literal values from all expanded types
            Enums = spectra_util:map_until_error(
                fun(Type) -> extract_literal_value(Type) end,
                ExpandedTypes
            ),
            case Enums of
                {error, not_all_literals} ->
                    not_all_literals;
                {ok, EnumValues} ->
                    JsonType = infer_json_type(ExpandedTypes),
                    case JsonType of
                        undefined ->
                            #{<<"enum">> => EnumValues};
                        Type ->
                            #{<<"type">> => Type, <<"enum">> => EnumValues}
                    end
            end
    end.

%% Expand a type to a list of literal types, resolving references and unions
-spec expand_to_literals(spectra:sp_type(), spectra:type_info() | undefined) ->
    {ok, [spectra:sp_type()]} | {error, not_all_literals}.
expand_to_literals(#sp_literal{} = Literal, _TypeInfo) ->
    {ok, [Literal]};
%% Resolve remote types
expand_to_literals(#sp_remote_type{mfargs = {Module, TypeName, Args}}, _TypeInfo) ->
    RemoteTypeInfo = spectra_module_types:get(Module),
    TypeArity = length(Args),
    Type = spectra_type_info:get_type(RemoteTypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(RemoteTypeInfo, Type, Args),
    expand_to_literals(TypeWithoutVars, RemoteTypeInfo);
%% Resolve user type references
expand_to_literals(#sp_user_type_ref{type_name = TypeName, variables = TypeArgs}, TypeInfo) when
    TypeInfo =/= undefined
->
    TypeArity = length(TypeArgs),
    Type = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, TypeArgs),
    expand_to_literals(TypeWithoutVars, TypeInfo);
%% Flatten unions - all members must expand to literals
expand_to_literals(#sp_union{types = UnionTypes}, TypeInfo) ->
    Results = lists:map(fun(T) -> expand_to_literals(T, TypeInfo) end, UnionTypes),
    case
        lists:all(
            fun
                ({ok, _}) -> true;
                (_) -> false
            end,
            Results
        )
    of
        true ->
            AllLiterals = lists:flatmap(
                fun
                    ({ok, Lits}) -> Lits;
                    (_) -> []
                end,
                Results
            ),
            {ok, AllLiterals};
        false ->
            {error, not_all_literals}
    end;
%% Anything else cannot be expanded to literals
expand_to_literals(_Type, _TypeInfo) ->
    {error, not_all_literals}.

%% Helper to extract literal value from a type (non-recursive, only handles direct literals)
-spec extract_literal_value(spectra:sp_type()) -> {ok, term()} | {error, not_all_literals}.
extract_literal_value(#sp_literal{value = Value}) when Value =:= undefined orelse Value =:= nil ->
    {ok, null};
extract_literal_value(#sp_literal{value = Value}) when Value =:= true orelse Value =:= false ->
    {ok, Value};
extract_literal_value(#sp_literal{value = Value, binary_value = BinaryValue}) when is_atom(Value) ->
    {ok, BinaryValue};
extract_literal_value(#sp_literal{value = Value}) when is_integer(Value) ->
    {ok, Value};
extract_literal_value(_Type) ->
    {error, not_all_literals}.

infer_json_type(Types) ->
    JsonTypes = lists:map(fun literal_to_json_type/1, Types),
    case lists:usort(JsonTypes) of
        [SingleType] -> SingleType;
        _ -> undefined
    end.

literal_to_json_type(#sp_literal{value = Value}) when Value =:= undefined orelse Value =:= nil ->
    null;
literal_to_json_type(#sp_literal{value = Value}) when Value =:= true orelse Value =:= false ->
    <<"boolean">>;
literal_to_json_type(#sp_literal{value = Value}) when is_integer(Value) ->
    <<"integer">>;
literal_to_json_type(#sp_literal{value = Value}) when is_atom(Value) ->
    <<"string">>.

%% Helper function to conditionally add key-value pairs to a map
-spec map_add_if_not_value(Map, Key, Value, SkipValue) -> Map when
    Map :: map(),
    Key :: term(),
    Value :: term(),
    SkipValue :: term().
map_add_if_not_value(Map, _Key, Value, SkipValue) when Value =:= SkipValue ->
    Map;
map_add_if_not_value(Map, Key, Value, _SkipValue) ->
    Map#{Key => Value}.

%% Extract inline doc from type metadata if present
-spec get_inline_doc(spectra:sp_type()) -> {ok, spectra:type_doc()} | error.
get_inline_doc(#sp_simple_type{meta = #{doc := Doc}}) -> {ok, Doc};
get_inline_doc(#sp_tuple{meta = #{doc := Doc}}) -> {ok, Doc};
get_inline_doc(#sp_map{meta = #{doc := Doc}}) -> {ok, Doc};
get_inline_doc(#sp_rec{meta = #{doc := Doc}}) -> {ok, Doc};
get_inline_doc(#sp_type_with_variables{meta = #{doc := Doc}}) -> {ok, Doc};
get_inline_doc(#sp_function{meta = #{doc := Doc}}) -> {ok, Doc};
get_inline_doc(#sp_union{meta = #{doc := Doc}}) -> {ok, Doc};
get_inline_doc(#sp_literal{meta = #{doc := Doc}}) -> {ok, Doc};
get_inline_doc(#sp_rec_ref{meta = #{doc := Doc}}) -> {ok, Doc};
get_inline_doc(#sp_remote_type{meta = #{doc := Doc}}) -> {ok, Doc};
get_inline_doc(#sp_maybe_improper_list{meta = #{doc := Doc}}) -> {ok, Doc};
get_inline_doc(#sp_nonempty_improper_list{meta = #{doc := Doc}}) -> {ok, Doc};
get_inline_doc(#sp_user_type_ref{meta = #{doc := Doc}}) -> {ok, Doc};
get_inline_doc(#sp_var{meta = #{doc := Doc}}) -> {ok, Doc};
get_inline_doc(#sp_range{meta = #{doc := Doc}}) -> {ok, Doc};
get_inline_doc(#sp_list{meta = #{doc := Doc}}) -> {ok, Doc};
get_inline_doc(#sp_nonempty_list{meta = #{doc := Doc}}) -> {ok, Doc};
get_inline_doc(_) -> error.

-spec normalize_doc_for_json_schema(
    spectra:type_info(), spectra:sp_type(), spectra:type_doc()
) -> json_schema_object().
normalize_doc_for_json_schema(TypeInfo, Type, Doc) ->
    maps:fold(
        fun
            (title, Value, Acc) when is_binary(Value) ->
                Acc#{<<"title">> => Value};
            (description, Value, Acc) when is_binary(Value) ->
                Acc#{<<"description">> => Value};
            (examples, ExampleTerms, Acc) when is_list(ExampleTerms) ->
                Acc#{<<"examples">> => convert_examples(TypeInfo, Type, ExampleTerms)};
            (examples_function, {Module, Function, Args}, Acc) ->
                ExampleTerms = erlang:apply(Module, Function, Args),
                Acc#{<<"examples">> => convert_examples(TypeInfo, Type, ExampleTerms)}
        end,
        #{},
        Doc
    ).

convert_examples(TypeInfo, Type, ExampleTerms) ->
    lists:map(
        fun(Term) ->
            case spectra_json:to_json(TypeInfo, Type, Term) of
                {ok, JsonValue} ->
                    JsonValue;
                {error, Errs} ->
                    erlang:error({invalid_example, Type, Term, Errs})
            end
        end,
        ExampleTerms
    ).

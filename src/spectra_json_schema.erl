-module(spectra_json_schema).

-export([to_schema/3, add_schema_version/1]).

-ignore_xref([to_schema/3, add_schema_version/1]).

-include("../include/spectra_internal.hrl").

-type json_schema_object() :: #{
    type => binary(),
    format => binary(),
    minLength => non_neg_integer(),
    maxLength => non_neg_integer(),
    pattern => binary(),
    minimum => integer(),
    maximum => integer(),
    enum => [null | binary() | integer() | boolean() | []],
    items => json_schema_object(),
    minItems => pos_integer(),
    oneOf => [json_schema_object()],
    properties => #{binary() => json_schema_object()},
    required => [binary()],
    additionalProperties => boolean(),
    title => binary(),
    description => binary(),
    deprecated => boolean(),
    examples => [json:encode_value()]
}.

-type json_schema() :: #{
    '$schema' => binary(),
    title => binary(),
    description => binary(),
    deprecated => boolean(),
    examples => [json:encode_value()],
    type => binary(),
    format => binary(),
    minLength => non_neg_integer(),
    maxLength => non_neg_integer(),
    pattern => binary(),
    minimum => integer(),
    maximum => integer(),
    enum => [null | binary() | integer() | boolean() | []],
    items => json_schema_object(),
    minItems => pos_integer(),
    oneOf => [json_schema_object()],
    properties => #{binary() => json_schema_object()},
    required => [binary()],
    additionalProperties => boolean()
}.

-export_type([json_schema/0, json_schema_object/0]).

%% API

-doc "Generates a JSON Schema object for `Type` using `Config` for codec and cache settings.".
-spec to_schema(spectra:type_info(), spectra:sp_type(), spectra:sp_config()) ->
    json_schema_object().
to_schema(TypeInfo, Type, Config) ->
    to_schema_for_sp_type(TypeInfo, Type, Config).

-spec to_schema_for_sp_type(spectra:type_info(), spectra:sp_type(), spectra:sp_config()) ->
    json_schema_object().
to_schema_for_sp_type(TypeInfo, Type, Config) ->
    Schema = do_to_schema(TypeInfo, Type, Config),
    merge_type_doc_into_schema(TypeInfo, Type, Schema, Config).

-spec do_to_schema(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type(),
    Config :: spectra:sp_config()
) ->
    json_schema_object().
do_to_schema(
    TypeInfo,
    #sp_user_type_ref{type_name = N, variables = Args, arity = Arity} = UserTypeRef,
    Config
) ->
    Type = spectra_type_info:get_type(TypeInfo, N, Arity),
    TypeRef = {type, N, Arity},
    case
        spectra_codec:try_codec_schema(TypeInfo, json_schema, TypeRef, Type, UserTypeRef, Config)
    of
        continue ->
            TypeWithoutVars = spectra_util:apply_args(TypeInfo, Type, Args),
            do_to_schema(TypeInfo, TypeWithoutVars, Config);
        Schema ->
            Schema
    end;
do_to_schema(
    _TypeInfo,
    #sp_remote_type{mfargs = {Mod, TypeName, Args}, arity = Arity} = RemoteRef,
    Config
) ->
    RemoteTypeInfo = spectra_module_types:get(Mod, Config),
    RemoteType = spectra_type_info:get_type(RemoteTypeInfo, TypeName, Arity),
    TypeRef = {type, TypeName, Arity},
    case
        spectra_codec:try_codec_schema(
            RemoteTypeInfo, json_schema, TypeRef, RemoteType, RemoteRef, Config
        )
    of
        continue ->
            TypeResolved = spectra_type:propagate_params(
                RemoteRef, spectra_util:apply_args(RemoteTypeInfo, RemoteType, Args)
            ),
            do_to_schema(RemoteTypeInfo, TypeResolved, Config);
        Schema ->
            Schema
    end;
do_to_schema(TypeInfo, #sp_rec_ref{record_name = N} = RecordRef, Config) ->
    RecordType = spectra_type_info:get_record(TypeInfo, N),
    TypeRef = {record, N},
    case
        spectra_codec:try_codec_schema(
            TypeInfo, json_schema, TypeRef, RecordType, RecordRef, Config
        )
    of
        continue ->
            Schema = record_to_schema_internal(TypeInfo, RecordType, Config),
            merge_type_doc_into_schema(TypeInfo, RecordType, Schema, Config);
        Schema ->
            Schema
    end;
%% Simple types
do_to_schema(_TypeInfo, #sp_simple_type{type = integer}, _Config) ->
    #{type => <<"integer">>};
do_to_schema(_TypeInfo, #sp_simple_type{type = string} = Type, _Config) ->
    apply_string_constraints(#{type => <<"string">>}, Type);
do_to_schema(_TypeInfo, #sp_simple_type{type = iodata}, _Config) ->
    #{type => <<"string">>};
do_to_schema(_TypeInfo, #sp_simple_type{type = iolist}, _Config) ->
    #{type => <<"string">>};
do_to_schema(_TypeInfo, #sp_simple_type{type = boolean}, _Config) ->
    #{type => <<"boolean">>};
do_to_schema(_TypeInfo, #sp_simple_type{type = number}, _Config) ->
    #{type => <<"number">>};
do_to_schema(_TypeInfo, #sp_simple_type{type = float}, _Config) ->
    #{type => <<"number">>, format => <<"float">>};
do_to_schema(_TypeInfo, #sp_simple_type{type = atom}, _Config) ->
    #{type => <<"string">>};
do_to_schema(_TypeInfo, #sp_simple_type{type = binary} = Type, _Config) ->
    apply_string_constraints(#{type => <<"string">>}, Type);
do_to_schema(_TypeInfo, #sp_simple_type{type = nonempty_binary} = Type, _Config) ->
    apply_string_constraints(#{type => <<"string">>, minLength => 1}, Type);
do_to_schema(_TypeInfo, #sp_simple_type{type = nonempty_string} = Type, _Config) ->
    apply_string_constraints(#{type => <<"string">>, minLength => 1}, Type);
do_to_schema(_TypeInfo, #sp_simple_type{type = pos_integer}, _Config) ->
    #{type => <<"integer">>, minimum => 1};
do_to_schema(_TypeInfo, #sp_simple_type{type = non_neg_integer}, _Config) ->
    #{type => <<"integer">>, minimum => 0};
do_to_schema(_TypeInfo, #sp_simple_type{type = neg_integer}, _Config) ->
    #{type => <<"integer">>, maximum => -1};
do_to_schema(_TypeInfo, #sp_simple_type{type = term}, _Config) ->
    % any type
    #{};
do_to_schema(_TypeInfo, #sp_simple_type{type = map}, _Config) ->
    % generic map type - allows any keys and values
    #{type => <<"object">>};
%% Range types
do_to_schema(
    _TypeInfo,
    #sp_range{
        type = integer,
        lower_bound = Min,
        upper_bound = Max
    },
    _Config
) ->
    #{
        type => <<"integer">>,
        minimum => Min,
        maximum => Max
    };
%% Literal types
do_to_schema(_TypeInfo, #sp_literal{value = Value}, _Config) when
    Value =:= undefined orelse Value =:= nil
->
    #{enum => [null]};
do_to_schema(_TypeInfo, #sp_literal{value = Value, binary_value = BinaryValue}, _Config) when
    is_atom(Value)
->
    #{enum => [BinaryValue]};
do_to_schema(_TypeInfo, #sp_literal{value = Value}, _Config) when
    is_integer(Value)
->
    #{enum => [Value]};
do_to_schema(_TypeInfo, #sp_literal{value = []}, _Config) ->
    #{enum => [[]]};
do_to_schema(_TypeInfo, #sp_literal{} = Type, _Config) ->
    erlang:error({type_not_supported, Type});
%% List types
do_to_schema(TypeInfo, #sp_list{type = ItemType}, Config) ->
    ItemSchema = do_to_schema(TypeInfo, ItemType, Config),
    #{type => <<"array">>, items => ItemSchema};
do_to_schema(TypeInfo, #sp_nonempty_list{type = ItemType}, Config) ->
    ItemSchema = do_to_schema(TypeInfo, ItemType, Config),
    #{
        type => <<"array">>,
        items => ItemSchema,
        minItems => 1
    };
%% Union types
do_to_schema(TypeInfo, #sp_union{types = Types}, Config) ->
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
            do_to_schema(TypeInfo, SingleType, Config);
        {[], NonMissingTypes} ->
            case try_generate_enum_schema(NonMissingTypes, TypeInfo, Config) of
                not_all_literals ->
                    generate_oneof_schema(TypeInfo, NonMissingTypes, Config);
                EnumSchema ->
                    EnumSchema
            end;
        {[_MissingLiteral], OtherTypes} when length(OtherTypes) > 1 ->
            case try_generate_enum_schema(OtherTypes, TypeInfo, Config) of
                not_all_literals ->
                    generate_oneof_schema(TypeInfo, Types, Config);
                EnumSchema ->
                    EnumSchema
            end
    end;
%% Map types
do_to_schema(TypeInfo, #sp_map{fields = Fields}, Config) ->
    map_fields_to_schema(TypeInfo, Fields, Config);
%% Record types
do_to_schema(TypeInfo, #sp_rec{} = RecordInfo, Config) ->
    record_to_schema_internal(TypeInfo, RecordInfo, Config);
%% Unsupported types
do_to_schema(_TypeInfo, #sp_simple_type{type = NotSupported} = Type, _Config) when
    NotSupported =:= pid orelse
        NotSupported =:= port orelse
        NotSupported =:= reference orelse
        NotSupported =:= bitstring orelse
        NotSupported =:= nonempty_bitstring orelse
        NotSupported =:= none
->
    erlang:error({type_not_supported, Type});
do_to_schema(_TypeInfo, #sp_tuple{} = Type, _Config) ->
    erlang:error({type_not_supported, Type});
do_to_schema(_TypeInfo, #sp_function{} = Type, _Config) ->
    erlang:error({type_not_supported, Type});
do_to_schema(_TypeInfo, #sp_maybe_improper_list{} = Type, _Config) ->
    erlang:error({type_not_supported, Type});
do_to_schema(_TypeInfo, #sp_nonempty_improper_list{} = Type, _Config) ->
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
can_be_json_key(TypeInfo, #sp_user_type_ref{
    type_name = TypeName, variables = TypeArgs, arity = TypeArity
}) ->
    Type = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = spectra_util:apply_args(TypeInfo, Type, TypeArgs),
    can_be_json_key(TypeInfo, TypeWithoutVars);
can_be_json_key(_TypeInfo, _Type) ->
    false.

%% Add JSON Schema version to the schema
-spec add_schema_version(json_schema_object()) -> json_schema().
add_schema_version(Schema) ->
    Schema#{'$schema' => <<"https://json-schema.org/draft/2020-12/schema">>}.

-spec map_fields_to_schema(spectra:type_info(), [spectra:map_field()], spectra:sp_config()) ->
    json_schema_object().
map_fields_to_schema(TypeInfo, Fields, Config) ->
    {Properties, Required, HasAdditional} = process_map_fields(
        TypeInfo, Fields, #{}, [], false, Config
    ),
    lists:foldl(
        fun({Key, Value, SkipValue}, Acc) ->
            map_add_if_not_value(Acc, Key, Value, SkipValue)
        end,
        #{type => <<"object">>, additionalProperties => HasAdditional},
        [{properties, Properties, #{}}, {required, Required, []}]
    ).

-spec process_map_fields(
    spectra:type_info(),
    [spectra:map_field()],
    map(),
    [binary()],
    boolean(),
    spectra:sp_config()
) ->
    {map(), [binary()], boolean()}.
process_map_fields(_TypeInfo, [], Properties, Required, HasAdditional, _Config) ->
    {Properties, Required, HasAdditional};
process_map_fields(
    TypeInfo,
    [#literal_map_field{kind = Kind, binary_name = BinaryName, val_type = FieldType} | Rest],
    Properties,
    Required,
    HasAdditional,
    Config
) ->
    FieldSchema = do_to_schema(TypeInfo, FieldType, Config),
    NewProperties = Properties#{BinaryName => FieldSchema},
    NewRequired =
        case Kind of
            exact -> [BinaryName | Required];
            assoc -> Required
        end,
    process_map_fields(TypeInfo, Rest, NewProperties, NewRequired, HasAdditional, Config);
process_map_fields(
    TypeInfo,
    [#typed_map_field{key_type = KeyType, val_type = ValType} = Field | Rest],
    Properties,
    Required,
    _HasAdditional,
    Config
) ->
    case can_be_json_key(TypeInfo, KeyType) of
        false ->
            erlang:error({type_not_supported, Field});
        true ->
            %% Validate that key and value types can generate JSON schemas (will crash if not)
            _ = do_to_schema(TypeInfo, KeyType, Config),
            _ = do_to_schema(TypeInfo, ValType, Config),
            process_map_fields(TypeInfo, Rest, Properties, Required, true, Config)
    end.

-spec record_to_schema_internal(spectra:type_info(), #sp_rec{}, spectra:sp_config()) ->
    json_schema_object().
record_to_schema_internal(TypeInfo, #sp_rec{} = Record, Config) ->
    record_fields_to_schema(TypeInfo, Record, Config).

-spec record_fields_to_schema(spectra:type_info(), #sp_rec{}, spectra:sp_config()) ->
    json_schema_object().
record_fields_to_schema(TypeInfo, #sp_rec{fields = Fields}, Config) ->
    {Properties, Required} = process_record_fields(TypeInfo, Fields, #{}, [], Config),
    #{
        type => <<"object">>,
        properties => Properties,
        required => Required
    }.

-spec process_record_fields(
    spectra:type_info(),
    [#sp_rec_field{}],
    map(),
    [binary()],
    spectra:sp_config()
) ->
    {map(), [binary()]}.
process_record_fields(_TypeInfo, [], Properties, Required, _Config) ->
    {Properties, lists:reverse(Required)};
process_record_fields(
    TypeInfo,
    [#sp_rec_field{binary_name = BinaryName, type = FieldType} | Rest],
    Properties,
    Required,
    Config
) ->
    FieldSchema = do_to_schema(TypeInfo, FieldType, Config),
    NewProperties = Properties#{BinaryName => FieldSchema},
    NewRequired =
        case spectra_type:can_be_missing(TypeInfo, FieldType) of
            {true, _} ->
                Required;
            false ->
                [BinaryName | Required]
        end,
    process_record_fields(TypeInfo, Rest, NewProperties, NewRequired, Config).

%% Helper function to generate oneOf schemas
generate_oneof_schema(TypeInfo, Types, Config) ->
    Schemas = lists:map(fun(T) -> do_to_schema(TypeInfo, T, Config) end, Types),
    #{oneOf => Schemas}.

try_generate_enum_schema(Types, TypeInfo, Config) ->
    %% First, expand all types to their base forms (resolving references)
    ExpandResults = lists:map(fun(T) -> expand_to_literals(T, TypeInfo, Config) end, Types),
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
                fun extract_literal_value/1,
                ExpandedTypes
            ),
            case Enums of
                {error, not_all_literals} ->
                    not_all_literals;
                {ok, EnumValues} ->
                    JsonType = infer_json_type(ExpandedTypes),
                    case JsonType of
                        undefined ->
                            #{enum => EnumValues};
                        Type ->
                            #{type => Type, enum => EnumValues}
                    end
            end
    end.

%% Expand a type to a list of literal types, resolving references and unions
-spec expand_to_literals(
    spectra:sp_type(), spectra:type_info() | undefined, spectra:sp_config()
) ->
    {ok, [spectra:sp_type()]} | {error, not_all_literals}.
expand_to_literals(#sp_literal{} = Literal, _TypeInfo, _Config) ->
    {ok, [Literal]};
%% Resolve remote types
expand_to_literals(
    #sp_remote_type{mfargs = {Module, TypeName, Args}, arity = TypeArity}, _TypeInfo, Config
) ->
    RemoteTypeInfo = spectra_module_types:get(Module, Config),
    Type = spectra_type_info:get_type(RemoteTypeInfo, TypeName, TypeArity),
    TypeWithoutVars = spectra_util:apply_args(RemoteTypeInfo, Type, Args),
    expand_to_literals(TypeWithoutVars, RemoteTypeInfo, Config);
%% Resolve user type references
expand_to_literals(
    #sp_user_type_ref{type_name = TypeName, variables = TypeArgs, arity = TypeArity},
    TypeInfo,
    Config
) when
    TypeInfo =/= undefined
->
    Type = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = spectra_util:apply_args(TypeInfo, Type, TypeArgs),
    expand_to_literals(TypeWithoutVars, TypeInfo, Config);
%% Flatten unions - all members must expand to literals
expand_to_literals(#sp_union{types = UnionTypes}, TypeInfo, Config) ->
    Results = lists:map(fun(T) -> expand_to_literals(T, TypeInfo, Config) end, UnionTypes),
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
expand_to_literals(_Type, _TypeInfo, _Config) ->
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
get_inline_doc(Type) ->
    case spectra_type:get_meta(Type) of
        #{doc := Doc} -> {ok, Doc};
        #{} -> error
    end.

-spec merge_type_doc_into_schema(
    spectra:type_info(), spectra:sp_type(), json_schema_object(), spectra:sp_config()
) ->
    json_schema_object().
merge_type_doc_into_schema(TypeInfo, Type, Schema, Config) ->
    case get_inline_doc(Type) of
        {ok, Doc} ->
            maps:merge(Schema, normalize_doc_for_json_schema(TypeInfo, Type, Doc, Config));
        error ->
            Schema
    end.

-spec normalize_doc_for_json_schema(
    spectra:type_info(), spectra:sp_type(), spectra:type_doc(), spectra:sp_config()
) -> json_schema_object().
normalize_doc_for_json_schema(TypeInfo, Type, Doc, Config) ->
    maps:fold(
        fun
            (title, Value, Acc) when is_binary(Value) ->
                Acc#{title => Value};
            (description, Value, Acc) when is_binary(Value) ->
                Acc#{description => Value};
            (deprecated, Value, Acc) when is_boolean(Value) ->
                Acc#{deprecated => Value};
            (examples, ExampleTerms, Acc) when is_list(ExampleTerms) ->
                Acc#{examples => convert_examples(TypeInfo, Type, ExampleTerms, Config)};
            (examples_function, {Module, Function, Args}, Acc) ->
                ExampleTerms = erlang:apply(Module, Function, Args),
                Acc#{examples => convert_examples(TypeInfo, Type, ExampleTerms, Config)}
        end,
        #{},
        Doc
    ).

-spec apply_string_constraints(json_schema_object(), spectra:sp_type()) ->
    json_schema_object().
apply_string_constraints(Base, Type) ->
    Params = spectra_type:parameters(Type),
    apply_string_params(Base, Params).

-spec apply_string_params(json_schema_object(), term()) -> json_schema_object().
apply_string_params(Base, undefined) ->
    Base;
apply_string_params(Base, Params) when is_map(Params) ->
    maps:fold(
        fun
            (min_length, V, Acc) when is_integer(V), V >= 0 ->
                Acc#{minLength => V};
            (max_length, V, Acc) when is_integer(V), V >= 0 ->
                Acc#{maxLength => V};
            (pattern, V, Acc) when is_binary(V) ->
                Acc#{pattern => V};
            (format, V, Acc) when is_binary(V) ->
                Acc#{format => V};
            (Key, Value, _Acc) ->
                erlang:error({invalid_string_constraint, Key, Value})
        end,
        Base,
        Params
    );
apply_string_params(_Base, Params) ->
    erlang:error({invalid_string_constraints, Params}).

convert_examples(TypeInfo, Type, ExampleTerms, Config) ->
    lists:map(
        fun(Term) ->
            case spectra_json:to_json(TypeInfo, Type, Term, Config) of
                {ok, JsonValue} ->
                    JsonValue;
                {error, Errs} ->
                    erlang:error({invalid_example, Type, Term, Errs})
            end
        end,
        ExampleTerms
    ).

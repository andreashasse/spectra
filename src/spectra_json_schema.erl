-module(spectra_json_schema).

-export([to_schema/3, add_schema_version/1]).

-ignore_xref([to_schema/3, add_schema_version/1]).

-include("../include/spectra_internal.hrl").

-type json_schema_object() :: #{
    type => binary() | [binary()],
    format => binary(),
    minLength => non_neg_integer(),
    maxLength => non_neg_integer(),
    pattern => binary(),
    minimum => integer(),
    maximum => integer(),
    enum => [null | binary() | integer() | boolean() | []],
    items => json_schema_object(),
    minItems => pos_integer(),
    anyOf => [json_schema_object()],
    properties => #{binary() => json_schema_object()},
    required => [binary()],
    additionalProperties => boolean() | json_schema_object(),
    propertyNames => json_schema_object(),
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
    type => binary() | [binary()],
    format => binary(),
    minLength => non_neg_integer(),
    maxLength => non_neg_integer(),
    pattern => binary(),
    minimum => integer(),
    maximum => integer(),
    enum => [null | binary() | integer() | boolean() | []],
    items => json_schema_object(),
    minItems => pos_integer(),
    anyOf => [json_schema_object()],
    properties => #{binary() => json_schema_object()},
    required => [binary()],
    additionalProperties => boolean() | json_schema_object(),
    propertyNames => json_schema_object()
}.

-export_type([json_schema/0, json_schema_object/0]).

%% API

-doc "Generates a JSON Schema object for `Type` using `Config` for codec and cache settings.".
-spec to_schema(spectra:type_info(), spectra:sp_type(), spectra:sp_config()) ->
    json_schema_object().
to_schema(TypeInfo, Type, Config) ->
    to_schema_for_sp_type(TypeInfo, Type, Config).

%% Every descent into a type — map field values, record fields, union branches,
%% list elements — and every resolution of a reference must go through here, so
%% that a type's `-spectra()` doc annotation travels with it wherever it is
%% inlined. Calling do_to_schema/3 directly silently drops the annotation.
%% Docs merge along the resolution chain: on conflicting keys the annotation
%% written nearest the use site wins.
%%
%% A doc only reaches a type node through spectra_abstract_code, which attaches
%% it to the top node of a stored named type or record, so today only the two
%% reference-resolution sites below observably change the output. The descent
%% sites hold the invariant for callers that build an sp_type() tree by hand
%% with a doc on a structural child, as the Elixir wrappers do.
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
    TypeRef = {type, N, Arity},
    Module = spectra_type_info:get_module(TypeInfo),
    case
        spectra_codec:try_codec_schema(json_schema, TypeInfo, Module, TypeRef, UserTypeRef, Config)
    of
        continue ->
            Type = spectra_type_info:get_type(TypeInfo, N, Arity),
            TypeWithoutVars0 = spectra_util:apply_args(TypeInfo, Type, Args),
            TypeWithoutVars = spectra_abstract_code:apply_ref_meta(
                TypeWithoutVars0, UserTypeRef#sp_user_type_ref.meta
            ),
            to_schema_for_sp_type(TypeInfo, TypeWithoutVars, Config);
        Schema ->
            Schema
    end;
do_to_schema(
    TypeInfo,
    #sp_remote_type{mfargs = {Mod, TypeName, Args}, arity = Arity} = RemoteRef,
    Config
) ->
    TypeRef = {type, TypeName, Arity},
    case spectra_codec:try_codec_schema(json_schema, TypeInfo, Mod, TypeRef, RemoteRef, Config) of
        continue ->
            RemoteTypeInfo = spectra_module_types:get(Mod, Config),
            RemoteType = spectra_type_info:get_type(RemoteTypeInfo, TypeName, Arity),
            TypeResolved0 = spectra_type:propagate_params(
                RemoteRef, spectra_util:apply_args(RemoteTypeInfo, RemoteType, Args)
            ),
            TypeResolved = spectra_abstract_code:apply_ref_meta(
                TypeResolved0, RemoteRef#sp_remote_type.meta
            ),
            to_schema_for_sp_type(RemoteTypeInfo, TypeResolved, Config);
        Schema ->
            Schema
    end;
do_to_schema(TypeInfo, #sp_rec_ref{record_name = N} = RecordRef, Config) ->
    TypeRef = {record, N},
    Module = spectra_type_info:get_module(TypeInfo),
    case
        spectra_codec:try_codec_schema(json_schema, TypeInfo, Module, TypeRef, RecordRef, Config)
    of
        continue ->
            RecordType0 = spectra_type_info:get_record(TypeInfo, N),
            #sp_rec{} =
                RecordType = spectra_abstract_code:apply_ref_meta(
                    RecordType0, RecordRef#sp_rec_ref.meta
                ),
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
    ItemSchema = list_item_schema(TypeInfo, ItemType, Config),
    #{type => <<"array">>, items => ItemSchema};
do_to_schema(TypeInfo, #sp_nonempty_list{type = ItemType}, Config) ->
    ItemSchema = list_item_schema(TypeInfo, ItemType, Config),
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
            missing_value_schema(to_schema_for_sp_type(TypeInfo, SingleType, Config), Config);
        {[], NonMissingTypes} ->
            union_schema(TypeInfo, NonMissingTypes, Config);
        {[_MissingLiteral], OtherTypes} when length(OtherTypes) > 1 ->
            missing_value_schema(union_schema(TypeInfo, OtherTypes, Config), Config)
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

-spec union_schema(spectra:type_info(), [spectra:sp_type()], spectra:sp_config()) ->
    json_schema_object().
union_schema(TypeInfo, Types, Config) ->
    case try_generate_enum_schema(Types, TypeInfo, Config) of
        not_all_literals ->
            generate_anyof_schema(TypeInfo, Types, Config);
        EnumSchema ->
            EnumSchema
    end.

%% A union with the missing value is encoded as null when it is not a field,
%% for example as a whole body. With `nil_fields = omit` the schema stays as it
%% was before null was allowed.
-spec missing_value_schema(json_schema_object(), spectra:sp_config()) -> json_schema_object().
missing_value_schema(Schema, #sp_config{nil_fields = omit}) ->
    Schema;
missing_value_schema(Schema, #sp_config{nil_fields = NilFields}) when
    NilFields =:= null; NilFields =:= omit_or_null
->
    allow_null(Schema).

%% A list item that holds the missing value is encoded as null, since a list
%% has no key to leave out.
-spec list_item_schema(spectra:type_info(), spectra:sp_type(), spectra:sp_config()) ->
    json_schema_object().
list_item_schema(TypeInfo, ItemType, Config) ->
    ItemSchema = to_schema_for_sp_type(TypeInfo, ItemType, Config),
    case spectra_type:can_be_missing(TypeInfo, ItemType) of
        {true, _} -> allow_null(ItemSchema);
        false -> ItemSchema
    end.

%% Check if a type can be used as a JSON object key (must be string-like)
-spec can_be_json_key(spectra:type_info(), spectra:sp_type(), spectra:sp_config()) -> boolean().
can_be_json_key(_TypeInfo, #sp_simple_type{type = Type}, _Config) when
    Type =:= string orelse
        Type =:= binary orelse
        Type =:= nonempty_string orelse
        Type =:= nonempty_binary orelse
        Type =:= atom
->
    true;
can_be_json_key(_TypeInfo, #sp_literal{value = Value}, _Config) when is_atom(Value) ->
    true;
can_be_json_key(TypeInfo, #sp_union{types = Types}, Config) ->
    lists:all(fun(T) -> can_be_json_key(TypeInfo, T, Config) end, Types);
can_be_json_key(
    TypeInfo,
    #sp_user_type_ref{type_name = TypeName, variables = TypeArgs, arity = TypeArity},
    Config
) ->
    Type = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = spectra_util:apply_args(TypeInfo, Type, TypeArgs),
    can_be_json_key(TypeInfo, TypeWithoutVars, Config);
can_be_json_key(
    _TypeInfo, #sp_remote_type{mfargs = {Module, TypeName, Args}, arity = TypeArity}, Config
) ->
    RemoteTypeInfo = spectra_module_types:get(Module, Config),
    Type = spectra_type_info:get_type(RemoteTypeInfo, TypeName, TypeArity),
    TypeWithoutVars = spectra_util:apply_args(RemoteTypeInfo, Type, Args),
    can_be_json_key(RemoteTypeInfo, TypeWithoutVars, Config);
can_be_json_key(_TypeInfo, _Type, _Config) ->
    false.

%% Add JSON Schema version to the schema
-spec add_schema_version(json_schema_object()) -> json_schema().
add_schema_version(Schema) ->
    Schema#{'$schema' => <<"https://json-schema.org/draft/2020-12/schema">>}.

-spec map_fields_to_schema(spectra:type_info(), [spectra:map_field()], spectra:sp_config()) ->
    json_schema_object().
map_fields_to_schema(TypeInfo, Fields, Config) ->
    {Properties, Required, KeySchemas, ValueSchemas} = process_map_fields(
        TypeInfo, Fields, #{}, [], [], [], Config
    ),
    lists:foldl(
        fun({Key, Value, SkipValue}, Acc) ->
            map_add_if_not_value(Acc, Key, Value, SkipValue)
        end,
        #{type => <<"object">>, additionalProperties => additional_properties(ValueSchemas)},
        [
            {properties, Properties, #{}},
            {required, Required, []},
            {propertyNames, property_names(Properties, KeySchemas), undefined}
        ]
    ).

-spec additional_properties([json_schema_object()]) -> boolean() | json_schema_object().
additional_properties([]) ->
    false;
additional_properties(ValueSchemas) ->
    case lists:member(#{}, ValueSchemas) of
        true -> true;
        false -> any_of_schemas(ValueSchemas)
    end.

%% propertyNames constrains every key, so it is only emitted when the map has no
%% literal fields whose names it would also have to admit.
-spec property_names(map(), [json_schema_object()]) -> json_schema_object() | undefined.
property_names(Properties, [_ | _] = KeySchemas) when map_size(Properties) =:= 0 ->
    case lists:any(fun is_constrained_key_schema/1, KeySchemas) of
        true -> any_of_schemas(KeySchemas);
        false -> undefined
    end;
property_names(_Properties, _KeySchemas) ->
    undefined.

-spec is_constrained_key_schema(json_schema_object()) -> boolean().
is_constrained_key_schema(Schema) ->
    maps:size(maps:with([enum, pattern, minLength, maxLength], Schema)) > 0.

-spec any_of_schemas([json_schema_object(), ...]) -> json_schema_object().
any_of_schemas(Schemas) ->
    case lists:uniq(Schemas) of
        [Schema] -> Schema;
        UniqueSchemas -> #{anyOf => UniqueSchemas}
    end.

-spec process_map_fields(
    spectra:type_info(),
    [spectra:map_field()],
    map(),
    [binary()],
    [json_schema_object()],
    [json_schema_object()],
    spectra:sp_config()
) ->
    {map(), [binary()], [json_schema_object()], [json_schema_object()]}.
process_map_fields(_TypeInfo, [], Properties, Required, KeySchemas, ValueSchemas, _Config) ->
    {Properties, Required, lists:reverse(KeySchemas), lists:reverse(ValueSchemas)};
process_map_fields(
    TypeInfo,
    [#literal_map_field{kind = Kind, binary_name = BinaryName, val_type = FieldType} | Rest],
    Properties,
    Required,
    KeySchemas,
    ValueSchemas,
    Config
) ->
    Missing = spectra_type:can_be_missing(TypeInfo, FieldType),
    FieldSchema = field_schema(TypeInfo, FieldType, Missing, Config),
    NewProperties = Properties#{BinaryName => FieldSchema},
    %% Optional (`=>`) fields are never required: the encoder leaves an absent
    %% key out. Exact (`:=`) fields follow the same rule as record fields.
    NewRequired =
        case Kind =:= exact andalso is_field_required(Missing, Config) of
            true -> [BinaryName | Required];
            false -> Required
        end,
    process_map_fields(
        TypeInfo, Rest, NewProperties, NewRequired, KeySchemas, ValueSchemas, Config
    );
process_map_fields(
    TypeInfo,
    [#typed_map_field{key_type = KeyType, val_type = ValType} = Field | Rest],
    Properties,
    Required,
    KeySchemas,
    ValueSchemas,
    Config
) ->
    case can_be_json_key(TypeInfo, KeyType, Config) of
        false ->
            erlang:error({type_not_supported, Field});
        true ->
            KeySchema = do_to_schema(TypeInfo, KeyType, Config),
            Missing = spectra_type:can_be_missing(TypeInfo, ValType),
            ValueSchema = field_schema(TypeInfo, ValType, Missing, Config),
            process_map_fields(
                TypeInfo,
                Rest,
                Properties,
                Required,
                [KeySchema | KeySchemas],
                [ValueSchema | ValueSchemas],
                Config
            )
    end.

-spec record_to_schema_internal(spectra:type_info(), #sp_rec{}, spectra:sp_config()) ->
    json_schema_object().
record_to_schema_internal(TypeInfo, #sp_rec{} = Record, Config) ->
    record_fields_to_schema(TypeInfo, Record, Config).

-spec record_fields_to_schema(spectra:type_info(), #sp_rec{}, spectra:sp_config()) ->
    json_schema_object().
record_fields_to_schema(TypeInfo, #sp_rec{fields = Fields, meta = Meta}, Config) ->
    FilteredFields =
        case Meta of
            #{only := Only} -> [F || #sp_rec_field{name = N} = F <- Fields, lists:member(N, Only)];
            _ -> Fields
        end,
    {Properties, Required} = process_record_fields(TypeInfo, FilteredFields, #{}, [], Config),
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
    Missing = spectra_type:can_be_missing(TypeInfo, FieldType),
    FieldSchema = field_schema(TypeInfo, FieldType, Missing, Config),
    NewProperties = Properties#{BinaryName => FieldSchema},
    NewRequired =
        case is_field_required(Missing, Config) of
            true -> [BinaryName | Required];
            false -> Required
        end,
    process_record_fields(TypeInfo, Rest, NewProperties, NewRequired, Config).

%% A field whose type can be missing is required only with `nil_fields = null`,
%% where it is always written. Its schema allows null unless `nil_fields = omit`,
%% where the missing value is always left out.
-spec field_schema(
    spectra:type_info(),
    spectra:sp_type(),
    {true, spectra:missing_value()} | false,
    spectra:sp_config()
) -> json_schema_object().
field_schema(TypeInfo, FieldType, {true, _}, #sp_config{nil_fields = NilFields} = Config) when
    NilFields =:= null; NilFields =:= omit_or_null
->
    allow_null(to_schema_for_sp_type(TypeInfo, FieldType, Config));
field_schema(TypeInfo, FieldType, _Missing, Config) ->
    to_schema_for_sp_type(TypeInfo, FieldType, Config).

-spec is_field_required({true, spectra:missing_value()} | false, spectra:sp_config()) ->
    boolean().
is_field_required(false, _Config) -> true;
is_field_required({true, _}, #sp_config{nil_fields = NilFields}) -> NilFields =:= null.

-spec allow_null(json_schema_object()) -> json_schema_object().
allow_null(Schema) ->
    case allows_null(Schema) of
        true -> Schema;
        false -> add_null(Schema)
    end.

-spec allows_null(json_schema_object()) -> boolean().
allows_null(#{type := Types}) when is_list(Types) ->
    lists:member(<<"null">>, Types);
allows_null(#{enum := Values}) ->
    lists:member(null, Values);
allows_null(#{anyOf := Schemas}) ->
    lists:any(fun allows_null/1, Schemas);
allows_null(#{type := <<"null">>}) ->
    true;
allows_null(_Schema) ->
    false.

-spec add_null(json_schema_object()) -> json_schema_object().
add_null(#{type := Type} = Schema) when is_binary(Type), not is_map_key(enum, Schema) ->
    Schema#{type => [Type, <<"null">>]};
add_null(#{anyOf := Schemas} = Schema) when map_size(Schema) =:= 1 ->
    #{anyOf => Schemas ++ [#{type => <<"null">>}]};
add_null(Schema) ->
    #{anyOf => [Schema, #{type => <<"null">>}]}.

generate_anyof_schema(TypeInfo, Types, Config) ->
    any_of_schemas(lists:map(fun(T) -> to_schema_for_sp_type(TypeInfo, T, Config) end, Types)).

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

-module(spectra).

-export([decode/4, decode/5, encode/4, encode/5, schema/3, schema/4, get_config/0]).

-ignore_xref([decode/4, decode/5, encode/4, encode/5, schema/3, schema/4, get_config/0]).

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-type sp_config() :: #sp_config{}.
-type type_info() :: spectra_type_info:type_info().
-type var_type() :: {VarName :: atom(), sp_type()}.
-type user_type_name() :: atom().
-type record_field_arg() :: {FieldName :: atom(), sp_type()}.
-type type_doc() :: #{
    title => binary(),
    description => binary(),
    deprecated => boolean(),
    examples => [dynamic()],
    examples_function => {module(), atom(), [term()]}
}.

-type function_doc() :: #{
    summary => binary(),
    description => binary(),
    deprecated => boolean()
}.

-type sp_type_meta() :: #{
    doc => type_doc(),
    name => sp_type_reference(),
    parameters => term()
}.

-type sp_function_spec_meta() :: #{
    doc => function_doc()
}.

%% FIXME: Add doc here.
%% iolist and iodata are aliases, but are so complex, so it is easier to handle them as separate types
-type sp_type() ::
    #sp_simple_type{}
    | #sp_rec_ref{}
    | #sp_user_type_ref{}
    | #sp_var{}
    | #sp_map{}
    | #sp_rec{}
    | #sp_tuple{}
    | #sp_type_with_variables{}
    | #sp_function{}
    | #sp_union{}
    | #sp_literal{}
    | #sp_range{}
    | #sp_list{}
    | #sp_nonempty_list{}
    | #sp_maybe_improper_list{}
    | #sp_nonempty_improper_list{}
    | #sp_remote_type{}.
-type map_field() :: #literal_map_field{} | #typed_map_field{}.
-type sp_type_reference() ::
    {type, Name :: atom(), Arity :: arity()} | {record, Name :: atom()}.
-type error() :: #sp_error{}.
-type sp_type_or_ref() :: sp_type() | sp_type_reference().
-type sp_function_spec() :: #sp_function_spec{}.
%undefined in erlang, nil in elixir
-type missing_value() :: undefined | nil.
-type literal_value() :: integer() | atom() | [].
-type record_field() :: #sp_rec_field{}.
-type decode_option() :: pre_decoded | {pre_decoded, boolean()}.
-type encode_option() :: pre_encoded | {pre_encoded, boolean()}.
-type schema_option() :: pre_encoded | {pre_encoded, boolean()}.
-type codec_key() :: {module(), sp_type_reference()}.
-type module_types_cache() :: persistent | local | none.
-type binary_string_decode_opts() :: map().
-type binary_string_encode_opts() :: map().
-doc """
Return type for codec `encode/4` callbacks. See `spectra_codec`.
""".
-type codec_encode_result() :: {ok, dynamic()} | {error, [error()]} | continue.
-doc """
Return type for codec `decode/4` callbacks. See `spectra_codec`.
""".
-type codec_decode_result() :: {ok, dynamic()} | {error, [error()]} | continue.
%% Internal type definitions moved from spectra_internal.hrl

-type simple_types() ::
    string
    | nonempty_string
    | integer
    | non_neg_integer
    | neg_integer
    | pos_integer
    | float
    | number
    | boolean
    | binary
    | nonempty_binary
    | bitstring
    | nonempty_bitstring
    | atom
    | term
    | reference
    | pid
    | port
    | iolist
    | iodata
    | none
    | map.

-export_type([
    sp_type/0,
    sp_type_reference/0,
    sp_type_or_ref/0,
    var_type/0,
    type_info/0,
    type_doc/0,
    function_doc/0,
    sp_type_meta/0,
    sp_function_spec_meta/0,
    record_field_arg/0,
    error/0,
    map_field/0,
    user_type_name/0,
    sp_function_spec/0,
    missing_value/0,
    simple_types/0,
    literal_value/0,
    record_field/0,
    decode_option/0,
    encode_option/0,
    binary_string_decode_opts/0,
    binary_string_encode_opts/0,
    codec_encode_result/0,
    codec_decode_result/0,
    codec_key/0,
    module_types_cache/0,
    schema_option/0,
    sp_config/0
]).

-doc """
Decodes data from the specified format into an Erlang term based on type information.

The function validates the decoded data against the type specification and returns
an error if the data doesn't match the expected type.

### Example:

```
-module(my_module).
-type user_id() :: pos_integer().
-type status() :: active | inactive | pending.
-record(user, {id :: user_id(), name :: binary(), age :: integer(), status :: status()}).

1> spectra:decode(json, my_module, user_id, <<"123">>).
{ok, 123}

2> spectra:decode(json, my_module, user, <<"{\"id\":42,\"name\":\"Bob\",\"age\":25, \"status\":\"active\"}">>).
{ok, #user{id = 42, name = <<"Bob">>, age = 25, status = active}}

3> spectra:decode(binary_string, my_module, status, <<"active">>).
{ok, active}

4> spectra:decode(json, my_module, user_id, <<"\"not_a_number\"">>).
{error, [#sp_error{type = type_mismatch, ...}]}
```
""".
-spec decode(
    Format :: atom(),
    ModuleOrTypeinfo :: module() | type_info(),
    TypeOrRef :: atom() | sp_type_or_ref(),
    Data :: dynamic()
) ->
    {ok, dynamic()} | {error, [error()]}.
decode(Format, ModuleOrTypeinfo, TypeOrRef, Data) ->
    decode(Format, ModuleOrTypeinfo, TypeOrRef, Data, []).

-doc """
Decodes data from the specified format into an Erlang term based on type information.

Accepts an options list. Supported options:
- `pre_decoded`: The input is already a decoded term (e.g. a JSON map from a web framework).
  Skips the deserialization step and passes the value directly to the type decoder.

### Example:

```
1> DecodedJson = #{<<"id">> => 42, <<"name">> => <<"Bob">>}.
2> spectra:decode(json, my_module, user, DecodedJson, [pre_decoded]).
{ok, #user{id = 42, name = <<"Bob">>}}
```
""".
-spec decode(
    Format :: atom(),
    ModuleOrTypeinfo :: module() | type_info(),
    TypeOrRef :: atom() | sp_type_or_ref(),
    Data :: dynamic(),
    Options :: [decode_option()]
) ->
    {ok, dynamic()} | {error, [error()]}.
decode(Format, Module, TypeOrRef, Data, Options) when is_atom(Module) ->
    Config = get_config(),
    TypeInfo = spectra_module_types:get(Module, Config),
    try
        do_decode(Format, TypeInfo, TypeOrRef, Data, Options, Config)
    after
        maybe_clear_local_cache(Config)
    end;
decode(Format, TypeInfo, TypeOrRef, Data, Options) ->
    Config = get_config(),
    try
        do_decode(Format, TypeInfo, TypeOrRef, Data, Options, Config)
    after
        maybe_clear_local_cache(Config)
    end.

-spec do_decode(
    Format :: atom(),
    TypeInfo :: type_info(),
    TypeOrRef :: atom() | sp_type_or_ref(),
    Data :: dynamic(),
    Options :: [decode_option()],
    Config :: sp_config()
) ->
    {ok, dynamic()} | {error, [error()]}.
do_decode(Format, TypeInfo, RefAtom, Data, Options, Config) when is_atom(RefAtom) ->
    TypeRef = spectra_util:normalize_type_ref(TypeInfo, RefAtom),
    do_decode(Format, TypeInfo, TypeRef, Data, Options, Config);
do_decode(Format, TypeInfo, {type, _, _} = TypeRef, Data, Options, Config) ->
    SpType = resolve_type_ref(TypeInfo, TypeRef),
    maybe_codec_decode(Format, TypeInfo, TypeRef, SpType, Data, Options, Config);
do_decode(Format, TypeInfo, {record, _} = TypeRef, Data, Options, Config) ->
    SpType = resolve_type_ref(TypeInfo, TypeRef),
    maybe_codec_decode(Format, TypeInfo, TypeRef, SpType, Data, Options, Config);
do_decode(Format, TypeInfo, SpType, Data, Options, Config) when is_record(TypeInfo, type_info) ->
    case type_ref_from_meta(SpType) of
        {ok, TypeRef} ->
            maybe_codec_decode(Format, TypeInfo, TypeRef, SpType, Data, Options, Config);
        error ->
            default_decode(Format, TypeInfo, SpType, Data, Options, Config)
    end;
do_decode(Format, TypeInfo, SpType, Data, Options, Config) ->
    default_decode(Format, TypeInfo, SpType, Data, Options, Config).

-spec default_decode(
    Format :: atom(),
    TypeInfo :: type_info(),
    Type :: sp_type(),
    Data :: dynamic(),
    Options :: [decode_option()],
    Config :: sp_config()
) ->
    {ok, dynamic()} | {error, [error()]}.
default_decode(json, Typeinfo, Type, Data, Options, Config) ->
    case proplists:get_value(pre_decoded, Options, false) of
        false when is_binary(Data) ->
            case json_decode(Data) of
                {ok, DecodedJson} ->
                    spectra_json:from_json(Typeinfo, Type, DecodedJson, Config);
                {error, _} = Err ->
                    Err
            end;
        false ->
            {error, [
                #sp_error{
                    location = [],
                    type = decode_error,
                    ctx = #{
                        value => Data,
                        message => "expected binary when pre_decoded option is not set"
                    }
                }
            ]};
        true ->
            spectra_json:from_json(Typeinfo, Type, Data, Config)
    end;
default_decode(binary_string, Typeinfo, TypeOrRef, Binary, _Options, Config) when
    is_binary(Binary)
->
    spectra_binary_string:from_binary_string(Typeinfo, TypeOrRef, Binary, #{}, Config);
default_decode(string, Typeinfo, TypeOrRef, String, _Options, Config) when is_list(String) ->
    spectra_string:from_string(Typeinfo, TypeOrRef, String, Config).

-doc """
Encodes an Erlang term to the specified format based on type information.

The function validates the Erlang term against the type specification before encoding
and returns an error if the data doesn't match the expected type.

### Example:

```
-module(my_module).
-type user_id() :: pos_integer().
-type status() :: active | inactive | pending.
-record(user, {id :: user_id(), name :: binary(), age :: integer(), status :: status()}).

1> spectra:encode(json, my_module, user_id, 123).
{ok, <<"123">>}

2> User = #user{id = 42, name = <<"Bob">>, age = 25, status = active}.
3> spectra:encode(json, my_module, user, User).
{ok, <<"{\"id\":42,\"name\":\"Bob\",\"age\":25, \"status\":\"active\"}">>}

4> spectra:encode(json, my_module, user_id, -5).
{error, [#sp_error{type = type_mismatch, ...}]}
```
""".
-spec encode(
    Format :: atom(),
    ModuleOrTypeinfo :: module() | type_info(),
    TypeOrRef :: atom() | sp_type_or_ref(),
    Data :: dynamic()
) ->
    {ok, dynamic()} | {error, [error()]}.
encode(Format, ModuleOrTypeinfo, TypeOrRef, Data) ->
    encode(Format, ModuleOrTypeinfo, TypeOrRef, Data, []).

-doc """
Encodes an Erlang term to the specified format based on type information.

Accepts an options list. Supported options:
- `pre_encoded`: Skip the final serialization step and return the intermediate
  term instead of bytes. For JSON, this returns a map/list/scalar instead of a binary.

### Example:

```
1> spectra:encode(json, my_module, user, #user{id = 42, name = <<"Bob">>}, [pre_encoded]).
{ok, #{<<"id">> => 42, <<"name">> => <<"Bob">>}}
```
""".
-spec encode(
    Format :: atom(),
    ModuleOrTypeinfo :: module() | type_info(),
    TypeOrRef :: atom() | sp_type_or_ref(),
    Data :: dynamic(),
    Options :: [encode_option()]
) ->
    {ok, dynamic()} | {error, [error()]}.
encode(Format, Module, TypeOrRef, Data, Options) when is_atom(Module) ->
    Config = get_config(),
    TypeInfo = spectra_module_types:get(Module, Config),
    try
        do_encode(Format, TypeInfo, TypeOrRef, Data, Options, Config)
    after
        maybe_clear_local_cache(Config)
    end;
encode(Format, TypeInfo, TypeOrRef, Data, Options) ->
    Config = get_config(),
    try
        do_encode(Format, TypeInfo, TypeOrRef, Data, Options, Config)
    after
        maybe_clear_local_cache(Config)
    end.

-spec do_encode(
    Format :: atom(),
    TypeInfo :: type_info(),
    TypeOrRef :: atom() | sp_type_or_ref(),
    Data :: dynamic(),
    Options :: [encode_option()],
    Config :: sp_config()
) ->
    {ok, dynamic()} | {error, [error()]}.
do_encode(Format, TypeInfo, TypeAtom, Data, Options, Config) when is_atom(TypeAtom) ->
    TypeRef = spectra_util:normalize_type_ref(TypeInfo, TypeAtom),
    do_encode(Format, TypeInfo, TypeRef, Data, Options, Config);
do_encode(Format, TypeInfo, {type, _, _} = TypeRef, Data, Options, Config) ->
    SpType = resolve_type_ref(TypeInfo, TypeRef),
    maybe_codec_encode(Format, TypeInfo, TypeRef, SpType, Data, Options, Config);
do_encode(Format, TypeInfo, {record, _} = TypeRef, Data, Options, Config) ->
    SpType = resolve_type_ref(TypeInfo, TypeRef),
    maybe_codec_encode(Format, TypeInfo, TypeRef, SpType, Data, Options, Config);
do_encode(Format, TypeInfo, SpType, Data, Options, Config) when is_record(TypeInfo, type_info) ->
    case type_ref_from_meta(SpType) of
        {ok, TypeRef} ->
            maybe_codec_encode(Format, TypeInfo, TypeRef, SpType, Data, Options, Config);
        error ->
            default_encode(Format, TypeInfo, SpType, Data, Options, Config)
    end;
do_encode(Format, TypeInfo, SpType, Data, Options, Config) ->
    default_encode(Format, TypeInfo, SpType, Data, Options, Config).

-spec default_encode(
    Format :: atom(),
    TypeInfo :: type_info(),
    SpType :: sp_type(),
    Data :: dynamic(),
    Options :: [encode_option()],
    Config :: sp_config()
) ->
    {ok, dynamic()} | {error, [error()]}.
default_encode(json, Typeinfo, TypeOrRef, Data, Options, Config) ->
    case spectra_json:to_json(Typeinfo, TypeOrRef, Data, Config) of
        {ok, Json} ->
            case proplists:get_value(pre_encoded, Options, false) of
                false -> {ok, json:encode(Json)};
                true -> {ok, Json}
            end;
        {error, _} = Err ->
            Err
    end;
default_encode(binary_string, Typeinfo, TypeOrRef, Data, _Options, Config) ->
    spectra_binary_string:to_binary_string(Typeinfo, TypeOrRef, Data, #{}, Config);
default_encode(string, Typeinfo, TypeOrRef, Data, _Options, Config) ->
    spectra_string:to_string(Typeinfo, TypeOrRef, Data, Config).

-doc """
Generates a schema for the specified type in the given format.

Equivalent to calling schema/4 with an empty options list.

### Example:

```
-module(my_module).
-type user_id() :: pos_integer().
-type status() :: active | inactive | pending.
-record(user, {id :: user_id(), name :: binary(), age :: integer(), status :: status()}).

1> spectra:schema(json_schema, my_module, user).
<<"{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"integer\"},...}}">>

2> spectra:schema(json_schema, my_module, status).
<<"{\"anyOf\":[{\"enum\":[\"active\"]},{\"enum\":[\"inactive\"]},{\"enum\":[\"pending\"]}]}">>

3> spectra:schema(json_schema, my_module, {type, user_id, 0}).
<<"{\"type\":\"integer\",\"minimum\":1}">>
```
""".
-doc #{
    equiv => schema(Format, ModuleOrTypeinfo, TypeOrRef, [])
}.
-spec schema(
    Format :: atom(),
    ModuleOrTypeinfo :: module() | type_info(),
    TypeOrRef :: atom() | sp_type_or_ref()
) ->
    iodata() | dynamic().
schema(Format, ModuleOrTypeinfo, TypeOrRef) ->
    schema(Format, ModuleOrTypeinfo, TypeOrRef, []).

-doc """
Generates a schema for the specified type in the given format.

Accepts an options list. Supported options:
- `pre_encoded`: Skip the final JSON encoding step and return the raw schema map
  instead of encoded `iodata()`. Useful for inspecting or manipulating the schema
  before serialisation.

### Example:

```
1> spectra:schema(json_schema, my_module, user, [pre_encoded]).
#{<<"type">> => <<"object">>, <<"properties">> => #{...}}
```
""".
-spec schema(
    Format :: atom(),
    ModuleOrTypeinfo :: module() | type_info(),
    TypeOrRef :: atom() | sp_type_or_ref(),
    Options :: [schema_option()]
) ->
    iodata() | dynamic().
schema(Format, Module, TypeOrRef, Options) when is_atom(Module) ->
    Config = get_config(),
    TypeInfo = spectra_module_types:get(Module, Config),
    try
        do_schema(Format, TypeInfo, TypeOrRef, Options, Config)
    after
        maybe_clear_local_cache(Config)
    end;
schema(Format, TypeInfo, TypeOrRef, Options) ->
    Config = get_config(),
    try
        do_schema(Format, TypeInfo, TypeOrRef, Options, Config)
    after
        maybe_clear_local_cache(Config)
    end.

-spec resolve_type_ref(type_info(), sp_type_reference()) -> sp_type().
resolve_type_ref(TypeInfo, {type, TypeName, TypeArity}) ->
    Type = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    spectra_util:type_replace_vars(TypeInfo, Type, #{});
resolve_type_ref(TypeInfo, {record, RecordName}) ->
    spectra_type_info:get_record(TypeInfo, RecordName).

-spec maybe_codec_schema(atom(), type_info(), sp_type_reference(), sp_type(), sp_config()) ->
    spectra_json_schema:json_schema_object().
maybe_codec_schema(Format, TypeInfo, TypeRef, SpType, Config) ->
    case spectra_codec:try_codec_schema(TypeInfo, Format, TypeRef, SpType, SpType, Config) of
        continue -> default_schema(Format, TypeInfo, SpType, Config);
        Schema -> Schema
    end.

-spec default_schema(atom(), type_info(), sp_type(), sp_config()) ->
    spectra_json_schema:json_schema_object().
default_schema(json_schema, TypeInfo, SpType, Config) ->
    spectra_json_schema:to_schema(TypeInfo, SpType, Config);
default_schema(Format, _TypeInfo, _SpType, _Config) ->
    erlang:error({unsupported_format, Format}).

-spec finalize_schema(atom(), spectra_json_schema:json_schema(), [schema_option()]) ->
    iodata() | dynamic().
finalize_schema(json_schema, SchemaMap, Options) ->
    case proplists:get_value(pre_encoded, Options, false) of
        true -> SchemaMap;
        false -> json:encode(SchemaMap)
    end;
finalize_schema(Format, _SchemaMap, _Options) ->
    erlang:error({unsupported_format, Format}).

-spec maybe_codec_decode(
    Format :: atom(),
    TypeInfo :: type_info(),
    TypeRef :: sp_type_reference(),
    SpType :: sp_type(),
    Data :: dynamic(),
    Options :: [decode_option()],
    Config :: sp_config()
) -> {ok, dynamic()} | {error, [error()]}.
maybe_codec_decode(Format, TypeInfo, TypeRef, SpType, Data, Options, Config) ->
    case
        spectra_codec:try_codec_decode(
            TypeInfo,
            Format,
            TypeRef,
            SpType,
            Data,
            SpType,
            Config
        )
    of
        continue -> default_decode(Format, TypeInfo, SpType, Data, Options, Config);
        Result -> Result
    end.

-spec maybe_codec_encode(
    Format :: atom(),
    TypeInfo :: type_info(),
    TypeRef :: sp_type_reference(),
    SpType :: sp_type(),
    Data :: dynamic(),
    Options :: [encode_option()],
    Config :: sp_config()
) -> {ok, dynamic()} | {error, [error()]}.
maybe_codec_encode(Format, TypeInfo, TypeRef, SpType, Data, Options, Config) ->
    case
        spectra_codec:try_codec_encode(
            TypeInfo,
            Format,
            TypeRef,
            SpType,
            Data,
            SpType,
            Config
        )
    of
        continue -> default_encode(Format, TypeInfo, SpType, Data, Options, Config);
        Result -> Result
    end.

-spec do_schema(atom(), type_info(), atom() | sp_type_or_ref(), [schema_option()], sp_config()) ->
    iodata() | map().
do_schema(Format, TypeInfo, TypeAtom, Options, Config) when is_atom(TypeAtom) ->
    TypeRef = spectra_util:normalize_type_ref(TypeInfo, TypeAtom),
    do_schema(Format, TypeInfo, TypeRef, Options, Config);
do_schema(Format, TypeInfo, {type, _, _} = TypeRef, Options, Config) ->
    do_schema_ref(Format, TypeInfo, TypeRef, Options, Config);
do_schema(Format, TypeInfo, {record, _} = TypeRef, Options, Config) ->
    do_schema_ref(Format, TypeInfo, TypeRef, Options, Config);
do_schema(json_schema, TypeInfo, SpType, Options, Config) ->
    SchemaMap =
        case type_ref_from_meta(SpType) of
            {ok, TypeRef} -> maybe_codec_schema(json_schema, TypeInfo, TypeRef, SpType, Config);
            error -> default_schema(json_schema, TypeInfo, SpType, Config)
        end,
    finalize_schema(json_schema, spectra_json_schema:add_schema_version(SchemaMap), Options);
do_schema(Format, TypeInfo, SpType, Options, Config) ->
    SchemaMap =
        case type_ref_from_meta(SpType) of
            {ok, TypeRef} -> maybe_codec_schema(Format, TypeInfo, TypeRef, SpType, Config);
            error -> default_schema(Format, TypeInfo, SpType, Config)
        end,
    finalize_schema(Format, SchemaMap, Options).

-spec do_schema_ref(atom(), type_info(), sp_type_reference(), [schema_option()], sp_config()) ->
    iodata() | map().
do_schema_ref(json_schema, TypeInfo, TypeRef, Options, Config) ->
    SpType = resolve_type_ref(TypeInfo, TypeRef),
    SchemaMap = spectra_json_schema:add_schema_version(
        maybe_codec_schema(json_schema, TypeInfo, TypeRef, SpType, Config)
    ),
    finalize_schema(json_schema, SchemaMap, Options);
do_schema_ref(Format, TypeInfo, TypeRef, Options, Config) ->
    SpType = resolve_type_ref(TypeInfo, TypeRef),
    SchemaMap = maybe_codec_schema(Format, TypeInfo, TypeRef, SpType, Config),
    finalize_schema(Format, SchemaMap, Options).

-spec type_ref_from_meta(sp_type()) -> {ok, sp_type_reference()} | error.
type_ref_from_meta(SpType) ->
    case spectra_type:get_meta(SpType) of
        #{name := TypeRef} -> {ok, TypeRef};
        #{} -> error
    end.

-spec get_config() -> sp_config().
get_config() ->
    #sp_config{
        module_types_cache = valid_module_types_cache(
            application:get_env(spectra, module_types_cache, local)
        ),
        check_unicode = application:get_env(spectra, check_unicode, false),
        codecs = application:get_env(spectra, codecs, #{})
    }.

-spec valid_module_types_cache(term()) -> module_types_cache().
valid_module_types_cache(persistent) -> persistent;
valid_module_types_cache(local) -> local;
valid_module_types_cache(none) -> none;
valid_module_types_cache(Value) -> erlang:error({invalid_config, module_types_cache, Value}).

-spec maybe_clear_local_cache(sp_config()) -> ok.
maybe_clear_local_cache(#sp_config{module_types_cache = local}) ->
    spectra_module_types:clear_local();
maybe_clear_local_cache(_) ->
    ok.

json_decode(Binary) ->
    try
        {ok, json:decode(Binary)}
    catch
        ErrType:Reason ->
            {error, [
                #sp_error{
                    location = [],
                    type = decode_error,
                    ctx = #{err_type => ErrType, err_reason => Reason}
                }
            ]}
    end.

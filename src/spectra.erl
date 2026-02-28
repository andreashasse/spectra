-module(spectra).

-export([decode/4, decode/5, encode/4, encode/5, schema/3]).

-ignore_xref([decode/4, decode/5, encode/4, encode/5, schema/3]).

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-type type_info() :: spectra_type_info:type_info().
-type var_type() :: {VarName :: atom(), sp_type()}.
-type user_type_name() :: atom().
-type record_field_arg() :: {FieldName :: atom(), sp_type()}.
-type type_doc() :: #{
    title => binary(),
    description => binary(),
    examples => [dynamic()],
    examples_function => {module(), atom(), [term()]}
}.

-type sp_type_meta() :: #{
    doc => type_doc()
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
-type encode_option() :: json_term | {json_term, boolean()}.
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
    sp_type_meta/0,
    record_field_arg/0,
    error/0,
    map_field/0,
    user_type_name/0,
    sp_function_spec/0,
    missing_value/0,
    simple_types/0,
    literal_value/0,
    record_field/0,
    encode_option/0
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
    Binary :: binary() | list()
) ->
    {ok, dynamic()} | {error, [error()]}.
decode(Format, ModuleOrTypeinfo, TypeOrRef, Data) ->
    decode(Format, ModuleOrTypeinfo, TypeOrRef, Data, []).

-doc """
Decodes data from the specified format into an Erlang term based on type information.

Accepts an options list. Supported options:
- `json_term`: The input is already a decoded JSON term.
  Skips the `json:decode/1` step and passes the value directly to the decoder.

### Example:

```
1> DecodedJson = #{<<"id">> => 42, <<"name">> => <<"Bob">>}.
2> spectra:decode(json, my_module, user, DecodedJson, [json_term]).
{ok, #user{id = 42, name = <<"Bob">>}}
```
""".
-spec decode(
    Format :: atom(),
    ModuleOrTypeinfo :: module() | type_info(),
    TypeOrRef :: atom() | sp_type_or_ref(),
    Data :: dynamic(),
    Options :: [encode_option()]
) ->
    {ok, dynamic()} | {error, [error()]}.
decode(Format, Module, TypeOrRef, Data, Options) when is_atom(Module) ->
    TypeInfo = spectra_module_types:get(Module),
    decode(Format, TypeInfo, TypeOrRef, Data, Options);
decode(Format, TypeInfo, RefAtom, Data, Options) when is_atom(RefAtom) ->
    Type = get_type_from_atom(TypeInfo, RefAtom),
    decode(Format, TypeInfo, Type, Data, Options);
decode(json, Typeinfo, TypeOrRef, Data, Options) ->
    case proplists:get_value(json_term, Options, false) of
        false when is_binary(Data) ->
            case json_decode(Data) of
                {ok, DecodedJson} ->
                    spectra_json:from_json(Typeinfo, TypeOrRef, DecodedJson);
                {error, _} = Err ->
                    Err
            end;
        false ->
            {error, [
                #sp_error{
                    location = [],
                    type = decode_error,
                    ctx = #{
                        value => Data, message => "expected binary when json_term option is not set"
                    }
                }
            ]};
        true ->
            spectra_json:from_json(Typeinfo, TypeOrRef, Data)
    end;
decode(binary_string, Typeinfo, TypeOrRef, Binary, _Options) when is_binary(Binary) ->
    spectra_binary_string:from_binary_string(Typeinfo, TypeOrRef, Binary);
decode(string, Typeinfo, TypeOrRef, String, _Options) when is_list(String) ->
    spectra_string:from_string(Typeinfo, TypeOrRef, String).

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
- `json_term`: Skip the final `json:encode/1` step and return the intermediate
  JSON term instead of a binary.

### Example:

```
1> spectra:encode(json, my_module, user, #user{id = 42, name = <<"Bob">>}, [json_term]).
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
    TypeInfo = spectra_module_types:get(Module),
    encode(Format, TypeInfo, TypeOrRef, Data, Options);
encode(Format, Module, TypeAtom, Data, Options) when is_atom(TypeAtom) ->
    Type = get_type_from_atom(Module, TypeAtom),
    encode(Format, Module, Type, Data, Options);
encode(json, Typeinfo, TypeOrRef, Data, Options) ->
    case spectra_json:to_json(Typeinfo, TypeOrRef, Data) of
        {ok, Json} ->
            case proplists:get_value(json_term, Options, false) of
                false -> {ok, json:encode(Json)};
                true -> {ok, Json}
            end;
        {error, _} = Err ->
            Err
    end;
encode(binary_string, Typeinfo, TypeOrRef, Data, _Options) ->
    spectra_binary_string:to_binary_string(Typeinfo, TypeOrRef, Data);
encode(string, Typeinfo, TypeOrRef, Data, _Options) ->
    spectra_string:to_string(Typeinfo, TypeOrRef, Data).

-doc """
Generates a schema for the specified type in the given format.

### Example:

```
-module(my_module).
-type user_id() :: pos_integer().
-type status() :: active | inactive | pending.
-record(user, {id :: user_id(), name :: binary(), age :: integer(), status :: status()}).

1> spectra:schema(json_schema, my_module, user).
<<"{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"integer\"},...}}">>

2> spectra:schema(json_schema, my_module, status).
<<"{\"oneOf\":[{\"enum\":[\"active\"]},{\"enum\":[\"inactive\"]},{\"enum\":[\"pending\"]}]}">>

3> spectra:schema(json_schema, my_module, {type, user_id, 0}).
<<"{\"type\":\"integer\",\"minimum\":1}">>
```
""".
-spec schema(
    Format :: atom(),
    ModuleOrTypeinfo :: module() | type_info(),
    TypeOrRef :: atom() | sp_type_or_ref()
) ->
    iodata().
schema(Format, Module, TypeOrRef) when is_atom(Module) ->
    TypeInfo = spectra_module_types:get(Module),
    schema(Format, TypeInfo, TypeOrRef);
schema(Format, TypeInfo, TypeAtom) when is_atom(TypeAtom) ->
    Type = get_type_from_atom(TypeInfo, TypeAtom),
    schema(Format, TypeInfo, Type);
schema(json_schema, TypeInfo, TypeOrRef) ->
    SchemaMap = spectra_json_schema:to_schema(TypeInfo, TypeOrRef),
    json:encode(SchemaMap).

get_type_from_atom(TypeInfo, RefAtom) ->
    case spectra_type_info:find_type(TypeInfo, RefAtom, 0) of
        {ok, Type} ->
            Type;
        error ->
            case spectra_type_info:find_record(TypeInfo, RefAtom) of
                {ok, Rec} ->
                    Rec;
                error ->
                    erlang:error({type_or_record_not_found, RefAtom})
            end
    end.

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

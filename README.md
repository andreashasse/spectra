# Spectra

A data validation library for Erlang inspired by Pydantic.
Spectra provides type-safe data serialization and deserialization for Erlang records and types. Currently the focus is on JSON.

- **Type-safe conversion**: Convert typed Erlang values to/from external formats such as JSON, making sure the data conforms to the type.
- **OpenAPI documentation**: Generate OpenAPI 3.1 specifications from your type definitions
- **Detailed errors**: Get error messages with location information when validation fails
- **Support for complex scenarios**: Handles unions, records, atoms, nested structures, ...


## Installation and Requirements

**Requires Erlang/OTP 27 or later** - Spectra uses the native `json` module introduced in OTP 27.

Add spectra to your rebar.config dependencies:

```erlang
{deps, [
    {spectra, "~> 0.11.0"}
]}.
```

Your modules must be compiled with `debug_info` for spectra to extract type information.

## Data (de)serialization and schemas

Here's how to use spectra for JSON serialization and deserialization:

```erlang
-module(user).

-export([from_json/1, to_json/1, json_schema/0]).

-record(user, {name, age, role}).

-type role() :: admin | member.
-type user() :: #user{
    name :: binary(),
    age :: non_neg_integer(),
    role :: role()
}.

-spec from_json(binary()) -> {ok, user()} | {error, [spectra:error()]}.
from_json(Json) ->
    spectra:decode(json, ?MODULE, user, Json).

-spec to_json(user()) -> {ok, iodata()} | {error, [spectra:error()]}.
to_json(User) ->
    spectra:encode(json, ?MODULE, user, User).

-spec json_schema() -> iodata().
json_schema() ->
    spectra:schema(json_schema, ?MODULE, user).
```

### Using the user module in the shell

```erlang
%% Compile the user module (note: You need debug info)
c("user.erl", [debug_info]).

%% Load the record defs into the shell.
rr(user).

User = #user{name = <<"Alice">>, age = 30, role = admin}.

{ok, JsonIO} = user:to_json(User).
Json = iolist_to_binary(JsonIO).
{ok, User} = user:from_json(Json).
iolist_to_binary(user:json_schema()).
```

### Data Serialization API

These are the main functions for JSON serialization and deserialization:

```erlang
spectra:encode(Format, Module, Type, Value) ->
    {ok, iodata()} | {error, [spectra:error()]}.
spectra:decode(Format, Module, Type, JsonBinary) ->
    {ok, Value} | {error, [spectra:error()]}.

```

Where:
- `Format` is `json`, `binary_string`, or `string`
- `Module` is the module where the type/record is defined (or a `type_info()` for advanced usage)
- `Type` is either:
  - an atom: spectra will look for a type of arity 0 or a record with that name
  - `{type, TypeName, Arity}` for user-defined types (e.g., `{type, my_type, 0}`)
  - `{record, RecordName}` for records (e.g., `{record, user}`)
  - a `spectra:sp_type()` directly, for advanced usage (e.g. when you have already resolved the type via `spectra_abstract_code`)

The `binary_string` and `string` formats decode a single value from a binary or string — useful for query parameters and path variables:

```erlang
-type role() :: admin | member.
-type page() :: 1..100.

spectra:decode(binary_string, ?MODULE, role, <<"admin">>).
%% => {ok, admin}

spectra:decode(binary_string, ?MODULE, page, <<"5">>).
%% => {ok, 5}
```

### Schema API

```erlang
spectra:schema(Format, Module, Type) -> Schema :: iodata().
spectra:schema(Format, Module, Type, Options) -> Schema :: iodata() | map().
```

Where `Format` is `json_schema`. The `Module` and `Type` arguments are the same as above.

### Options

All three functions accept an optional `Options` list as the last argument. Two options skip the outer serialization layer — useful when integrating with a web framework that handles JSON encoding/decoding:

| Option | Function | Effect |
|--------|----------|--------|
| `pre_decoded` | `decode` | Input is already a parsed term — skips `json:decode/1` |
| `pre_encoded` | `encode`, `schema` | Returns a term instead of `iodata()` — skips `json:encode/1` |

```erlang
%% Input already decoded by a web framework
{ok, User} = spectra:decode(json, my_module, user, DecodedJson, [pre_decoded]),

%% Get a term instead of iodata
{ok, JsonTerm} = spectra:encode(json, my_module, user, User, [pre_encoded]),
SchemaMap = spectra:schema(json_schema, my_module, user, [pre_encoded]).
```

## Custom Codecs

Custom codecs let you override how spectra encodes, decodes, and generates schemas for specific types. Eg, the JSON format differ from their Erlang structure (e.g. a `{X, Y}` tuple serialised as a `[X, Y]` array)

### Implementing a Codec

Implement the `spectra_codec` behaviour in your module. Codec callbacks receive a type reference: `{type, Name, Arity}` for named types (declared with `-type name() :: ...`) and `{record, Name}` for records. Return `continue` for any type reference your codec does not own — spectra falls through to its default structural encoder/decoder.

```erlang
-module(my_geo_codec).
-behaviour(spectra_codec).

-export([encode/7, decode/7, schema/6]).

%% point() is an opaque type: {X, Y} tuple serialised as a JSON [X, Y] array.
-type point() :: {float(), float()}.
-export_type([point/0]).

encode(json, _Mod, {type, point, 0}, {X, Y}, _SpType, _Params, _Config) when is_number(X), is_number(Y) ->
    {ok, [X, Y]};
encode(_Format, _Mod, {type, point, 0}, Data, _SpType, _Params, _Config) ->
    {error, [sp_error:type_mismatch({type, point, 0}, Data)]};
encode(_Format, _Mod, _TypeRef, _Data, _SpType, _Params, _Config) ->
    continue.

decode(json, _Mod, {type, point, 0}, [X, Y], _SpType, _Params, _Config) when is_number(X), is_number(Y) ->
    {ok, {X, Y}};
decode(_Format, _Mod, {type, point, 0}, Data, _SpType, _Params, _Config) ->
    {error, [sp_error:type_mismatch({type, point, 0}, Data)]};
decode(_Format, _Mod, _TypeRef, _Input, _SpType, _Params, _Config) ->
    continue.

schema(json_schema, _Mod, {type, point, 0}, _SpType, _Params, _Config) ->
    #{type => <<"array">>, items => #{type => <<"number">>}, minItems => 2, maxItems => 2};
schema(_Format, _Mod, _TypeRef, _SpType, _Params, _Config) ->
    continue.
```

For types your codec owns, return `{error, [sp_error:type_mismatch(TypeRef, Data)]}` when the data does not match — this allows spectra to correctly handle union types like `point() | undefined` by trying the next alternative instead of crashing on structural encoding of an opaque type.

The `schema/6` callback is optional — you do not need to export it if you only support formats that don't have a schema.

### Types in the Same Module (No Configuration)

If a module declares `-behaviour(spectra_codec)`, spectra automatically uses it as the codec for all named types defined in that module — no application environment configuration required. The `my_geo_codec` example above works this way: `point()` is defined in the same module that implements the behaviour.

### Types from Other Modules (App Environment)

To use a codec for a named type defined in another module (e.g., from a dependency), register it in the application environment:

```erlang
%% sys.config
{spectra, [
    {codecs, #{
        {calendar, {type, datetime, 0}} => my_datetime_codec
    }}
]}
```

The key is a `spectra:codec_key()` — a `{Module, TypeRef}` tuple identifying the type's owning module. `TypeRef` can be:
- `{type, TypeName, Arity}` for user-defined types (e.g., `{calendar, {type, datetime, 0}}`)
- `{record, RecordName}` for records (e.g., `{my_module, {record, my_record}}`)

Alternatively, if the other module itself implements `-behaviour(spectra_codec)`, spectra automatically uses it as the codec for all types defined in that module — no app environment entry is required. This applies to third-party modules that have adopted the `spectra_codec` behaviour themselves.

### The SpType Argument

The `SpType` argument (5th position) is the instantiation node from the traversal. For generic types (`#sp_user_type_ref{}` / `#sp_remote_type{}`), this node carries the **concrete type-variable bindings**. Call `spectra_type:type_args/1` to extract them. For a field typed as `dict:dict(binary(), integer())` the codec receives the remote-type node and can extract `[BinaryType, IntegerType]` to recursively encode/decode keys and values.

See [`spectra_dict_codec`](src/spectra_dict_codec.erl) for a complete example of a codec that uses `spectra_type:type_args/1` to handle a parameterised type.

## Built-in Codecs

Spectra ships with opt-in codecs for common OTP types. None are active by default — register them in the application environment.

### spectra_dict_codec

Encodes `dict:dict(Key, Value)` as a JSON object. Keys must encode to binary strings when encoding to JSON. `Key` and `Value` types are resolved from the type-variable bindings at each usage site.

```erlang
{spectra, [
    {codecs, #{
        {dict, {type, dict, 2}} => spectra_dict_codec
    }}
]}
```

```erlang
-type word_counts() :: dict:dict(binary(), non_neg_integer()).

D = dict:from_list([{<<"hello">>, 3}, {<<"world">>, 1}]),
{ok, Json} = spectra:encode(json, my_module, word_counts, D).
%% => {ok, <<"{\"hello\":3,\"world\":1}">>}

{ok, D2} = spectra:decode(json, my_module, word_counts, Json).
```

### spectra_calendar_codec

Encodes `calendar:datetime()` and `calendar:date()` as ISO 8601 strings. Register only the types you use:

```erlang
{spectra, [
    {codecs, #{
        {calendar, {type, datetime, 0}} => spectra_calendar_codec,
        {calendar, {type, date, 0}} => spectra_calendar_codec
    }}
]}
```

| Type | JSON representation | JSON Schema format |
|---|---|---|
| `calendar:datetime()` | `"2024-01-15T10:30:00Z"` | `date-time` |
| `calendar:date()` | `"2024-01-15"` | `date` |

`calendar:datetime()` has no timezone — values are treated as UTC. Encoding always appends `Z`; decoding requires `Z` and rejects other offsets. If you need full timezone support, use a dedicated datetime library and implement a custom `spectra_codec` for it.

```erlang
-type event() :: #{title => binary(), at => calendar:datetime()}.

{ok, Json} = spectra:encode(json, my_module, event,
    #{title => <<"Party">>, at => {{2024, 1, 15}, {18, 30, 0}}}).
%% => {ok, <<"{\"title\":\"Party\",\"at\":\"2024-01-15T18:30:00Z\"}">>}
```

## Type Parameters

The `-spectra()` attribute accepts a `type_parameters` key that serves two purposes: it is passed as `Params` to codec callbacks, and for built-in string/binary types it enables structural constraints — no custom codec required.

### String and Binary Constraints

For `binary()`, `nonempty_binary()`, `string()`, and `nonempty_string()` types you can apply structural constraints directly via `type_parameters` — **no custom codec required**. The value must be a map with any combination of the following keys:

| Key | JSON Schema keyword | Validated at encode/decode? | Notes |
|---|---|---|---|
| `min_length` | `minLength` | yes | Codepoint count (Unicode), not byte count |
| `max_length` | `maxLength` | yes | Codepoint count (Unicode), not byte count |
| `pattern` | `pattern` | yes | Erlang `re` regular expression (PCRE-style syntax) |
| `format` | `format` | no | Schema annotation only |

```erlang
-spectra(#{type_parameters => #{min_length => 2, max_length => 64}}).
-type username() :: binary().

-spectra(#{type_parameters => #{pattern => <<"^[a-z0-9_]+$">>, format => <<"hostname">>}}).
-type slug() :: binary().
```

Decoding enforces the constraints and returns a validation error on failure:

```erlang
spectra:decode(json, my_module, username, <<"x">>).
%% => {error, [#sp_error{...}]}   %% too short

spectra:decode(json, my_module, username, <<"alice">>).
%% => {ok, <<"alice">>}
```

The generated JSON Schema reflects the constraints:

```erlang
spectra:schema(json_schema, my_module, slug).
%% => #{type => <<"string">>, pattern => <<"^[a-z0-9_]+$">>, format => <<"hostname">>}
```

Encoding (`to_json`) also validates constraints — an error is returned if the Erlang value violates `min_length`, `max_length`, or `pattern`.

`nonempty_binary()` and `nonempty_string()` already imply `minLength: 1` in the schema; a `min_length` parameter overrides this baseline value.

Constraints are also preserved when the type body is a remote alias that resolves to a string/binary type — for example, Elixir's `String.t()`:

```erlang
-spectra(#{type_parameters => #{min_length => 1, max_length => 255}}).
-type name() :: 'Elixir.String':t().
```

Unknown keys in the `type_parameters` map crash with `{invalid_string_constraint, Key, Value}`.

### Codec Configuration

The `type_parameters` value is passed as `Params` (the 6th argument) to `encode/7`, `decode/7`, and `schema/6`. This lets you reuse a single codec across multiple types that differ only by configuration.

For example, a prefixed-ID codec where each type carries its own expected prefix:

```erlang
-module(prefixed_id_codec).
-behaviour(spectra_codec).

-export([encode/7, decode/7, schema/6]).

%% Only user_id() and org_id() are defined in this module, so spectra will only
%% ever call this codec for those two types — no continue clause needed.
%% The Erlang value is the raw ID (without prefix); the wire format includes the prefix.

-spectra(#{type_parameters => <<"user:">>}).
-type user_id() :: binary().

-spectra(#{type_parameters => <<"org:">>}).
-type org_id() :: binary().

-export_type([user_id/0, org_id/0]).

%% Strips the prefix on decode, re-attaches it on encode.
decode(json, ?MODULE, TypeRef, Data, _SpType, Prefix, _Config) when is_binary(Data), is_binary(Prefix) ->
    PrefixLen = byte_size(Prefix),
    case Data of
        <<Prefix:PrefixLen/binary, Rest/binary>> -> {ok, Rest};
        _ -> {error, [sp_error:type_mismatch(TypeRef, Data)]}
    end;
decode(json, ?MODULE, TypeRef, Data, _SpType, _Prefix, _Config) ->
    {error, [sp_error:type_mismatch(TypeRef, Data)]}.

encode(json, ?MODULE, _TypeRef, Data, _SpType, Prefix, _Config) when is_binary(Data), is_binary(Prefix) ->
    {ok, <<Prefix/binary, Data/binary>>};
encode(json, ?MODULE, TypeRef, Data, _SpType, _Prefix, _Config) ->
    {error, [sp_error:type_mismatch(TypeRef, Data)]}.

schema(json_schema, ?MODULE, _TypeRef, _SpType, Prefix, _Config) when is_binary(Prefix) ->
    #{type => <<"string">>, pattern => <<"^", Prefix/binary>>}.
```

```erlang
spectra:decode(json, prefixed_id_codec, user_id, <<"user:abc123">>).
%% => {ok, <<"abc123">>}

spectra:decode(json, prefixed_id_codec, user_id, <<"org:abc123">>).
%% => {error, [#sp_error{...}]}

spectra:encode(json, prefixed_id_codec, user_id, <<"abc123">>).
%% => {ok, <<"user:abc123">>}

spectra:schema(json_schema, prefixed_id_codec, org_id).
%% => #{type => <<"string">>, pattern => <<"^org:">>}
```

When `type_parameters` is not set on a type, the codec receives `undefined` as `Params`.

Parameters belong to the **type definition**, not the usage site. If `user_id()` is referenced from another module, the parameters always come from the module where `user_id()` is defined. There is no way to override them at the call site — which means the same prefix is enforced wherever the type is used.

## Documentation and Examples in Schemas

The `-spectra()` attribute annotates types, records, and function specs with metadata. The valid keys differ depending on what follows the attribute.

**Before a type or record:**

| Key | Type | Description |
|-----|------|-------------|
| `title` | `binary()` | Short display name |
| `description` | `binary()` | Longer description |
| `deprecated` | `boolean()` | Marks the type as deprecated |
| `examples` | `[term()]` | Example values (use tuple syntax for records) |
| `examples_function` | `{module(), atom(), [term()]}` | MFA returning example values — avoids tuple syntax for records |
| `type_parameters` | `term()` | Passed to codec callbacks; also enables string/binary constraints (see [Type Parameters](#type-parameters)) |
| `only` | `[atom()]` | Restrict encoding, decoding, and schema to the listed field names (see [Field Filtering with `only`](#field-filtering-with-only)) |

**Before a `-spec` declaration:**

| Key | Type | Description |
|-----|------|-------------|
| `summary` | `binary()` | Short one-line summary |
| `description` | `binary()` | Longer description |
| `deprecated` | `boolean()` | Marks the function as deprecated |

Note that types use `title` while function specs use `summary` — there is no `title` for functions, and no `summary` for types.

```erlang
-spectra(#{
    title => <<"User Status">>,
    description => <<"Current status of the user account">>,
    examples => [active, inactive]
}).
-type status() :: active | inactive | pending.

-spectra(#{
    title => <<"User Record">>,
    description => <<"A user in the system">>,
    examples => [
        {user, 1, <<"Alice">>, active},
        {user, 42, <<"Bob">>, inactive}
    ]
}).
-record(user, {
    id :: non_neg_integer(),
    name :: binary(),
    status :: status()
}).
```

**Note:** When using examples with records, you must use tuple syntax (e.g., `{user, 1, <<"Alice">>, active}`), which can be error-prone.
For better maintainability, especially with records, use the `examples_function` field to be able to use record syntax and programmatically generate examples:

```erlang
-record(person, {
    name :: binary(),
    age :: non_neg_integer()
}).

-spectra(#{
    title => <<"Person">>,
    description => <<"A person with name and age">>,
    examples_function => {?MODULE, person_examples, []}
}).
-type person_type() :: #person{}.

person_examples() ->
    [
        #person{name = <<"Alice">>, age = 30},
        #person{name = <<"Bob">>, age = 25}
    ].
```

The function specified in `examples_function` must be exported.

## Field Filtering with `only`

The `only` key in the `-spectra()` attribute restricts which fields are included when encoding, decoding, and generating schemas for a map type. It works for plain Erlang maps and Elixir structs alike, similarly to Jason's `only` option.

```erlang
-spectra(#{only => [name, age]}).
-type t() :: #{
    name := binary(),
    age := non_neg_integer(),
    email := binary() | undefined,
    password_hash := binary()
}.
```

With this definition:

- **Encoding**: only `name` and `age` appear in the JSON output — `email` and `password_hash` are omitted even if present in the input map.
- **Decoding**: only `name` and `age` are read from the JSON input. For Elixir structs, excluded fields are still populated from the struct's default values (via `__struct__/0`). Extra fields in the JSON for excluded fields are silently ignored.
- **Schema**: the generated schema includes only `name` and `age` as properties.

The `only` filter propagates through union types, so `t() | undefined` works as expected — the map member is filtered and the `undefined` literal is left unchanged.

> **Note:** When `only` is used, mandatory (`:=`) fields that are excluded will be absent from
> decoded plain maps, and absent from the encoded JSON — the result does not fully conform to the
> declared Erlang type. This is intentional: `only` is an opt-in escape hatch for controlling the
> external representation independently of the internal type. Dialyzer will not warn about this.

## OpenAPI Spec

Spectra can generate complete [OpenAPI 3.1](https://spec.openapis.org/oas/v3.1.0) specifications from your type definitions.

**Most users should not use this API directly.** Instead, use a web server integration library that wraps it — for example [elli_openapi](https://github.com/andreashasse/elli_openapi) for Elli. The builder API below is intended for authors of such libraries.

### OpenAPI Builder API

```erlang
%% Create a base endpoint
spectra_openapi:endpoint(Method, Path) ->
    endpoint_spec().

%% Create an endpoint with OpenAPI operation documentation
spectra_openapi:endpoint(Method, Path, Doc) ->
    endpoint_spec().

%% Build a response, then add it to an endpoint
spectra_openapi:response(StatusCode, Description) ->
    response_spec().
spectra_openapi:response_with_body(Response, Module, Schema) ->
    response_spec().
spectra_openapi:response_with_body(Response, Module, Schema, ContentType) ->
    response_spec().
spectra_openapi:response_with_header(Response, HeaderName, Module, HeaderSpec) ->
    response_spec().
spectra_openapi:add_response(Endpoint, Response) ->
    endpoint_spec().

%% Add request body (content type defaults to application/json)
spectra_openapi:with_request_body(Endpoint, Module, Schema) ->
    endpoint_spec().
%% Add request body with a custom content type (must be a binary)
spectra_openapi:with_request_body(Endpoint, Module, Schema, ContentType :: binary()) ->
    endpoint_spec().

%% Add parameters (path, query, header, cookie)
spectra_openapi:with_parameter(Endpoint, Module, ParameterSpec) ->
    endpoint_spec().

%% Generate complete OpenAPI spec (returns encoded JSON iodata)
spectra_openapi:endpoints_to_openapi(Metadata, Endpoints) ->
    {ok, json:encode_value() | iodata()} | {error, [spectra:error()]}.

%% Generate complete OpenAPI spec with options
spectra_openapi:endpoints_to_openapi(Metadata, Endpoints, Options) ->
    {ok, json:encode_value() | iodata()} | {error, [spectra:error()]}.
```

The `Options` list is passed to `spectra:encode/5` and controls the output format via the `pre_encoded` option:

| Options | Return value on success |
|---------|------------------------|
| `[]` or `[{pre_encoded, false}]` (default) | `{ok, iodata()}` — an encoded JSON binary |
| `[pre_encoded]` or `[{pre_encoded, true}]` | `{ok, json:encode_value()}` — a decoded map |

```erlang
%% Default: get encoded JSON iodata, e.g. to write to a file or HTTP response
{ok, Json} = spectra_openapi:endpoints_to_openapi(Meta, Endpoints),
file:write_file("openapi.json", Json).

%% Get a decoded map for inspection or further processing
{ok, Spec} = spectra_openapi:endpoints_to_openapi(Meta, Endpoints, [pre_encoded]),
```

The `Doc` map in `endpoint/3` can contain any of the following OpenAPI operation fields:
- `summary` — short summary of the endpoint (binary)
- `description` — detailed description (binary)
- `operationId` — unique identifier for the operation (binary)
- `tags` — list of tags for grouping (list of binaries)
- `deprecated` — whether the endpoint is deprecated (boolean)
- `externalDocs` — external documentation link (map with `url` and optional `description`)

```erlang
spectra_openapi:endpoint(get, <<"/users">>, #{
    summary => <<"List users">>,
    description => <<"Returns all users in the system">>,
    operationId => <<"listUsers">>,
    tags => [<<"users">>]
}).
```

The `ParameterSpec` map in `with_parameter/3` supports the following fields:
- `name` — parameter name (binary, required)
- `in` — parameter location: `path | query | header | cookie` (required)
- `required` — whether the parameter is required (boolean, required)
- `schema` — type reference or direct type (`spectra:sp_type_or_ref()`, required)

For both `with_request_body` and `with_parameter`, `description` and `deprecated` are sourced automatically from the `-spectra()` annotation on the schema type. There is no parameter for overriding them at the call site — annotate the type instead:

```erlang
-spectra(#{description => <<"User to create">>, deprecated => false}).
-type create_user_request() :: #create_user_request{}.
```

The `Metadata` map in `endpoints_to_openapi/2,3` supports the following fields:
- `title` — API title (binary, required)
- `version` — API version (binary, required)
- `summary` — short summary of the API (binary)
- `description` — longer description of the API (binary)
- `terms_of_service` — URL to the terms of service (binary)
- `contact` — contact information map with optional `name`, `url`, `email` fields (binary values)
- `license` — license map with required `name` and optional `url` or `identifier` (binary values)
- `servers` — list of server objects, each with required `url` and optional `description` (binary values)


## Error Handling

Spectra uses two different error handling strategies depending on the type of error:

### Returned Errors (`{error, [spectra:error()]}`)

Data validation errors are returned as `{error, [#sp_error{}]}` tuples. These occur when input data doesn't match the expected type during encoding/decoding.

Example:
```erlang
BadSourceJson = <<"[{\"number\":\"+1-555-123-4567\",\"verified\":{\"source\":\"a_bad_source\",\"confidence\":\"high\"},\"sms_capable\":true}]">>.

{error, [#sp_error{...}]} = json_to_contacts(BadSourceJson).
```

`#sp_error{}` contains:

- `location` — list of field names / list indices tracing the path from the root to the failing value, e.g. `[contacts, 0, verified, source]`
- `type` — one of:
  - `type_mismatch` — value did not match the expected type
  - `missing_data` — a required field was absent
  - `not_matched_fields` — an exact typed-map field had no matching keys in the data
  - `no_match` — no branch of a union type matched (sub-errors per branch are in `ctx`)
- `ctx` — map with at least `#{type => ExpectedType, value => ActualValue}`; for `no_match` also includes `#{errors => [{BranchType, [#sp_error{}]}]}`

### Raised Exceptions

Configuration and structural errors raise exceptions. These occur when:
- Module not found, not loaded, or not compiled with `debug_info`
- Type or record not found in module (e.g., `{type_or_record_not_found, TypeName}`)
- Unsupported type used (e.g., `pid()`, `port()`, `tuple()`)

These errors indicate a problem with your application's configuration or type definitions, not with the data being processed.

### Extra Fields in JSON (Deserialization)

When **deserializing JSON into Erlang** (using `spectra:decode/4`), extra fields that are not defined in the type are **silently ignored** for maps, records, and structs. This lenient behavior allows for flexible API evolution and backwards compatibility.

Example:
```erlang
-type user() :: #{name := binary(), age := integer()}.

%% JSON with extra fields is accepted during deserialization
Json = <<"{\"name\":\"Alice\",\"age\":30,\"extra\":\"ignored\"}">>,
{ok, #{name := <<"Alice">>, age := 30}} = spectra:decode(json, ?MODULE, user, Json).
```

**Note:** The `not_matched_fields` error is still raised during **serialization** (Erlang → JSON) when encoding data with exact typed map fields that don't match the provided data structure.

## Special Handling

### `undefined` and `nil` Values

The atoms `undefined` and `nil` have special handling in JSON serialization to represent missing or null values.

**Encoding (Erlang → JSON):**
- Fields with `undefined` or `nil` values are omitted from the JSON output
- Example: `#{name => <<"John">>, email => undefined}` encodes to `{"name":"John"}`

**Decoding (JSON → Erlang):**

The behavior depends on whether fields are mandatory (`:=`) or optional (`=>`):

**Mandatory fields** (`:=`), **record fields**, and **Elixir struct fields**:
- Missing JSON fields decode to `undefined` or `nil` if the type includes that literal
- Explicit JSON `null` values also decode to `undefined` or `nil` if the type includes that literal
- Example with type `#{email := binary() | undefined}`:
  - `{}` (missing field) → `#{email => undefined}`
  - `{"email": null}` → `#{email => undefined}`
  - `{"email": "test@example.com"}` → `#{email => <<"test@example.com">>}`

**Optional fields** (`=>`):
- Missing JSON fields result in the key being absent from the map entirely
- Explicit JSON `null` values decode to `undefined` or `nil` if the type includes that literal
- Example with type `#{email => binary() | undefined}`:
  - `{}` (missing field) → `#{}` (key absent)
  - `{"email": null}` → `#{email => undefined}` (key present)
  - `{"email": "test@example.com"}` → `#{email => <<"test@example.com">>}` (key present)

**Note on record and struct fields**: Erlang record fields and Elixir struct fields behave the same as mandatory map fields (`:=`). When a field is missing from JSON, it will be filled with `undefined` or `nil` if the field type includes that literal. For example, a record field `email :: binary() | undefined` will decode `{}` to a record with `email = undefined`.

**Note**: If a union type includes both `undefined` and `nil` (e.g., `integer() | undefined | nil`), the selection of which missing value to use depends on the order they appear in the type definition. The last one encountered will be used. For predictable behavior, include only one missing value literal in your type definitions. The `nil` atom is primarily for Elixir interoperability.

### Maps with Typed and Literal Fields

When a map has both typed fields (e.g., `binary() => integer()`) and exact literal fields (`:=`), the literal fields take precedence for their specific keys. For predictable behavior, use exact fields (`:=`) for literal values.

### `term()` | `any()`

When using types with `term`, `spectra_json` will not reject any data, which means it can return data that `json.erl` cannot convert to JSON.


### Char

Char is currently handled as integer, which is probably not what you want. Try to not use the char type for now. This is documented in `test/char_test.erl`.

### Unsupported Types

Each format supports a subset of Erlang types. For JSON serialization and schema, the following are not supported:
- `maybe_improper_list()` - Currently returns an error
- `pid()`, `port()`, `reference()` - Cannot be serialized to JSON
- `tuple()`, `bitstring()`, `nonempty_bitstring()` - Not JSON-compatible
- Function types - Cannot be serialized

It would be interesting to add support for key value lists, but as it isn't a native type in erlang, I haven't gotten around to it yet.

## Configuration

### Application Environment Variables

You can configure spectra behavior using application environment variables:

#### `module_types_cache`
- **Type**: `persistent | local | none`
- **Default**: `local`
- **Description**: Controls caching of extracted type information for modules.
  - `persistent` — stores type info in `persistent_term`, shared across all processes. Fastest for read-heavy workloads. Writes are expensive and trigger a global GC scan.
  - `local` — stores type info in the calling process's dictionary for the duration of a single `spectra:decode/encode/schema` call. Automatically cleared on return. Useful for request-scoped caching without the global write cost of `persistent_term`.
  - `none` — no caching; type info is always re-extracted from BEAM debug info.
- **Note**: With `persistent`, cached type info remains until you explicitly clear it with `spectra_module_types:clear/1`. With `local`, the cache only exists for a single `spectra:decode/encode/schema` call and is automatically cleared when that call returns, so type changes are picked up on the next call.
- **Recommendation**: Use `persistent` in production systems where no hot code reloading is done. Use `local` when you want per-call caching without affecting other processes.

#### `check_unicode`
- **Type**: `boolean()`
- **Default**: `false`
- **Description**: When set to `true`, enables additional Unicode validation for list-type string data. Disable for better performance when Unicode validity is guaranteed by other means.

Example configuration in `sys.config`:

```erlang
{spectra, [
    {module_types_cache, local},
    {check_unicode, false}
]}.
```

## Related Projects

- **[elli_openapi](https://hex.pm/packages/elli_openapi)** - elli middleware for automatic OpenAPI spec generation and validation using spectra
- **[spectral](https://hex.pm/packages/spectral)** - Elixir wrapper for spectra
- **[phoenix_spectral](https://hex.pm/packages/phoenix_spectral)** - Phoenix integration for spectral

## Development Status

This library is under active development. The core API is stabilising — breaking changes will be noted in the changelog.

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

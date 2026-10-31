-module(spectra_openapi).

-include("../include/spectra_internal.hrl").

-export([
    endpoint/2, endpoint/3,
    webhook/2, webhook/3,
    with_request_body/3, with_request_body/4,
    with_parameter/3,
    with_nil_fields/2,
    endpoints_to_openapi/2, endpoints_to_openapi/3,
    to_openapi/4,
    format_error/2,
    response/2,
    response_with_body/3, response_with_body/4,
    response_with_header/4,
    add_response/2
]).

-ignore_xref([
    {spectra_openapi, endpoint, 2},
    {spectra_openapi, endpoint, 3},
    {spectra_openapi, webhook, 2},
    {spectra_openapi, webhook, 3},
    {spectra_openapi, with_request_body, 3},
    {spectra_openapi, with_request_body, 4},
    {spectra_openapi, with_parameter, 3},
    {spectra_openapi, with_nil_fields, 2},
    {spectra_openapi, format_error, 2},
    {spectra_openapi, endpoints_to_openapi, 2},
    {spectra_openapi, endpoints_to_openapi, 3},
    {spectra_openapi, to_openapi, 4},
    {spectra_openapi, response, 2},
    {spectra_openapi, response_with_body, 3},
    {spectra_openapi, response_with_body, 4},
    {spectra_openapi, response_with_header, 4},
    {spectra_openapi, add_response, 2}
]).

-export_type([
    endpoint_spec/0,
    webhook_spec/0,
    endpoint_doc/0,
    response_spec/0,
    parameter_spec/0,
    parameter_input_spec/0,
    http_method/0,
    http_status_code/0,
    openapi_metadata/0,
    openapi_spec/0,
    openapi_webhooks/0
]).

-define(DEFAULT_CONTENT_TYPE, <<"application/json">>).

-type http_method() :: get | put | post | delete | options | head | patch | trace.
%% The binary is an OpenAPI 3.1 status code range, "1XX" to "5XX".
-type http_status_code() :: 100..599 | <<_:24>>.
-type parameter_location() :: path | query | header | cookie.
-type openapi_schema() :: json:encode_value() | #{'$ref' := binary()}.
%% Every key here must also appear in openapi_operation().
-type endpoint_doc() ::
    #{
        summary => binary(),
        description => binary(),
        operationId => binary(),
        tags => [binary()],
        deprecated => boolean(),
        externalDocs => #{description => binary(), url := binary()},
        security => [openapi_security_requirement()]
    }.
-type request_body_spec() ::
    #{
        schema := spectra:sp_type_or_ref(),
        module := module(),
        content_type => binary()
    }.
-type response_header_input_spec() ::
    #{
        required => boolean(),
        schema := spectra:sp_type_or_ref()
    }.
-type response_header_spec() ::
    #{
        required => boolean(),
        schema := spectra:sp_type_or_ref(),
        module := module()
    }.
-type response_spec() ::
    #{
        description := binary(),
        schema => spectra:sp_type_or_ref(),
        module => module(),
        status_code => http_status_code(),
        content_type => binary(),
        headers => #{binary() => response_header_spec()}
    }.
-type parameter_spec() ::
    #{
        name := binary(),
        in := parameter_location(),
        required := boolean(),
        schema := spectra:sp_type_or_ref(),
        module := module()
    }.
%% The map passed by the caller to with_parameter/3 — module is added by the function.
-type parameter_input_spec() ::
    #{
        name := binary(),
        in := parameter_location(),
        required := boolean(),
        schema := spectra:sp_type_or_ref()
    }.
-type openapi_server() :: #{url := binary(), description => binary()}.
-type openapi_contact() :: #{name => binary(), url => binary(), email => binary()}.
-type openapi_license() :: #{name := binary(), url => binary(), identifier => binary()}.
-type openapi_security_scheme() :: json:encode_value().
-type openapi_security_requirement() :: #{binary() => [binary()]}.
-type openapi_metadata() ::
    #{
        title := binary(),
        version := binary(),
        summary => binary(),
        description => binary(),
        terms_of_service => binary(),
        contact => openapi_contact(),
        license => openapi_license(),
        servers => [openapi_server()],
        security_schemes => #{binary() => openapi_security_scheme()},
        security => [openapi_security_requirement()]
    }.
-type endpoint_spec() ::
    #{
        method := http_method(),
        path := binary(),
        responses := #{http_status_code() => response_spec()},
        parameters := [parameter_spec()],
        request_body => request_body_spec(),
        doc => endpoint_doc()
    }.
-type webhook_spec() ::
    #{
        name := binary(),
        method := http_method(),
        responses := #{http_status_code() => response_spec()},
        parameters := [parameter_spec()],
        request_body => request_body_spec(),
        nil_fields => null,
        doc => endpoint_doc()
    }.
-type path_operations() :: #{http_method() => openapi_operation()}.
-type openapi_webhooks() :: #{binary() => path_operations()}.
-type openapi_operation() ::
    #{
        summary => binary(),
        description => binary(),
        operationId => binary(),
        tags => [binary()],
        deprecated => boolean(),
        externalDocs => #{description => binary(), url := binary()},
        security => [openapi_security_requirement()],
        responses => #{binary() => openapi_response()},
        requestBody => openapi_request_body(),
        parameters => [openapi_parameter()]
    }.
-type openapi_response() ::
    #{
        description := binary(),
        content => #{binary() => #{schema := openapi_schema()}},
        headers => #{binary() => openapi_header()}
    }.
-type openapi_header() ::
    #{
        description => binary(),
        required => boolean(),
        deprecated => boolean(),
        schema := openapi_schema()
    }.
-type openapi_request_body() ::
    #{
        required := boolean(),
        content := #{binary() => #{schema := openapi_schema()}},
        description => binary()
    }.
-type openapi_parameter() ::
    #{
        name := binary(),
        in := parameter_location(),
        required := boolean(),
        schema := openapi_schema(),
        description => binary(),
        deprecated => boolean()
    }.
-type openapi_spec() ::
    #{
        openapi := binary(),
        info :=
            #{
                title := binary(),
                version := binary(),
                summary => binary(),
                description => binary(),
                termsOfService => binary(),
                contact => openapi_contact(),
                license => openapi_license()
            },
        paths := #{binary() => path_operations()},
        webhooks => openapi_webhooks(),
        servers => [openapi_server()],
        security => [openapi_security_requirement()],
        components =>
            #{
                schemas => #{binary() => openapi_schema()},
                securitySchemes => #{binary() => openapi_security_scheme()}
            }
    }.

-doc """
Creates a basic endpoint specification.

Equivalent to calling endpoint/3 with an empty documentation map.

### Returns
Endpoint map with method and path set
""".
-doc #{
    equiv => endpoint(Method, Path, #{}),
    params =>
        #{
            "Method" => "HTTP method (get, post, put, delete, patch, head, options)",
            "Path" => "URL path for the endpoint (e.g., \"/users/{id}\")"
        }
}.

-spec endpoint(Method :: http_method(), Path :: binary()) -> endpoint_spec().
endpoint(Method, Path) when is_atom(Method) andalso is_binary(Path) ->
    endpoint(Method, Path, #{}).

-doc """
Creates an endpoint specification with documentation.

This function creates the foundation for an endpoint with the specified HTTP method, path, and documentation.
Additional details like responses, request body, and parameters can be added using the with_* functions.

### Documentation Fields
The Doc map can contain:
- summary: Short summary of the endpoint (binary)
- description: Detailed description (binary)
- operationId: Unique identifier for the operation (binary)
- tags: List of tags for grouping (list of binaries)
- deprecated: Whether the endpoint is deprecated (boolean)
- externalDocs: External documentation link (map with url and optional description)
- security: Security requirements for this operation, overriding the global default ([] opts out)

### Returns
Endpoint map with method, path, and documentation set
""".
-doc #{
    params =>
        #{
            ~"Doc" => ~"Documentation map with summary, description, operationId, tags, etc.",
            ~"Method" => ~"HTTP method (get, post, put, delete, patch, head, options)",
            ~"Path" => ~"URL path for the endpoint (e.g., \"/users/{id}\")"
        }
}.

-spec endpoint(Method :: http_method(), Path :: binary(), Doc :: endpoint_doc()) ->
    endpoint_spec().
endpoint(Method, Path, Doc) when is_atom(Method) andalso is_binary(Path) andalso is_map(Doc) ->
    #{
        method => Method,
        path => Path,
        responses => #{},
        parameters => [],
        doc => Doc
    }.

-doc """
Creates a webhook specification.

Equivalent to calling webhook/3 with an empty documentation map.

### Returns
Webhook map with name and method set
""".
-doc #{
    equiv => webhook(Name, Method, #{}),
    params =>
        #{
            "Method" => "HTTP method the API uses when calling the consumer (usually post)",
            "Name" => "Event name identifying the webhook (e.g., \"userCreated\")"
        }
}.

-spec webhook(Name :: binary(), Method :: http_method()) -> webhook_spec().
webhook(Name, Method) when is_binary(Name) andalso is_atom(Method) ->
    webhook(Name, Method, #{}).

-doc """
Creates a webhook specification with documentation.

Webhooks are keyed by event name and emitted under the OpenAPI 3.1 top-level
`webhooks` key. The request body is the payload this API sends, and the
responses describe what the consumer returns. Build them with the same
with_* functions as endpoints; path and query parameters are rejected.

### Documentation Fields
The Doc map takes the same fields as endpoint/3.

### Example
```erlang
Webhook = spectra_openapi:webhook(<<"userCreated">>, post, #{summary => <<"User created">>}),
Webhook1 = spectra_openapi:with_request_body(Webhook, Module, {type, user, 0}),
Webhook2 = spectra_openapi:add_response(
    Webhook1, spectra_openapi:response(200, <<"Acknowledged">>)
).
```

### Returns
Webhook map with name, method, and documentation set
""".
-doc #{
    params =>
        #{
            "Doc" => "Documentation map with summary, description, operationId, tags, etc.",
            "Method" => "HTTP method the API uses when calling the consumer (usually post)",
            "Name" => "Event name identifying the webhook (e.g., \"userCreated\")"
        }
}.

-spec webhook(Name :: binary(), Method :: http_method(), Doc :: endpoint_doc()) ->
    webhook_spec().
webhook(Name, Method, Doc) when is_binary(Name) andalso is_atom(Method) andalso is_map(Doc) ->
    #{
        name => Name,
        method => Method,
        responses => #{},
        parameters => [],
        doc => Doc
    }.

-doc """
Creates a response builder for constructing response specifications.

This function creates a response builder that can be incrementally configured with body and headers
before being added to an endpoint using add_response/2.

### Example
```erlang
Response = spectra_openapi:response(200, <<"Success">>),
Response2 = spectra_openapi:response_with_body(Response, Module, Schema),
Response3 = spectra_openapi:response_with_header(Response2, <<"X-Rate-Limit">>, Module, HeaderSpec),
Endpoint = spectra_openapi:add_response(Endpoint1, Response3).
```

### Returns
Response builder map with status code and description
""".
-doc #{
    params =>
        #{
            "Description" => "Human-readable description of the response",
            "StatusCode" =>
                "HTTP status code (e.g., 200, 404, 500), or a status code range "
                "<<\"1XX\">> to <<\"5XX\">>"
        }
}.

-spec response(StatusCode :: http_status_code(), Description :: binary()) ->
    response_spec().
response(StatusCode, Description) when
    (is_integer(StatusCode) orelse
        StatusCode =:= <<"1XX">> orelse
        StatusCode =:= <<"2XX">> orelse
        StatusCode =:= <<"3XX">> orelse
        StatusCode =:= <<"4XX">> orelse
        StatusCode =:= <<"5XX">>) andalso
        is_binary(Description)
->
    #{status_code => StatusCode, description => Description}.

-doc """
Adds a complete response specification to an endpoint.

This function adds a response that was built using the response builder pattern:
response/2, response_with_body/3-4, and response_with_header/4.

### Example
```erlang
Response = spectra_openapi:response(200, <<"Success">>),
Response2 = spectra_openapi:response_with_body(Response, Module, Schema),
Response3 = spectra_openapi:response_with_header(Response2, <<"X-Rate-Limit">>, Module, HeaderSpec),
Endpoint = spectra_openapi:add_response(Endpoint1, Response3).
```

### Returns
Updated endpoint map with the response added
""".
-doc #{
    params =>
        #{
            "Endpoint" => "Endpoint or webhook map to add the response to",
            "Response" => "Response specification built with response/2 and related functions"
        }
}.

-spec add_response(
    Endpoint :: endpoint_spec() | webhook_spec(),
    Response :: response_spec()
) ->
    endpoint_spec() | webhook_spec().
add_response(Endpoint, Response) when is_map(Endpoint) andalso is_map(Response) ->
    {StatusCode, ResponseWithoutStatusCode} = maps:take(status_code, Response),
    Responses = maps:get(responses, Endpoint, #{}),
    Endpoint#{responses => Responses#{StatusCode => ResponseWithoutStatusCode}}.

-doc """
Adds a response body to a response builder.

This function sets the schema and module for the response body.
Use this with response/2 to build up a complete response specification.

### Returns
Updated response builder with body schema added
""".
-doc #{
    params =>
        #{
            "Module" => "Module containing the type definition",
            "Response" => "Response builder created with response/2",
            "Schema" => "Schema reference or direct type (spectra:sp_type_or_ref())"
        }
}.

-spec response_with_body(
    Response :: response_spec(),
    Module :: module(),
    Schema :: spectra:sp_type_or_ref()
) ->
    response_spec().
response_with_body(Response, Module, Schema) when
    is_map(Response) andalso is_atom(Module)
->
    Response#{schema => Schema, module => Module}.

-doc """
Adds a response body with custom content type to a response builder.

This function sets the schema, module, and content type for the response body.
Use this with response/2 to build up a complete response specification.

### Returns
Updated response builder with body schema and content type added
""".
-doc #{
    params =>
        #{
            "ContentType" =>
                "Content type for the response body (e.g., \"application/json\", \"application/xml\")",
            "Module" => "Module containing the type definition",
            "Response" => "Response builder created with response/2",
            "Schema" => "Schema reference or direct type (spectra:sp_type_or_ref())"
        }
}.

-spec response_with_body(
    Response :: response_spec(),
    Module :: module(),
    Schema :: spectra:sp_type_or_ref(),
    ContentType :: binary()
) ->
    response_spec().
response_with_body(Response, Module, Schema, ContentType) when
    is_map(Response) andalso is_atom(Module) andalso is_binary(ContentType)
->
    Response#{
        schema => Schema,
        module => Module,
        content_type => ContentType
    }.

-doc """
Adds a header to a response builder.

This function adds a header specification to the response being built.
Multiple headers can be added by calling this function multiple times.

`description` and `deprecated` for the header are sourced automatically from the
`-spectra()` annotation on the schema type.

### Returns
Updated response builder with header added
""".
-doc #{
    params =>
        #{
            "HeaderName" => "Name of the response header (e.g., \"X-Rate-Limit\")",
            "HeaderSpec" => "Header specification (response_header_input_spec map)",
            "Module" => "Module containing the type definition",
            "Response" => "Response builder created with response/2"
        }
}.

-spec response_with_header(
    Response :: response_spec(),
    HeaderName :: binary(),
    Module :: module(),
    HeaderSpec :: response_header_input_spec()
) ->
    response_spec().
response_with_header(Response, HeaderName, Module, HeaderSpec) when
    is_map(Response) andalso
        is_binary(HeaderName) andalso
        is_atom(Module) andalso
        is_map(HeaderSpec)
->
    Headers = maps:get(headers, Response, #{}),
    HeaderSpecWithModule = HeaderSpec#{module => Module},
    Response#{headers => Headers#{HeaderName => HeaderSpecWithModule}}.

-doc """
Adds a request body to an endpoint.

This function sets the request body schema for the endpoint.
Typically used with POST, PUT, and PATCH endpoints. The content type defaults
to `application/json`. Use `with_request_body/4` to override it.

The `description` and `deprecated` fields in the generated OpenAPI
`requestBody` object are sourced automatically from the `-spectra()`
attribute on the schema type — there is no parameter for overriding them
on this call.

### Example

```erlang
-spectra(#{description => <<"User to create">>}).
-type create_user_request() :: #create_user_request{}.

Endpoint = spectra_openapi:with_request_body(
    spectra_openapi:endpoint(post, <<"/users">>),
    my_module,
    create_user_request
).
```

### Returns
Updated endpoint map with request body set
""".
-doc #{
    params =>
        #{
            "Endpoint" => "Endpoint or webhook map to add the request body to",
            "Module" => "Module containing the type definition",
            "Schema" => "Schema reference or direct type (spectra:sp_type_or_ref())"
        }
}.

-spec with_request_body(
    Endpoint :: endpoint_spec() | webhook_spec(),
    Module :: module(),
    Schema :: spectra:sp_type_or_ref()
) ->
    endpoint_spec() | webhook_spec().
with_request_body(Endpoint, Module, Schema) when
    is_map(Endpoint) andalso is_atom(Module)
->
    Endpoint#{request_body => #{schema => Schema, module => Module}}.

-doc """
Adds a request body to an endpoint with a custom content type.

Like `with_request_body/3` but overrides the default content type
(`application/json`). `ContentType` must be a binary such as
`<<"application/xml">>`.

The `description` and `deprecated` fields in the generated OpenAPI
`requestBody` object are sourced automatically from the `-spectra()`
attribute on the schema type.

### Example

```erlang
Endpoint = spectra_openapi:with_request_body(
    spectra_openapi:endpoint(post, <<"/upload">>),
    my_module,
    upload_request,
    <<"application/octet-stream">>
).
```

### Returns
Updated endpoint map with request body set
""".
-doc #{
    params =>
        #{
            "ContentType" =>
                "Content type binary for the request body (e.g., <<\"application/xml\">>). "
                "Must be a binary — passing a map will cause a function_clause error.",
            "Endpoint" => "Endpoint or webhook map to add the request body to",
            "Module" => "Module containing the type definition",
            "Schema" => "Schema reference or direct type (spectra:sp_type_or_ref())"
        }
}.

-spec with_request_body(
    Endpoint :: endpoint_spec() | webhook_spec(),
    Module :: module(),
    Schema :: spectra:sp_type_or_ref(),
    ContentType :: binary()
) ->
    endpoint_spec() | webhook_spec().
with_request_body(Endpoint, Module, Schema, ContentType) when
    is_map(Endpoint) andalso is_atom(Module) andalso is_binary(ContentType)
->
    Endpoint#{request_body => #{schema => Schema, module => Module, content_type => ContentType}}.

-doc """
Adds a parameter specification to an endpoint.

This function adds a parameter (path, query, header, or cookie) to the endpoint.
Multiple parameters can be added by calling this function multiple times.

### Parameter Specification
The parameter spec should be a map with these keys:
- name: Parameter name (binary, required)
- in: Parameter location (path | query | header | cookie, required)
- required: Whether the parameter is required (boolean, required)
- schema: Schema reference or direct type (spectra:sp_type_or_ref(), required)

`description` and `deprecated` are sourced automatically from the `-spectra()` annotation
on the schema type and should not be included in the parameter spec.

### Returns
Updated endpoint map with the new parameter added
""".
-doc #{
    params =>
        #{
            "Endpoint" => "Endpoint or webhook map to add the parameter to",
            "Module" => "Module containing the type definition",
            "ParameterSpec" => "Parameter specification map"
        }
}.

-spec with_parameter(
    Endpoint :: endpoint_spec() | webhook_spec(),
    Module :: module(),
    ParameterSpec :: parameter_input_spec()
) ->
    endpoint_spec() | webhook_spec().
with_parameter(Endpoint, Module, #{name := Name} = ParameterSpec) when
    is_map(Endpoint) andalso
        is_atom(Module) andalso
        is_map(ParameterSpec) andalso
        is_binary(Name)
->
    ValidKeys = [name, in, required, schema],
    case maps:keys(maps:without(ValidKeys, ParameterSpec)) of
        [] -> ok;
        UnknownKeys -> error({unsupported_parameter_spec_keys, UnknownKeys})
    end,
    ok = check_parameter_location(Endpoint, ParameterSpec),
    Parameters = maps:get(parameters, Endpoint, []),
    ParameterWithModule = ParameterSpec#{module => Module},
    Endpoint#{parameters => [ParameterWithModule | Parameters]}.

-doc """
Makes the webhook payload write every missing value as `null`.

Use it when the code that sends the webhook encodes the payload with the
`{nil_fields, null}` option of `spectra:encode/5`. The payload schema then lists
every field whose type can be missing in `required` and allows `null` for it,
nested types included. Headers and response schemas do not change.

The payload type keeps its normal schema name. If the same type is also used
without this option, this webhook gets a second schema with `Nullable` added to
the name. Under the `nil_fields` setting `null` the option changes nothing, and
under `omit` `to_openapi/4` raises `badarg`, because that setting does not
allow the per-call `{nil_fields, null}` option.

### Returns
Webhook map with nil_fields set
""".
-doc #{
    params =>
        #{
            "NilFields" => "Must be `null`",
            "Webhook" => "Webhook specification created with webhook/2-3"
        }
}.

-spec with_nil_fields(Webhook :: webhook_spec(), NilFields :: null) -> webhook_spec().
with_nil_fields(#{name := _} = Webhook, null) ->
    Webhook#{nil_fields => null};
with_nil_fields(#{name := _}, Value) ->
    raise_badarg({webhook_nil_fields, {invalid_option, Value}});
with_nil_fields(_Endpoint, _Value) ->
    raise_badarg({webhook_nil_fields, not_a_webhook}).

%% Leaves the arguments out of the stack trace, as spectra:encode/5 does.
-spec raise_badarg(term()) -> no_return().
raise_badarg(Cause) ->
    erlang:error(badarg, none, [{error_info, #{module => ?MODULE, cause => Cause}}]).

-doc """
Formats the `badarg` errors raised by with_nil_fields/2 and to_openapi/4.
Called by `erl_error` and by Elixir to build the error message.
""".
-spec format_error(Reason :: term(), StackTrace :: erlang:stacktrace()) ->
    #{pos_integer() | general => unicode:chardata()}.
format_error(badarg, [{_M, _F, _As, Info} | _]) ->
    #{cause := Cause} = proplists:get_value(error_info, Info),
    format_cause(Cause).

-spec format_cause(term()) -> #{general => unicode:chardata()}.
format_cause({webhook_nil_fields, {invalid_option, Value}}) ->
    #{general => io_lib:format("the webhook nil_fields option must be null, got: ~tp", [Value])};
format_cause({webhook_nil_fields, not_a_webhook}) ->
    #{general => <<"nil_fields can be set only on webhooks, not on endpoints">>};
format_cause({webhook_nil_fields, {omit_setting, Name}}) ->
    #{
        general => io_lib:format(
            "webhook ~ts sets nil_fields to null, which is not allowed when the "
            "nil_fields setting is omit, because that schema does not allow null",
            [Name]
        )
    };
format_cause({webhook_nil_fields, {name_clash, SchemaName}}) ->
    #{
        general => io_lib:format(
            "the schema name ~ts is needed for a webhook payload with nil_fields null, "
            "but another type already has that name",
            [SchemaName]
        )
    }.

%% Webhooks are told apart from endpoints by their name key.
-spec check_parameter_location(
    endpoint_spec() | webhook_spec(),
    parameter_input_spec()
) ->
    ok.
check_parameter_location(#{name := WebhookName}, #{in := In}) when
    In =:= path orelse In =:= query
->
    error({parameter_location_not_supported_on_webhook, WebhookName, In});
check_parameter_location(_Endpoint, _ParameterSpec) ->
    ok.

-doc """
Generates a complete OpenAPI 3.1 specification from a list of endpoints.

This function takes a list of endpoint specifications and generates a complete OpenAPI document
with paths, operations, and component schemas.

### Returns
{ok, OpenAPISpec} containing the complete OpenAPI 3.1 document, or {error, Errors} if generation fails
""".
-doc #{
    params =>
        #{
            "Endpoints" =>
                "List of endpoint specifications created with endpoint/2 and with_* functions",
            "MetaData" => "OpenAPI metadata map with title and version"
        }
}.

-spec endpoints_to_openapi(
    MetaData :: openapi_metadata(),
    Endpoints :: [endpoint_spec()]
) ->
    {ok, json:encode_value() | iodata()} | {error, [spectra:error()]}.
endpoints_to_openapi(MetaData, Endpoints) ->
    endpoints_to_openapi(MetaData, Endpoints, []).

-spec endpoints_to_openapi(
    MetaData :: openapi_metadata(),
    Endpoints :: [endpoint_spec()],
    Options :: [spectra:schema_option()]
) ->
    {ok, json:encode_value() | iodata()} | {error, [spectra:error()]}.
endpoints_to_openapi(MetaData, Endpoints, Options) when is_list(Endpoints) ->
    to_openapi(MetaData, Endpoints, [], Options).

-doc """
Generates a complete OpenAPI 3.1 specification from endpoints and webhooks.

Webhooks are emitted under the top-level `webhooks` key, keyed by the event name
given to webhook/2-3. Their schemas share `components/schemas` with the
endpoints, so a type used by both is emitted once.

A global `security` requirement in the metadata is emitted at the top level and
therefore applies to webhook operations too, per OpenAPI - even though its
meaning is inverted there, since it would describe this API authenticating to
the consumer. Give a webhook its own `security` in its Doc map to override that,
or `security => []` to opt it out entirely.

### Returns
{ok, OpenAPISpec} containing the complete OpenAPI 3.1 document, or {error, Errors} if generation fails
""".
-doc #{
    params =>
        #{
            "Endpoints" =>
                "List of endpoint specifications created with endpoint/2 and with_* functions",
            "MetaData" => "OpenAPI metadata map with title and version",
            "Options" => "`pre_encoded` to return the spec as a map instead of JSON",
            "Webhooks" =>
                "List of webhook specifications created with webhook/2-3 and with_* functions"
        }
}.

-spec to_openapi(
    MetaData :: openapi_metadata(),
    Endpoints :: [endpoint_spec()],
    Webhooks :: [webhook_spec()],
    Options :: [spectra:schema_option()]
) ->
    {ok, json:encode_value() | iodata()} | {error, [spectra:error()]}.
to_openapi(MetaData, Endpoints, Webhooks, Options) when
    is_list(Endpoints) andalso is_list(Webhooks)
->
    Config = spectra:get_config(),
    ok = check_webhook_nil_fields(Webhooks, Config),
    try
        {ValueRefs, NullRefs, ParameterRefs} = collect_schema_refs(Endpoints ++ Webhooks, Config),
        MixedRefs = [Ref || Ref <- NullRefs, lists:member(Ref, ValueRefs)],
        Paths = generate_path_items(group_by(path, Endpoints), MixedRefs, Config),
        WebhookItems = generate_path_items(group_by(name, Webhooks), MixedRefs, Config),
        ComponentsResult = add_security_schemes(
            generate_components(
                ValueRefs,
                NullRefs -- MixedRefs,
                MixedRefs,
                ParameterRefs -- (ValueRefs ++ NullRefs),
                Config
            ),
            MetaData
        ),
        BaseInfo = #{title => maps:get(title, MetaData), version => maps:get(version, MetaData)},
        Info = lists:foldl(
            fun({MetaKey, InfoKey}, Acc) ->
                case maps:get(MetaKey, MetaData, undefined) of
                    undefined -> Acc;
                    Value -> Acc#{InfoKey => Value}
                end
            end,
            BaseInfo,
            [
                {summary, summary},
                {description, description},
                {terms_of_service, termsOfService},
                {contact, contact},
                {license, license}
            ]
        ),
        BaseSpec = #{
            openapi => <<"3.1.0">>, info => Info, paths => Paths, components => ComponentsResult
        },
        WithWebhooks =
            case map_size(WebhookItems) of
                0 -> BaseSpec;
                _ -> BaseSpec#{webhooks => WebhookItems}
            end,
        WithServers = copy_if_present(servers, MetaData, WithWebhooks),
        OpenAPISpec = copy_if_present(security, MetaData, WithServers),
        spectra:encode(json, ?MODULE, {type, openapi_spec, 0}, OpenAPISpec, Options)
    after
        spectra_module_types:clear_local()
    end.

-spec check_webhook_nil_fields([webhook_spec()], spectra:sp_config()) -> ok.
check_webhook_nil_fields(Webhooks, #sp_config{nil_fields = omit}) ->
    case [Name || #{name := Name, nil_fields := null} <- Webhooks] of
        [] -> ok;
        [Name | _] -> raise_badarg({webhook_nil_fields, {omit_setting, Name}})
    end;
check_webhook_nil_fields(_Webhooks, _Config) ->
    ok.

%% A webhook with nil_fields null needs its own payload schema only while the
%% setting leaves missing values out when encoding.
-spec null_payload(endpoint_spec() | webhook_spec(), spectra:sp_config()) -> boolean().
null_payload(#{nil_fields := null}, #sp_config{nil_fields = omit_or_null}) -> true;
null_payload(_Spec, _Config) -> false.

-spec null_config(spectra:sp_config()) -> spectra:sp_config().
null_config(Config) ->
    Config#sp_config{nil_fields = null}.

-spec group_by(path | name, [endpoint_spec() | webhook_spec()]) ->
    #{binary() => [endpoint_spec() | webhook_spec()]}.
group_by(Key, Specs) ->
    lists:foldl(
        fun(Spec, Acc) ->
            GroupKey = maps:get(Key, Spec),
            Group = maps:get(GroupKey, Acc, []),
            Acc#{GroupKey => [Spec | Group]}
        end,
        #{},
        Specs
    ).

-spec generate_path_items(
    #{binary() => [endpoint_spec() | webhook_spec()]},
    [{module(), spectra:sp_type_reference()}],
    spectra:sp_config()
) ->
    #{binary() => path_operations()}.
generate_path_items(Groups, MixedRefs, Config) ->
    maps:map(fun(_Key, Specs) -> generate_path_operations(Specs, MixedRefs, Config) end, Groups).

-spec generate_path_operations(
    [endpoint_spec() | webhook_spec()],
    [{module(), spectra:sp_type_reference()}],
    spectra:sp_config()
) ->
    path_operations().
generate_path_operations(Endpoints, MixedRefs, Config) ->
    lists:foldl(
        fun(#{method := Method} = Endpoint, Acc) ->
            Operation = generate_operation(Endpoint, MixedRefs, Config),
            Acc#{Method => Operation}
        end,
        #{},
        Endpoints
    ).

-spec generate_operation(
    endpoint_spec() | webhook_spec(),
    [{module(), spectra:sp_type_reference()}],
    spectra:sp_config()
) ->
    openapi_operation().
generate_operation(Endpoint, MixedRefs, Config) ->
    %% Start with documentation if present
    Operation = maps:get(doc, Endpoint, #{}),

    %% Add responses
    Responses = maps:get(responses, Endpoint, #{}),
    OperationWithResponses =
        case maps:size(Responses) > 0 of
            true ->
                OpenAPIResponses =
                    maps:map(
                        fun(_StatusCode, ResponseSpec) ->
                            generate_response(ResponseSpec, Config)
                        end,
                        Responses
                    ),
                OpenAPIResponsesBinary =
                    maps:fold(
                        fun
                            (K, V, NewAcc) when is_integer(K) ->
                                NewAcc#{integer_to_binary(K) => V};
                            (K, V, NewAcc) when is_binary(K) ->
                                NewAcc#{K => V}
                        end,
                        #{},
                        OpenAPIResponses
                    ),
                Operation#{responses => OpenAPIResponsesBinary};
            false ->
                Operation
        end,

    OperationWithBody =
        case maps:get(request_body, Endpoint, undefined) of
            undefined ->
                OperationWithResponses;
            RequestBodyRef ->
                RequestBody =
                    case null_payload(Endpoint, Config) of
                        true ->
                            generate_request_body(RequestBodyRef, MixedRefs, null_config(Config));
                        false ->
                            generate_request_body(RequestBodyRef, [], Config)
                    end,
                OperationWithResponses#{requestBody => RequestBody}
        end,

    Parameters = maps:get(parameters, Endpoint, []),
    case Parameters of
        [] ->
            OperationWithBody;
        _ ->
            OpenAPIParameters = lists:map(fun(P) -> generate_parameter(P, Config) end, Parameters),
            OperationWithBody#{parameters => OpenAPIParameters}
    end.

-spec generate_response(response_spec(), spectra:sp_config()) -> openapi_response().
generate_response(#{description := Description} = ResponseSpec, Config) when
    is_binary(Description)
->
    %% Build base response with optional content
    BaseResponse =
        case
            {maps:get(schema, ResponseSpec, undefined), maps:get(module, ResponseSpec, undefined)}
        of
            {undefined, _} ->
                %% No schema means header-only response (no content)
                #{description => Description};
            {_, undefined} ->
                %% Schema without module should not happen, but handle defensively
                #{description => Description};
            {#sp_literal{value = NilValue}, _Module} when
                NilValue =:= nil orelse NilValue =:= undefined
            ->
                #{description => Description};
            {Schema, Module} ->
                ModuleTypeInfo = spectra_module_types:get(
                    Module, Config
                ),
                NormalizedSchema = spectra_util:normalize_type_ref(ModuleTypeInfo, Schema),
                SchemaContent =
                    case NormalizedSchema of
                        {type, Name, Arity} ->
                            SchemaName = schema_component_name(Module, {type, Name, Arity}),
                            #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>};
                        {record, Name} ->
                            SchemaName = schema_component_name(Module, {record, Name}),
                            #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>};
                        #sp_list{
                            type = #sp_remote_type{
                                mfargs = {ItemMod, ItemName, _}, arity = ItemArity
                            }
                        } ->
                            ItemSchemaName = schema_component_name(
                                ItemMod, {type, ItemName, ItemArity}
                            ),
                            #{
                                type => <<"array">>,
                                items =>
                                    #{
                                        '$ref' =>
                                            <<"#/components/schemas/", ItemSchemaName/binary>>
                                    }
                            };
                        #sp_remote_type{mfargs = {RemoteMod, RemoteName, _}, arity = RemoteArity} ->
                            SchemaName = schema_component_name(
                                RemoteMod, {type, RemoteName, RemoteArity}
                            ),
                            #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>};
                        DirectType ->
                            InlineSchema =
                                spectra_json_schema:to_schema(ModuleTypeInfo, DirectType, Config),
                            maps:remove('$schema', InlineSchema)
                    end,
                ContentType = maps:get(content_type, ResponseSpec, ?DEFAULT_CONTENT_TYPE),
                #{
                    description => Description,
                    content => #{ContentType => #{schema => SchemaContent}}
                }
        end,

    %% Add headers if present
    case maps:get(headers, ResponseSpec, #{}) of
        HeadersSpec when map_size(HeadersSpec) =:= 0 ->
            BaseResponse;
        HeadersSpec ->
            GeneratedHeaders =
                maps:map(
                    fun(_HeaderName, HeaderSpec) ->
                        generate_response_header(HeaderSpec, Config)
                    end,
                    HeadersSpec
                ),
            BaseResponse#{headers => GeneratedHeaders}
    end.

-spec copy_if_present(atom(), map(), map()) -> map().
copy_if_present(Key, Source, Target) ->
    case maps:get(Key, Source, undefined) of
        undefined -> Target;
        Value -> Target#{Key => Value}
    end.

-spec add_security_schemes(map(), openapi_metadata()) -> map().
add_security_schemes(Components, MetaData) ->
    case maps:get(security_schemes, MetaData, undefined) of
        undefined -> Components;
        SecuritySchemes -> Components#{securitySchemes => SecuritySchemes}
    end.

-spec generate_response_header(response_header_spec(), spectra:sp_config()) -> openapi_header().
generate_response_header(#{schema := Schema, module := Module} = HeaderSpec, Config) ->
    ModuleTypeInfo = spectra_module_types:get(Module, Config),
    NormalizedSchema = spectra_util:normalize_type_ref(ModuleTypeInfo, Schema),
    {Doc, OpenApiSchema} = lift_doc(ModuleTypeInfo, NormalizedSchema, parameter_config(Config)),
    Base = Doc#{schema => OpenApiSchema},
    copy_if_present(required, HeaderSpec, Base).

%% A ref in MixedRefs points to the Nullable copy of its component.
-spec generate_request_body(
    request_body_spec(), [{module(), spectra:sp_type_reference()}], spectra:sp_config()
) -> openapi_request_body().
generate_request_body(
    #{schema := Schema, module := Module} = RequestBodySpec, MixedRefs, Config
) ->
    ModuleTypeInfo = spectra_module_types:get(Module, Config),
    NormalizedSchema = spectra_util:normalize_type_ref(ModuleTypeInfo, Schema),
    SchemaContent =
        case NormalizedSchema of
            {type, Name, Arity} ->
                SchemaName = body_component_name(Module, {type, Name, Arity}, MixedRefs),
                #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>};
            {record, Name} ->
                SchemaName = body_component_name(Module, {record, Name}, MixedRefs),
                #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>};
            #sp_remote_type{mfargs = {RemoteMod, RemoteName, _}, arity = RemoteArity} ->
                SchemaName = body_component_name(
                    RemoteMod, {type, RemoteName, RemoteArity}, MixedRefs
                ),
                #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>};
            DirectType ->
                InlineSchema = spectra_json_schema:to_schema(ModuleTypeInfo, DirectType, Config),
                OpenApiSchema = maps:remove('$schema', InlineSchema),
                OpenApiSchema
        end,

    ContentType = maps:get(content_type, RequestBodySpec, ?DEFAULT_CONTENT_TYPE),
    Doc = maps:with([description], type_doc(ModuleTypeInfo, NormalizedSchema, Config)),
    Base = Doc#{required => true, content => #{ContentType => #{schema => SchemaContent}}},
    Base.

-spec body_component_name(
    module(), spectra:sp_type_reference(), [{module(), spectra:sp_type_reference()}]
) -> binary().
body_component_name(Module, TypeRef, MixedRefs) ->
    SchemaName = schema_component_name(Module, TypeRef),
    case lists:member({Module, TypeRef}, MixedRefs) of
        true -> nullable_component_name(SchemaName);
        false -> SchemaName
    end.

-spec nullable_component_name(binary()) -> binary().
nullable_component_name(SchemaName) ->
    <<SchemaName/binary, "Nullable">>.

-spec generate_parameter(parameter_spec(), spectra:sp_config()) -> openapi_parameter().
generate_parameter(
    #{
        name := Name,
        in := In,
        schema := Schema,
        module := Module
    } =
        ParameterSpec,
    Config
) when
    is_binary(Name)
->
    ModuleTypeInfo = spectra_module_types:get(Module, Config),
    NormalizedSchema = spectra_util:normalize_type_ref(ModuleTypeInfo, Schema),
    Required = maps:get(required, ParameterSpec, false),

    {Doc, OpenApiSchema} = lift_doc(ModuleTypeInfo, NormalizedSchema, parameter_config(Config)),
    Doc#{name => Name, in => In, required => Required, schema => OpenApiSchema}.

%% A parameter or header value is never JSON null; `required: false` expresses
%% that it can be left out.
-spec parameter_config(spectra:sp_config()) -> spectra:sp_config().
parameter_config(Config) ->
    Config#sp_config{missing_value_as_null = false}.

%% Moves description and deprecated from the type's inline schema up to the
%% parameter or header, so the OpenAPI output does not state them twice.
-spec lift_doc(spectra:type_info(), spectra:sp_type_or_ref(), spectra:sp_config()) ->
    {#{description => binary(), deprecated => boolean()}, spectra_json_schema:json_schema()}.
lift_doc(ModuleTypeInfo, NormalizedSchema, Config) ->
    InlineSchema = to_inline_schema(ModuleTypeInfo, NormalizedSchema, Config),
    Doc = maps:with([description, deprecated], type_doc(ModuleTypeInfo, NormalizedSchema, Config)),
    OpenApiSchema = maps:without(['$schema' | maps:keys(Doc)], InlineSchema),
    {Doc, OpenApiSchema}.

%% Returns the refs used by bodies, by null webhook payloads and by parameters
%% apart: a component that only parameters use gets the parameter schema, and
%% one that only null payloads use gets the null schema.
-spec collect_schema_refs([endpoint_spec() | webhook_spec()], spectra:sp_config()) ->
    {
        [{module(), spectra:sp_type_reference()}],
        [{module(), spectra:sp_type_reference()}],
        [{module(), spectra:sp_type_reference()}]
    }.
collect_schema_refs(Endpoints, Config) ->
    lists:foldl(
        fun(Endpoint, {ValueAcc, NullAcc, ParameterAcc}) ->
            {ValueRefs, NullRefs, ParameterRefs} = collect_endpoint_schema_refs(Endpoint, Config),
            {
                lists:usort(ValueRefs ++ ValueAcc),
                lists:usort(NullRefs ++ NullAcc),
                lists:usort(ParameterRefs ++ ParameterAcc)
            }
        end,
        {[], [], []},
        Endpoints
    ).

-spec collect_endpoint_schema_refs(endpoint_spec() | webhook_spec(), spectra:sp_config()) ->
    {
        [{module(), spectra:sp_type_reference()}],
        [{module(), spectra:sp_type_reference()}],
        [{module(), spectra:sp_type_reference()}]
    }.
collect_endpoint_schema_refs(
    #{responses := Responses, parameters := Parameters} =
        Endpoint,
    Config
) ->
    ResponseRefs = collect_response_refs(Responses, Config),
    RequestBodyRefs =
        case maps:get(request_body, Endpoint, undefined) of
            undefined ->
                [];
            #{schema := Schema, module := Module} ->
                case filter_typeref(Schema, Module, Config) of
                    {true, ModuleTypeRef} ->
                        [ModuleTypeRef];
                    false ->
                        []
                end
        end,
    ParameterRefs = collect_parameter_refs(Parameters, Config),

    %% A list payload is written inline, so its item type gets no null copy.
    case null_payload(Endpoint, Config) andalso not list_body(Endpoint, Config) of
        true -> {ResponseRefs, RequestBodyRefs, ParameterRefs};
        false -> {ResponseRefs ++ RequestBodyRefs, [], ParameterRefs}
    end.

-spec list_body(endpoint_spec() | webhook_spec(), spectra:sp_config()) -> boolean().
list_body(#{request_body := #{schema := Schema, module := Module}}, Config) ->
    TypeInfo = spectra_module_types:get(Module, Config),
    is_record(spectra_util:normalize_type_ref(TypeInfo, Schema), sp_list);
list_body(_Spec, _Config) ->
    false.

-spec collect_response_refs(#{http_status_code() => response_spec()}, spectra:sp_config()) ->
    [{module(), spectra:sp_type_reference()}].
collect_response_refs(Responses, Config) ->
    maps:fold(
        fun(_StatusCode, ResponseSpec, Acc) ->
            case
                {
                    maps:get(schema, ResponseSpec, undefined),
                    maps:get(module, ResponseSpec, undefined)
                }
            of
                {undefined, _} ->
                    %% Header-only response, no schema reference
                    Acc;
                {_, undefined} ->
                    %% Schema without module should not happen
                    Acc;
                {Schema, Module} ->
                    case filter_typeref(Schema, Module, Config) of
                        {true, ModuleTypeRef} ->
                            [ModuleTypeRef | Acc];
                        false ->
                            Acc
                    end
            end
        end,
        [],
        Responses
    ).

-spec collect_parameter_refs([parameter_spec()], spectra:sp_config()) ->
    [{module(), spectra:sp_type_reference()}].
collect_parameter_refs(Parameters, Config) ->
    lists:filtermap(
        fun(#{schema := Schema, module := Module}) ->
            filter_typeref(Schema, Module, Config)
        end,
        Parameters
    ).

-spec filter_typeref(spectra:sp_type_or_ref(), module(), spectra:sp_config()) ->
    {true, {module(), spectra:sp_type_reference()}} | false.
filter_typeref(Schema, Module, Config) ->
    TypeInfo = spectra_module_types:get(Module, Config),
    NormalizedSchema = spectra_util:normalize_type_ref(TypeInfo, Schema),
    case NormalizedSchema of
        {type, _, _} = TypeRef ->
            {true, {Module, TypeRef}};
        {record, _} = TypeRef ->
            {true, {Module, TypeRef}};
        #sp_list{type = #sp_remote_type{mfargs = {ItemMod, ItemName, _}, arity = ItemArity}} ->
            {true, {ItemMod, {type, ItemName, ItemArity}}};
        #sp_remote_type{mfargs = {RemoteMod, RemoteName, _}, arity = RemoteArity} ->
            {true, {RemoteMod, {type, RemoteName, RemoteArity}}};
        _ ->
            false
    end.

%% NullOnlyRefs get the null schema under their plain name, MixedRefs get it
%% under the Nullable name next to their plain schema.
-spec generate_components(
    [{module(), spectra:sp_type_reference()}],
    [{module(), spectra:sp_type_reference()}],
    [{module(), spectra:sp_type_reference()}],
    [{module(), spectra:sp_type_reference()}],
    spectra:sp_config()
) ->
    #{schemas => #{binary() => openapi_schema()}}.
generate_components(ValueRefs, NullOnlyRefs, MixedRefs, ParameterOnlyRefs, Config) ->
    ToSchema = fun({Module, TypeRef}, RefConfig) ->
        ModuleTypeInfo = spectra_module_types:get(Module, RefConfig),
        maps:remove('$schema', to_inline_schema(ModuleTypeInfo, TypeRef, RefConfig))
    end,
    Plain = lists:foldl(
        fun({{Module, TypeRef} = Ref, RefConfig}, Acc) ->
            Acc#{schema_component_name(Module, TypeRef) => ToSchema(Ref, RefConfig)}
        end,
        #{},
        [{Ref, Config} || Ref <- ValueRefs] ++
            [{Ref, null_config(Config)} || Ref <- NullOnlyRefs] ++
            [{Ref, parameter_config(Config)} || Ref <- ParameterOnlyRefs]
    ),
    Schemas = lists:foldl(
        fun({Module, TypeRef} = Ref, Acc) ->
            SchemaName = nullable_component_name(schema_component_name(Module, TypeRef)),
            case maps:is_key(SchemaName, Acc) of
                true -> raise_badarg({webhook_nil_fields, {name_clash, SchemaName}});
                false -> Acc#{SchemaName => ToSchema(Ref, null_config(Config))}
            end
        end,
        Plain,
        MixedRefs
    ),
    case maps:size(Schemas) > 0 of
        true ->
            #{schemas => Schemas};
        false ->
            #{}
    end.

-spec type_ref_to_component_name(spectra:sp_type_reference()) -> binary().
type_ref_to_component_name({type, TypeName, Arity}) ->
    TypeStr = atom_to_list(TypeName),
    Words = string:split(TypeStr, "_", all),
    PascalCase = lists:map(fun capitalize_word/1, Words),
    ArityStr = integer_to_list(Arity),
    iolist_to_binary([PascalCase, ArityStr]);
type_ref_to_component_name({record, RecordName}) ->
    TypeStr = atom_to_list(RecordName),
    Words = string:split(TypeStr, "_", all),
    PascalCase = lists:map(fun capitalize_word/1, Words),
    iolist_to_binary(PascalCase).

%% Use the last module segment as the schema name when the type is the
%% idiomatic t/0, to avoid all Elixir structs colliding on "T0".
-spec schema_component_name(module(), spectra:sp_type_reference()) -> binary().
schema_component_name(Module, {type, t, 0}) ->
    ModuleStr = atom_to_list(Module),
    Parts = string:split(ModuleStr, ".", all),
    LastPart = lists:last(Parts),
    iolist_to_binary(capitalize_word(LastPart));
schema_component_name(_Module, TypeRef) ->
    type_ref_to_component_name(TypeRef).

-spec type_doc(spectra:type_info(), spectra:sp_type_or_ref(), spectra:sp_config()) ->
    spectra:type_doc().
type_doc(TypeInfo, {type, Name, Arity}, Config) ->
    type_doc(TypeInfo, spectra_type_info:get_type(TypeInfo, Name, Arity), Config);
type_doc(TypeInfo, {record, Name}, Config) ->
    type_doc(TypeInfo, spectra_type_info:get_record(TypeInfo, Name), Config);
type_doc(TypeInfo, #sp_user_type_ref{type_name = Name, arity = Arity} = Ref, Config) ->
    case spectra_type:get_meta(Ref) of
        #{doc := Doc} -> maps:remove(examples_function, Doc);
        _ -> type_doc(TypeInfo, spectra_type_info:get_type(TypeInfo, Name, Arity), Config)
    end;
type_doc(_TypeInfo, #sp_remote_type{mfargs = {Mod, Name, _}, arity = Arity} = Ref, Config) ->
    case spectra_type:get_meta(Ref) of
        #{doc := Doc} ->
            maps:remove(examples_function, Doc);
        _ ->
            RemoteTypeInfo = spectra_module_types:get(Mod, Config),
            type_doc(
                RemoteTypeInfo, spectra_type_info:get_type(RemoteTypeInfo, Name, Arity), Config
            )
    end;
type_doc(_TypeInfo, Type, _Config) ->
    case spectra_type:get_meta(Type) of
        #{doc := Doc} -> maps:remove(examples_function, Doc);
        _ -> #{}
    end.

capitalize_word([]) ->
    [];
capitalize_word([First | Rest]) ->
    [string:to_upper(First) | Rest].

-spec to_inline_schema(spectra:type_info(), spectra:sp_type_or_ref(), spectra:sp_config()) ->
    spectra_json_schema:json_schema().
to_inline_schema(TypeInfo, {type, Name, Arity}, Config) ->
    Type = spectra_type_info:get_type(TypeInfo, Name, Arity),
    TypeRef = {type, Name, Arity},
    Module = spectra_type_info:get_module(TypeInfo),
    case spectra_codec:try_codec_schema(json_schema, TypeInfo, Module, TypeRef, Type, Config) of
        continue -> spectra_json_schema:to_schema(TypeInfo, Type, Config);
        Schema -> Schema
    end;
to_inline_schema(TypeInfo, {record, RecordName}, Config) ->
    Record = spectra_type_info:get_record(TypeInfo, RecordName),
    TypeRef = {record, RecordName},
    Module = spectra_type_info:get_module(TypeInfo),
    case spectra_codec:try_codec_schema(json_schema, TypeInfo, Module, TypeRef, Record, Config) of
        continue -> spectra_json_schema:to_schema(TypeInfo, Record, Config);
        Schema -> Schema
    end;
to_inline_schema(TypeInfo, SpType, Config) ->
    spectra_json_schema:to_schema(TypeInfo, SpType, Config).

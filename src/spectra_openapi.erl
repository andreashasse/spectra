-module(spectra_openapi).

-include("../include/spectra_internal.hrl").

-export([
    endpoint/2, endpoint/3,
    with_request_body/3, with_request_body/4,
    with_parameter/3,
    endpoints_to_openapi/2, endpoints_to_openapi/3,
    response/2,
    response_with_body/3, response_with_body/4,
    response_with_header/4,
    add_response/2
]).

-ignore_xref([
    {spectra_openapi, endpoint, 2},
    {spectra_openapi, endpoint, 3},
    {spectra_openapi, with_request_body, 3},
    {spectra_openapi, with_request_body, 4},
    {spectra_openapi, with_parameter, 3},
    {spectra_openapi, endpoints_to_openapi, 2},
    {spectra_openapi, endpoints_to_openapi, 3},
    {spectra_openapi, response, 2},
    {spectra_openapi, response_with_body, 3},
    {spectra_openapi, response_with_body, 4},
    {spectra_openapi, response_with_header, 4},
    {spectra_openapi, add_response, 2}
]).

-export_type([
    endpoint_spec/0,
    endpoint_doc/0,
    response_spec/0,
    parameter_spec/0,
    parameter_input_spec/0,
    http_method/0,
    http_status_code/0,
    openapi_metadata/0,
    openapi_spec/0
]).

-define(DEFAULT_CONTENT_TYPE, <<"application/json">>).

-type http_method() :: get | put | post | delete | options | head | patch | trace.
-type http_status_code() :: 100..599.
-type parameter_location() :: path | query | header | cookie.
-type openapi_schema() :: json:encode_value() | #{'$ref' := binary()}.
-type endpoint_doc() ::
    #{
        summary => binary(),
        description => binary(),
        operationId => binary(),
        tags => [binary()],
        deprecated => boolean(),
        externalDocs => #{description => binary(), url := binary()}
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
-type openapi_metadata() ::
    #{
        title := binary(),
        version := binary(),
        summary => binary(),
        description => binary(),
        terms_of_service => binary(),
        contact => openapi_contact(),
        license => openapi_license(),
        servers => [openapi_server()]
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
-type path_operations() :: #{http_method() => openapi_operation()}.
-type openapi_operation() ::
    #{
        summary => binary(),
        description => binary(),
        operationId => binary(),
        tags => [binary()],
        deprecated => boolean(),
        externalDocs => #{description => binary(), url := binary()},
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
        servers => [openapi_server()],
        components => #{schemas => #{binary() => openapi_schema()}}
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
            "StatusCode" => "HTTP status code (e.g., 200, 404, 500)"
        }
}.

-spec response(StatusCode :: http_status_code(), Description :: binary()) ->
    response_spec().
response(StatusCode, Description) when
    is_integer(StatusCode) andalso is_binary(Description)
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
            "Endpoint" => "Endpoint map to add the response to",
            "Response" => "Response specification built with response/2 and related functions"
        }
}.

-spec add_response(Endpoint :: endpoint_spec(), Response :: response_spec()) ->
    endpoint_spec().
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
            "Endpoint" => "Endpoint map to add the request body to",
            "Module" => "Module containing the type definition",
            "Schema" => "Schema reference or direct type (spectra:sp_type_or_ref())"
        }
}.

-spec with_request_body(
    Endpoint :: endpoint_spec(),
    Module :: module(),
    Schema :: spectra:sp_type_or_ref()
) ->
    endpoint_spec().
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
            "Endpoint" => "Endpoint map to add the request body to",
            "Module" => "Module containing the type definition",
            "Schema" => "Schema reference or direct type (spectra:sp_type_or_ref())"
        }
}.

-spec with_request_body(
    Endpoint :: endpoint_spec(),
    Module :: module(),
    Schema :: spectra:sp_type_or_ref(),
    ContentType :: binary()
) ->
    endpoint_spec().
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
            "Endpoint" => "Endpoint map to add the parameter to",
            "Module" => "Module containing the type definition",
            "ParameterSpec" => "Parameter specification map"
        }
}.

-spec with_parameter(
    Endpoint :: endpoint_spec(),
    Module :: module(),
    ParameterSpec :: parameter_input_spec()
) ->
    endpoint_spec().
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
    Parameters = maps:get(parameters, Endpoint, []),
    ParameterWithModule = ParameterSpec#{module => Module},
    Endpoint#{parameters => [ParameterWithModule | Parameters]}.

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
    Options :: [spectra:encode_option()]
) ->
    {ok, json:encode_value() | iodata()} | {error, [spectra:error()]}.
endpoints_to_openapi(MetaData, Endpoints, Options) when is_list(Endpoints) ->
    Config = spectra:get_config(),
    try
        PathGroups = group_endpoints_by_path(Endpoints),
        Paths =
            maps:fold(
                fun(Path, PathEndpoints, Acc) ->
                    PathOps = generate_path_operations(PathEndpoints, Config),
                    Acc#{Path => PathOps}
                end,
                #{},
                PathGroups
            ),

        SchemaRefs = collect_schema_refs(Endpoints, Config),
        ComponentsResult = generate_components(SchemaRefs, Config),
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
        OpenAPISpec = copy_if_present(servers, MetaData, BaseSpec),
        spectra:encode(json, ?MODULE, {type, openapi_spec, 0}, OpenAPISpec, Options)
    after
        spectra_module_types:clear_local()
    end.

-spec group_endpoints_by_path([endpoint_spec()]) -> #{binary() => [endpoint_spec()]}.
group_endpoints_by_path(Endpoints) ->
    lists:foldl(
        fun(Endpoint, Acc) ->
            Path = maps:get(path, Endpoint),
            PathEndpoints = maps:get(Path, Acc, []),
            Acc#{Path => [Endpoint | PathEndpoints]}
        end,
        #{},
        Endpoints
    ).

-spec generate_path_operations([endpoint_spec()], spectra:sp_config()) -> path_operations().
generate_path_operations(Endpoints, Config) ->
    lists:foldl(
        fun(#{method := Method} = Endpoint, Acc) ->
            Operation = generate_operation(Endpoint, Config),
            Acc#{Method => Operation}
        end,
        #{},
        Endpoints
    ).

-spec generate_operation(endpoint_spec(), spectra:sp_config()) -> openapi_operation().
generate_operation(Endpoint, Config) ->
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
                        fun(K, V, NewAcc) ->
                            BinaryKey = integer_to_binary(K),
                            NewAcc#{BinaryKey => V}
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
                RequestBody = generate_request_body(RequestBodyRef, Config),
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

-spec generate_response_header(response_header_spec(), spectra:sp_config()) -> openapi_header().
generate_response_header(#{schema := Schema, module := Module} = HeaderSpec, Config) ->
    ModuleTypeInfo = spectra_module_types:get(Module, Config),
    NormalizedSchema = spectra_util:normalize_type_ref(ModuleTypeInfo, Schema),
    InlineSchema = to_inline_schema(ModuleTypeInfo, NormalizedSchema, Config),
    OpenApiSchema = maps:remove('$schema', InlineSchema),
    Doc = maps:with([description, deprecated], type_doc(ModuleTypeInfo, NormalizedSchema, Config)),
    Base = Doc#{schema => OpenApiSchema},
    copy_if_present(required, HeaderSpec, Base).

-spec generate_request_body(request_body_spec(), spectra:sp_config()) -> openapi_request_body().
generate_request_body(#{schema := Schema, module := Module} = RequestBodySpec, Config) ->
    ModuleTypeInfo = spectra_module_types:get(Module, Config),
    NormalizedSchema = spectra_util:normalize_type_ref(ModuleTypeInfo, Schema),
    SchemaContent =
        case NormalizedSchema of
            {type, Name, Arity} ->
                SchemaName = schema_component_name(Module, {type, Name, Arity}),
                #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>};
            {record, Name} ->
                SchemaName = schema_component_name(Module, {record, Name}),
                #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>};
            #sp_remote_type{mfargs = {RemoteMod, RemoteName, _}, arity = RemoteArity} ->
                SchemaName = schema_component_name(RemoteMod, {type, RemoteName, RemoteArity}),
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

    InlineSchema = to_inline_schema(ModuleTypeInfo, NormalizedSchema, Config),
    OpenApiSchema = maps:remove('$schema', InlineSchema),
    Doc = maps:with([description, deprecated], type_doc(ModuleTypeInfo, NormalizedSchema, Config)),
    Doc#{name => Name, in => In, required => Required, schema => OpenApiSchema}.

-spec collect_schema_refs([endpoint_spec()], spectra:sp_config()) ->
    [{module(), spectra:sp_type_reference()}].
collect_schema_refs(Endpoints, Config) ->
    lists:foldl(
        fun(Endpoint, Acc) ->
            EndpointRefs = collect_endpoint_schema_refs(Endpoint, Config),
            lists:usort(EndpointRefs ++ Acc)
        end,
        [],
        Endpoints
    ).

-spec collect_endpoint_schema_refs(endpoint_spec(), spectra:sp_config()) ->
    [{module(), spectra:sp_type_reference()}].
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

    ResponseRefs ++ RequestBodyRefs ++ ParameterRefs.

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

-spec generate_components([{module(), spectra:sp_type_reference()}], spectra:sp_config()) ->
    #{schemas => #{binary() => openapi_schema()}}.
generate_components(SchemaRefs, Config) ->
    Schemas = lists:foldl(
        fun({Module, TypeRef}, Acc) ->
            ModuleTypeInfo = spectra_module_types:get(
                Module, Config
            ),
            Schema = to_inline_schema(ModuleTypeInfo, TypeRef, Config),
            SchemaName = schema_component_name(Module, TypeRef),
            OpenApiSchema = maps:remove('$schema', Schema),
            Acc#{SchemaName => OpenApiSchema}
        end,
        #{},
        SchemaRefs
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
    case spectra_codec:try_codec_schema(TypeInfo, json_schema, Type, Type, Config) of
        continue -> spectra_json_schema:to_schema(TypeInfo, Type, Config);
        Schema -> Schema
    end;
to_inline_schema(TypeInfo, {record, RecordName}, Config) ->
    Record = spectra_type_info:get_record(TypeInfo, RecordName),
    case spectra_codec:try_codec_schema(TypeInfo, json_schema, Record, Record, Config) of
        continue -> spectra_json_schema:to_schema(TypeInfo, Record, Config);
        Schema -> Schema
    end;
to_inline_schema(TypeInfo, SpType, Config) ->
    spectra_json_schema:to_schema(TypeInfo, SpType, Config).

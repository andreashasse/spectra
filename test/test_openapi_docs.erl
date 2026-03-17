-module(test_openapi_docs).

-include_lib("eunit/include/eunit.hrl").
-include("../include/spectra_internal.hrl").

-compile([nowarn_unused_type]).

-export([user_examples/0]).

-record(user, {
    id :: pos_integer(),
    name :: binary()
}).

-spectra(#{
    title => <<"User">>,
    description => <<"A user in the system">>,
    examples_function => {?MODULE, user_examples, []}
}).
-type user_type() :: #user{}.

-spectra(#{description => <<"A user's unique identifier">>}).
-type user_id() :: pos_integer().

-record(create_user_request, {name :: binary()}).
-spectra(#{description => <<"Request body for creating a user">>}).
-type create_user_request() :: #create_user_request{}.

-type undocumented_id() :: pos_integer().

%% Alias with no annotation — description should be inherited from user_id
-type user_id_alias() :: user_id().

-spectra(#{description => <<"A documented alias">>}).
-type documented_alias() :: user_id().

user_examples() ->
    [
        #user{id = 1, name = <<"Alice">>},
        #user{id = 2, name = <<"Bob">>}
    ].

%% Navigates nested maps by applying maps:get for each key in sequence.
map_gets(Keys, Map) ->
    lists:foldl(fun maps:get/2, Map, Keys).

%% Builds a single-parameter GET endpoint, generates the spec, and returns the
%% rendered parameter map for the given schema.
path_param_for_schema(Schema) ->
    Param = #{name => <<"id">>, in => path, required => true, schema => Schema},
    Endpoint1 = spectra_openapi:endpoint(get, <<"/users/{id}">>),
    Endpoint2 = spectra_openapi:with_parameter(Endpoint1, ?MODULE, Param),
    Endpoint = spectra_openapi:add_response(Endpoint2, spectra_openapi:response(200, <<"OK">>)),
    Metadata = #{title => <<"API">>, version => <<"1.0">>},
    {ok, Spec} = spectra_openapi:endpoints_to_openapi(Metadata, [Endpoint]),
    [RenderedParam] = map_gets([<<"paths">>, <<"/users/{id}">>, <<"get">>, <<"parameters">>], Spec),
    RenderedParam.

openapi_parameter_description_from_type_test() ->
    Param = path_param_for_schema({type, user_id, 0}),
    ?assertMatch(#{<<"description">> := <<"A user's unique identifier">>}, Param).

openapi_request_body_description_from_type_test() ->
    Endpoint1 = spectra_openapi:endpoint(post, <<"/users">>),
    Endpoint2 = spectra_openapi:with_request_body(
        Endpoint1, ?MODULE, {type, create_user_request, 0}
    ),
    Endpoint = spectra_openapi:add_response(
        Endpoint2, spectra_openapi:response(201, <<"Created">>)
    ),
    Metadata = #{title => <<"API">>, version => <<"1.0">>},
    {ok, Spec} = spectra_openapi:endpoints_to_openapi(Metadata, [Endpoint]),
    ?assertMatch(
        #{
            <<"paths">> := #{
                <<"/users">> := #{
                    <<"post">> := #{
                        <<"requestBody">> := #{
                            <<"description">> := <<"Request body for creating a user">>
                        }
                    }
                }
            }
        },
        Spec
    ).

openapi_no_description_when_type_has_none_test() ->
    Param = path_param_for_schema({type, undocumented_id, 0}),
    ?assertNot(maps:is_key(<<"description">>, Param)).

%% When a type alias has no -spectra annotation, type_doc follows the
%% sp_user_type_ref to the referenced type and returns its description.
openapi_parameter_description_follows_user_type_ref_test() ->
    Param = path_param_for_schema({type, user_id_alias, 0}),
    ?assertMatch(#{<<"description">> := <<"A user's unique identifier">>}, Param).

%% When a type alias has its own -spectra annotation, type_doc uses the local
%% annotation rather than following the reference.
openapi_parameter_description_uses_local_alias_doc_test() ->
    Param = path_param_for_schema({type, documented_alias, 0}),
    ?assertMatch(#{<<"description">> := <<"A documented alias">>}, Param).

%% When the schema is an sp_remote_type{} with no local meta, type_doc follows
%% the remote reference and returns the description from the remote module.
openapi_parameter_description_follows_remote_type_test() ->
    Param = path_param_for_schema(#sp_remote_type{mfargs = {?MODULE, user_id, []}}),
    ?assertMatch(#{<<"description">> := <<"A user's unique identifier">>}, Param).

openapi_includes_documentation_test() ->
    %% Create a simple endpoint with a user response
    Response = spectra_openapi:response(200, <<"User found">>),
    ResponseWithBody = spectra_openapi:response_with_body(Response, ?MODULE, {type, user_type, 0}),

    Endpoint1 = spectra_openapi:endpoint(get, <<"/users/{id}">>),
    Endpoint = spectra_openapi:add_response(Endpoint1, ResponseWithBody),

    %% Generate OpenAPI spec
    Metadata = #{
        title => <<"Test API">>,
        version => <<"1.0.0">>
    },
    {ok, OpenAPISpec} = spectra_openapi:endpoints_to_openapi(Metadata, [Endpoint]),

    %% Verify that components/schemas contains the user schema with complete documentation
    ?assertMatch(
        #{
            <<"components">> := #{
                <<"schemas">> := #{
                    <<"UserType0">> := #{
                        title := <<"User">>,
                        description := <<"A user in the system">>,
                        examples := [
                            #{<<"id">> := 1, <<"name">> := <<"Alice">>},
                            #{<<"id">> := 2, <<"name">> := <<"Bob">>}
                        ],
                        type := <<"object">>,
                        properties := #{
                            <<"id">> := #{type := <<"integer">>, minimum := 1},
                            <<"name">> := #{type := <<"string">>}
                        },
                        required := [<<"id">>, <<"name">>]
                    }
                }
            }
        },
        OpenAPISpec
    ).

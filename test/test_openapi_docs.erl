-module(test_openapi_docs).

-include_lib("eunit/include/eunit.hrl").

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

user_examples() ->
    [
        #user{id = 1, name = <<"Alice">>},
        #user{id = 2, name = <<"Bob">>}
    ].

openapi_parameter_description_from_type_test() ->
    Param = #{name => <<"id">>, in => path, required => true, schema => {type, user_id, 0}},
    Endpoint1 = spectra_openapi:endpoint(get, <<"/users/{id}">>),
    Endpoint2 = spectra_openapi:with_parameter(Endpoint1, ?MODULE, Param),
    Endpoint = spectra_openapi:add_response(Endpoint2, spectra_openapi:response(200, <<"OK">>)),
    Metadata = #{title => <<"API">>, version => <<"1.0">>},
    {ok, Spec} = spectra_openapi:endpoints_to_openapi(Metadata, [Endpoint]),
    ?assertMatch(
        #{
            <<"paths">> := #{
                <<"/users/{id}">> := #{
                    <<"get">> := #{
                        <<"parameters">> := [
                            #{
                                <<"name">> := <<"id">>,
                                <<"description">> := <<"A user's unique identifier">>
                            }
                        ]
                    }
                }
            }
        },
        Spec
    ).

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
    %% undocumented_id type has no spectra doc, so no description should appear
    Param = #{name => <<"id">>, in => path, required => true, schema => {type, undocumented_id, 0}},
    Endpoint1 = spectra_openapi:endpoint(get, <<"/users/{id}">>),
    Endpoint2 = spectra_openapi:with_parameter(Endpoint1, ?MODULE, Param),
    Endpoint = spectra_openapi:add_response(Endpoint2, spectra_openapi:response(200, <<"OK">>)),
    Metadata = #{title => <<"API">>, version => <<"1.0">>},
    {ok, Spec} = spectra_openapi:endpoints_to_openapi(Metadata, [Endpoint]),
    [Parameter] = maps:get(
        <<"parameters">>,
        maps:get(<<"get">>, maps:get(<<"/users/{id}">>, maps:get(<<"paths">>, Spec)))
    ),
    ?assertNot(maps:is_key(<<"description">>, Parameter)).

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

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

user_examples() ->
    [
        #user{id = 1, name = <<"Alice">>},
        #user{id = 2, name = <<"Bob">>}
    ].

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

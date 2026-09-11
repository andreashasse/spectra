-module(spectra_json_schema_inline_doc_test).

%% Doc annotations (title, description, deprecated, examples) must travel with
%% a type wherever it is inlined — map fields, union branches, list elements,
%% optional map values and remote types — not only when schema generation is
%% entered with that type.

-include_lib("eunit/include/eunit.hrl").

-compile([nowarn_unused_type]).

-spectra(#{
    title => <<"Payer">>,
    description => <<"The party paying for the session">>,
    deprecated => true,
    examples => [<<"alice">>, <<"bob">>]
}).
-type payer() :: binary().

-spectra(#{
    title => <<"Legacy Name">>,
    description => <<"Superseded by payer">>,
    deprecated => true,
    examples => [<<"old-style">>]
}).
-type deprecated_string() :: binary().

-type object() :: #{id := integer()}.

%% A type carrying both a doc annotation and type_parameters. Both have to end
%% up on the same inlined schema.
-spectra(#{
    title => <<"URL">>,
    description => <<"An absolute https URL">>,
    type_parameters => #{max_length => 2048, pattern => <<"^https://">>}
}).
-type url() :: binary().

%% An alias of a documented type, carrying a conflicting title of its own.
-spectra(#{title => <<"Session Payer">>}).
-type session_payer() :: payer().

-type request() :: #{payer := payer()}.

-type object_or_legacy() :: object() | deprecated_string().

-type payer_list() :: [payer()].

-type nonempty_payer_list() :: [payer(), ...].

-type optional_payer() :: #{payer => payer()}.

-type remote_holder() :: #{tag := inline_doc_remote_helper:tag()}.

-type url_holder() :: #{success_url := url()}.

-type nested_payer() :: #{inner := request()}.

-type aliased_payer_holder() :: #{payer := session_payer()}.

schema(TypeName) ->
    SchemaJson = spectra:schema(json_schema, ?MODULE, {type, TypeName, 0}),
    #{} = Schema = json:decode(iolist_to_binary(SchemaJson)),
    json_schema_validator_helper:validate_or_skip(Schema),
    Schema.

payer_doc() ->
    #{
        <<"type">> => <<"string">>,
        <<"title">> => <<"Payer">>,
        <<"description">> => <<"The party paying for the session">>,
        <<"deprecated">> => true,
        <<"examples">> => [<<"alice">>, <<"bob">>]
    }.

map_field_keeps_doc_test() ->
    ?assertMatch(
        #{
            <<"properties">> := #{
                <<"payer">> := #{
                    <<"type">> := <<"string">>,
                    <<"title">> := <<"Payer">>,
                    <<"description">> := <<"The party paying for the session">>,
                    <<"deprecated">> := true,
                    <<"examples">> := [<<"alice">>, <<"bob">>]
                }
            }
        },
        schema(request)
    ).

nested_map_field_keeps_doc_test() ->
    #{<<"properties">> := #{<<"inner">> := Inner}} = schema(nested_payer),
    ?assertEqual(#{<<"payer">> => payer_doc()}, maps:get(<<"properties">>, Inner)).

union_branch_keeps_doc_test() ->
    ?assertMatch(
        #{
            <<"anyOf">> := [
                #{<<"type">> := <<"object">>},
                #{
                    <<"type">> := <<"string">>,
                    <<"title">> := <<"Legacy Name">>,
                    <<"description">> := <<"Superseded by payer">>,
                    <<"deprecated">> := true,
                    <<"examples">> := [<<"old-style">>]
                }
            ]
        },
        schema(object_or_legacy)
    ).

list_element_keeps_doc_test() ->
    ?assertMatch(
        #{
            <<"type">> := <<"array">>,
            <<"items">> := #{
                <<"title">> := <<"Payer">>,
                <<"description">> := <<"The party paying for the session">>,
                <<"deprecated">> := true,
                <<"examples">> := [<<"alice">>, <<"bob">>]
            }
        },
        schema(payer_list)
    ).

nonempty_list_element_keeps_doc_test() ->
    ?assertMatch(
        #{
            <<"type">> := <<"array">>,
            <<"minItems">> := 1,
            <<"items">> := #{
                <<"title">> := <<"Payer">>,
                <<"deprecated">> := true
            }
        },
        schema(nonempty_payer_list)
    ).

optional_map_value_keeps_doc_test() ->
    Schema = schema(optional_payer),
    ?assertEqual(#{<<"payer">> => payer_doc()}, maps:get(<<"properties">>, Schema)),
    %% An optional key is never required, doc or no doc.
    ?assertEqual(error, maps:find(<<"required">>, Schema)).

remote_type_keeps_doc_test() ->
    ?assertMatch(
        #{
            <<"properties">> := #{
                <<"tag">> := #{
                    <<"type">> := <<"string">>,
                    <<"title">> := <<"Remote Tag">>,
                    <<"description">> := <<"A tag defined in another module">>,
                    <<"deprecated">> := true,
                    <<"examples">> := [<<"remote">>]
                }
            }
        },
        schema(remote_holder)
    ).

%% Regression: type_parameters and doc annotations on the same type must both
%% reach the inlined schema.
type_parameters_and_doc_merge_test() ->
    ?assertMatch(
        #{
            <<"properties">> := #{
                <<"success_url">> := #{
                    <<"type">> := <<"string">>,
                    <<"title">> := <<"URL">>,
                    <<"description">> := <<"An absolute https URL">>,
                    <<"maxLength">> := 2048,
                    <<"pattern">> := <<"^https://">>
                }
            }
        },
        schema(url_holder)
    ).

%% Precedence: annotations merge along the resolution chain and the one written
%% nearest the use site wins on conflicting keys. This matches how a type alias
%% of a documented record already behaved (see spectra_json_schema_doc_test).
alias_doc_wins_over_inlined_doc_test() ->
    ?assertMatch(
        #{
            <<"properties">> := #{
                <<"payer">> := #{
                    <<"title">> := <<"Session Payer">>,
                    <<"description">> := <<"The party paying for the session">>,
                    <<"deprecated">> := true,
                    <<"examples">> := [<<"alice">>, <<"bob">>]
                }
            }
        },
        schema(aliased_payer_holder)
    ).

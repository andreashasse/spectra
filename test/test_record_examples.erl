-module(test_record_examples).

-include_lib("eunit/include/eunit.hrl").

-compile([nowarn_unused_type, nowarn_unused_record]).

%% This file demonstrates the traditional way of adding examples using tuple syntax.
%% For a more flexible approach using functions with record syntax, see test_examples_function.erl
%% which uses the examples_function field:
%%
%% -spectra(#{
%%     examples_function => {my_module, person_examples, []}
%% }).
%%
%% Where person_examples/0 returns:
%% [
%%     #person{name = <<"Alice">>, age = 30},
%%     #person{name = <<"Bob">>, age = 25}
%% ]

-record(person, {
    name :: binary(),
    age :: non_neg_integer()
}).

-spectra(#{
    title => <<"Person">>,
    description => <<"A person with name and age">>,
    examples => [
        {person, <<"Alice">>, 30},
        {person, <<"Bob">>, 25}
    ]
}).
-type person_type() :: #person{}.

-spectra(#{
    title => <<"Product">>,
    description => <<"A product with price">>,
    examples => [
        {product, <<"Widget">>, 999},
        {product, <<"Gadget">>, 1499}
    ]
}).
-record(product, {
    name :: binary(),
    price_cents :: pos_integer()
}).

record_with_examples_test() ->
    SchemaJson = spectra:schema(json_schema, ?MODULE, {record, person}),
    Schema = json:decode(iolist_to_binary(SchemaJson)),

    %% The record itself doesn't have docs with examples
    %% Only the type alias has the docs
    ?assertEqual(false, maps:is_key(<<"examples">>, Schema)),
    ok.

type_with_record_examples_test() ->
    %% Test that examples work for a type that references a record
    SchemaJson = spectra:schema(json_schema, ?MODULE, {type, person_type, 0}),
    Schema = json:decode(iolist_to_binary(SchemaJson)),

    ?assertMatch(
        #{
            <<"title">> := <<"Person">>,
            <<"description">> := <<"A person with name and age">>,
            <<"examples">> := [
                #{<<"name">> := <<"Alice">>, <<"age">> := 30},
                #{<<"name">> := <<"Bob">>, <<"age">> := 25}
            ]
        },
        Schema
    ),

    ok.

record_direct_examples_test() ->
    %% Test that examples work directly on a record definition
    SchemaJson = spectra:schema(json_schema, ?MODULE, {record, product}),
    Schema = json:decode(iolist_to_binary(SchemaJson)),

    ?assertMatch(
        #{
            <<"title">> := <<"Product">>,
            <<"description">> := <<"A product with price">>,
            <<"examples">> := [
                #{<<"name">> := <<"Widget">>, <<"price_cents">> := 999},
                #{<<"name">> := <<"Gadget">>, <<"price_cents">> := 1499}
            ]
        },
        Schema
    ),

    ok.

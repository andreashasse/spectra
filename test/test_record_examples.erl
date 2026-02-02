-module(test_record_examples).

-include_lib("eunit/include/eunit.hrl").

-compile([nowarn_unused_type, nowarn_unused_record]).

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

    ?assertEqual(<<"Person">>, maps:get(<<"title">>, Schema)),
    ?assertEqual(<<"A person with name and age">>, maps:get(<<"description">>, Schema)),

    %% Check that examples were converted correctly
    Examples = maps:get(<<"examples">>, Schema),
    ?assertEqual(2, length(Examples)),

    %% First example should be: {"name": "Alice", "age": 30}
    [Example1, Example2] = Examples,
    ?assertEqual(#{<<"name">> => <<"Alice">>, <<"age">> => 30}, Example1),
    ?assertEqual(#{<<"name">> => <<"Bob">>, <<"age">> => 25}, Example2),

    ok.

record_direct_examples_test() ->
    %% Test that examples work directly on a record definition
    SchemaJson = spectra:schema(json_schema, ?MODULE, {record, product}),
    Schema = json:decode(iolist_to_binary(SchemaJson)),

    ?assertEqual(<<"Product">>, maps:get(<<"title">>, Schema)),
    ?assertEqual(<<"A product with price">>, maps:get(<<"description">>, Schema)),

    %% Check that examples were converted correctly
    Examples = maps:get(<<"examples">>, Schema),
    ?assertEqual(2, length(Examples)),

    %% Examples should be converted from record tuples to JSON objects
    [Example1, Example2] = Examples,
    ?assertEqual(#{<<"name">> => <<"Widget">>, <<"price_cents">> => 999}, Example1),
    ?assertEqual(#{<<"name">> => <<"Gadget">>, <<"price_cents">> => 1499}, Example2),

    ok.

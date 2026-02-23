-module(test_examples_function).

-include_lib("eunit/include/eunit.hrl").

-compile([nowarn_unused_type, nowarn_unused_record]).

-export([person_examples/0, product_examples/0, sized_examples/1]).

-record(person, {
    name :: binary(),
    age :: non_neg_integer()
}).

-record(product, {
    name :: binary(),
    price_cents :: pos_integer()
}).

%% Test using examples_function with a simple function
-spectra(#{
    title => <<"Person">>,
    description => <<"A person with name and age">>,
    examples_function => {?MODULE, person_examples, []}
}).
-type person_type() :: #person{}.

%% Test using examples_function on a record
-spectra(#{
    title => <<"Product">>,
    description => <<"A product with price">>,
    examples_function => {?MODULE, product_examples, []}
}).
-record(product_with_doc, {
    name :: binary(),
    price_cents :: pos_integer()
}).

%% Test using examples_function with arguments
-spectra(#{
    title => <<"Sized Examples">>,
    description => <<"Generated examples">>,
    examples_function => {?MODULE, sized_examples, [2]}
}).
-type sized_type() :: #person{}.

%% Example function that returns person records
person_examples() ->
    [
        #person{name = <<"Alice">>, age = 30},
        #person{name = <<"Bob">>, age = 25}
    ].

%% Example function that returns product records
product_examples() ->
    [
        #product_with_doc{name = <<"Widget">>, price_cents = 999},
        #product_with_doc{name = <<"Gadget">>, price_cents = 1499}
    ].

%% Example function with arguments
sized_examples(Count) ->
    [
        #person{name = iolist_to_binary(["Person", integer_to_list(N)]), age = N * 10}
     || N <- lists:seq(1, Count)
    ].

type_with_examples_function_test() ->
    %% Test that examples_function works for a type
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

record_with_examples_function_test() ->
    %% Test that examples_function works on a record
    SchemaJson = spectra:schema(json_schema, ?MODULE, {record, product_with_doc}),
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

examples_function_with_args_test() ->
    %% Test that examples_function works with arguments
    SchemaJson = spectra:schema(json_schema, ?MODULE, {type, sized_type, 0}),
    Schema = json:decode(iolist_to_binary(SchemaJson)),

    ?assertMatch(
        #{
            <<"title">> := <<"Sized Examples">>,
            <<"examples">> := [
                #{<<"name">> := <<"Person1">>, <<"age">> := 10},
                #{<<"name">> := <<"Person2">>, <<"age">> := 20}
            ]
        },
        Schema
    ),

    ok.

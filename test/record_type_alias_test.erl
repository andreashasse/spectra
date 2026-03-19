-module(record_type_alias_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-compile(nowarn_unused_type).

-record(person, {name :: string(), age :: integer()}).
-record(address, {street :: string(), city :: string()}).

json_encode(TypeRef, Data) ->
    spectra:encode(json, ?MODULE, TypeRef, Data, [pre_encoded]).

json_decode(TypeRef, Data) ->
    spectra:decode(json, ?MODULE, TypeRef, Data, [pre_decoded]).

-type person_alias() :: #person{name :: string(), age :: integer()}.
-type address_alias() :: #address{street :: string(), city :: string()}.
-type person_new_age() :: #person{age :: non_neg_integer()}.
-type person_t() :: #person{}.

type_in_form_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    PersonAliasType = spectra_type_info:get_type(TypeInfo, person_alias, 0),
    ?assertMatch(
        #sp_rec_ref{
            record_name = person,
            field_types =
                [
                    {name, #sp_simple_type{type = string}},
                    {age, #sp_simple_type{type = integer}}
                ]
        },
        PersonAliasType
    ),

    AddressAliasType = spectra_type_info:get_type(TypeInfo, address_alias, 0),
    ?assertMatch(
        #sp_rec_ref{
            record_name = address,
            field_types =
                [
                    {street, #sp_simple_type{type = string}},
                    {city, #sp_simple_type{type = string}}
                ]
        },
        AddressAliasType
    ),

    PersonNewAgeType = spectra_type_info:get_type(TypeInfo, person_new_age, 0),
    ?assertMatch(
        #sp_rec_ref{
            record_name = person,
            field_types = [{age, #sp_simple_type{type = non_neg_integer}}]
        },
        PersonNewAgeType
    ),

    PersonTType = spectra_type_info:get_type(TypeInfo, person_t, 0),
    ?assertMatch(#sp_rec_ref{record_name = person, field_types = []}, PersonTType).

to_json_person_record_test() ->
    Person = #person{name = "John", age = 30},
    ?assertEqual(
        {ok, #{<<"name">> => <<"John">>, <<"age">> => 30}}, json_encode({record, person}, Person)
    ).

to_json_person_record_bad_test() ->
    NotPersonArity = {person, "John"},
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]}, json_encode({record, person}, NotPersonArity)
    ).

from_json_person_record_test() ->
    Person = #{<<"name">> => <<"John">>, <<"age">> => 30},
    ?assertEqual({ok, #person{name = "John", age = 30}}, json_decode({record, person}, Person)).

to_json_person_alias_test() ->
    Person = #person{name = "John", age = -1},
    ?assertEqual(
        {ok, #{<<"name">> => <<"John">>, <<"age">> => -1}}, json_encode(person_alias, Person)
    ).

to_json_person_alias_bad_test() ->
    Person = #person{name = "John", age = "not_an_integer"},
    ?assertEqual(
        {error, [
            sp_error:append_location(
                sp_error:type_mismatch(#sp_simple_type{type = integer}, "not_an_integer"),
                age
            )
        ]},
        json_encode(person_alias, Person)
    ).

to_json_person_new_age_test() ->
    Person = #person{name = "John", age = 0},
    ?assertEqual(
        {ok, #{<<"name">> => <<"John">>, <<"age">> => 0}}, json_encode(person_new_age, Person)
    ).

to_json_person_new_age_bad_test() ->
    Person = #person{name = "John", age = -1},
    ?assertEqual(
        {error, [
            sp_error:append_location(
                sp_error:type_mismatch(#sp_simple_type{type = non_neg_integer}, -1),
                age
            )
        ]},
        json_encode(person_new_age, Person)
    ).

to_json_person_t_test() ->
    Person = #person{name = "John", age = 0},
    ?assertEqual({ok, #{<<"name">> => <<"John">>, <<"age">> => 0}}, json_encode(person_t, Person)).

from_json_person_alias_test() ->
    Json = #{<<"name">> => <<"John">>, <<"age">> => 30},
    ?assertEqual({ok, #person{name = "John", age = 30}}, json_decode(person_alias, Json)).

from_json_person_alias_bad_test() ->
    Json = #{<<"name">> => <<"John">>, <<"age">> => <<"not_an_integer">>},
    ?assertEqual(
        {error, [
            sp_error:append_location(
                sp_error:type_mismatch(#sp_simple_type{type = integer}, <<"not_an_integer">>),
                age
            )
        ]},
        json_decode(person_alias, Json)
    ).

from_json_person_new_age_test() ->
    Json = #{<<"name">> => <<"John">>, <<"age">> => 30},
    ?assertEqual({ok, #person{name = "John", age = 30}}, json_decode(person_new_age, Json)).

from_json_person_new_age_bad_test() ->
    Json = #{<<"name">> => <<"John">>, <<"age">> => -1},
    ?assertEqual(
        {error, [
            sp_error:append_location(
                sp_error:type_mismatch(#sp_simple_type{type = non_neg_integer}, -1),
                age
            )
        ]},
        json_decode(person_new_age, Json)
    ).

from_json_person_t_test() ->
    Json = #{<<"name">> => <<"John">>, <<"age">> => 30},
    ?assertEqual({ok, #person{name = "John", age = 30}}, json_decode(person_t, Json)).

from_json_person_t_bad_test() ->
    Json = #{<<"name">> => <<"John">>, <<"age">> => <<"not_an_integer">>},
    ?assertEqual(
        {error, [
            sp_error:append_location(
                sp_error:type_mismatch(#sp_simple_type{type = integer}, <<"not_an_integer">>),
                age
            )
        ]},
        json_decode(person_t, Json)
    ).

to_json_address_alias_test() ->
    Address = #address{street = "Main St", city = "Boston"},
    ?assertEqual(
        {ok, #{<<"street">> => <<"Main St">>, <<"city">> => <<"Boston">>}},
        json_encode(address_alias, Address)
    ).

from_json_address_alias_test() ->
    Json = #{<<"street">> => <<"Main St">>, <<"city">> => <<"Boston">>},
    ?assertEqual(
        {ok, #address{street = "Main St", city = "Boston"}},
        json_decode(address_alias, Json)
    ).

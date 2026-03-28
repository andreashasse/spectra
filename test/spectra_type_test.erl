-module(spectra_type_test).

-include_lib("eunit/include/eunit.hrl").
-include("../include/spectra_internal.hrl").

update_meta_adds_key_test() ->
    Type = #sp_simple_type{type = integer, meta = #{}},
    Updated = spectra_type:update_meta(Type, #{name => {type, my_int, 0}}),
    ?assertEqual(#{name => {type, my_int, 0}}, spectra_type:get_meta(Updated)).

update_meta_preserves_existing_keys_test() ->
    Type = #sp_simple_type{type = integer, meta = #{name => {type, existing, 0}}},
    Updated = spectra_type:update_meta(Type, #{parameters => #{min_length => 1}}),
    ?assertEqual(
        #{name => {type, existing, 0}, parameters => #{min_length => 1}},
        spectra_type:get_meta(Updated)
    ).

update_meta_overwrites_existing_key_test() ->
    Type = #sp_simple_type{type = integer, meta = #{name => {type, old_name, 0}}},
    Updated = spectra_type:update_meta(Type, #{name => {type, new_name, 0}}),
    ?assertEqual(#{name => {type, new_name, 0}}, spectra_type:get_meta(Updated)).

update_meta_empty_updates_test() ->
    Meta = #{name => {type, my_type, 0}},
    Type = #sp_simple_type{type = integer, meta = Meta},
    Updated = spectra_type:update_meta(Type, #{}),
    ?assertEqual(Meta, spectra_type:get_meta(Updated)).

update_meta_preserves_type_test() ->
    Type = #sp_union{types = [], meta = #{name => {type, my_union, 0}}},
    Updated = spectra_type:update_meta(Type, #{parameters => foo}),
    ?assertMatch(#sp_union{}, Updated),
    ?assertEqual(
        #{name => {type, my_union, 0}, parameters => foo},
        spectra_type:get_meta(Updated)
    ).

-module(struct_shaped_map_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").

%% Regression test for the badmap crash in to_json/4's #sp_map{} clause,
%% reproduced entirely through plain Erlang, no Elixir involved: the type
%% here is defined in struct_shaped_map_types.erl (a plain .erl file), and
%% no Elixir module is loaded anywhere. spectra_abstract_code detects the
%% "struct" shape from the type's abstract syntax alone (a mandatory
%% '__struct__' field mapped to a literal atom), independent of source
%% language, so this same crash was reachable without Elixir.

encode_struct(Data) ->
    spectra:encode(json, struct_shaped_map_types, {type, t, 0}, Data, [pre_encoded]).

encode_list_of_structs(Data) ->
    spectra:encode(json, struct_shaped_map_types, {type, list_of_t, 0}, Data, [pre_encoded]).

struct_ok_test() ->
    ?assertEqual(
        {ok, #{<<"name">> => <<"Alice">>, <<"age">> => 30}},
        encode_struct(#{'__struct__' => erlang_only_struct, name => <<"Alice">>, age => 30})
    ).

%% Before the fix: erlang:error({badmap, BadData}) from maps:get('__struct__', ...).
%% After the fix: falls through to the existing catch-all type_mismatch clause,
%% same as every other type already does for data of the wrong shape.
struct_non_map_data_returns_error_test() ->
    lists:foreach(
        fun(BadData) ->
            ?assertMatch(
                {error, [#sp_error{type = type_mismatch}]},
                encode_struct(BadData)
            )
        end,
        [<<"a string">>, 42, [1, 2, 3], an_atom]
    ).

list_of_structs_ok_test() ->
    ?assertEqual(
        {ok, [#{<<"name">> => <<"Alice">>, <<"age">> => 30}]},
        encode_list_of_structs([
            #{'__struct__' => erlang_only_struct, name => <<"Alice">>, age => 30}
        ])
    ).

%% Same bug, one level deeper: list_to_json/4 recurses into to_json/4 per
%% element, so a list of "structs" with a non-map element (a sublist here)
%% hit the identical unguarded clause and crashed before the fix.
list_of_structs_with_non_map_element_returns_error_test() ->
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch, location = [1]}]},
        encode_list_of_structs([[1, 2, 3]])
    ).

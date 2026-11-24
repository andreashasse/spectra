-module(reserved_struct_key_test).

-include_lib("eunit/include/eunit.hrl").

%% An untrusted JSON document must never produce a map with an Elixir
%% `__struct__` key, since that would let the sender forge a struct.

decode(TypeName, Json) ->
    TypeInfo = spectra_abstract_code:types_in_module(reserved_struct_key_types),
    Type = spectra_type_info:get_type(TypeInfo, TypeName, 0),
    spectra:decode(json, TypeInfo, Type, Json).

struct_key_rejected_with_atom_to_term_test() ->
    ?assertMatch(
        {error, [_]},
        decode(atom_to_term, <<"{\"__struct__\":\"Elixir.Foo\",\"a\":1}">>)
    ).

struct_key_rejected_with_atom_to_atom_test() ->
    ?assertMatch(
        {error, [_]},
        decode(atom_to_atom, <<"{\"__struct__\":\"erlang\"}">>)
    ).

struct_key_rejected_alongside_literal_fields_test() ->
    ?assertMatch(
        {error, [_]},
        decode(atom_key_with_literal, <<"{\"name\":\"x\",\"__struct__\":\"Elixir.Foo\"}">>)
    ).

exception_key_rejected_test() ->
    ?assertMatch(
        {error, [_]},
        decode(atom_to_term, <<"{\"__exception__\":true}">>)
    ).

meta_key_rejected_test() ->
    ?assertMatch(
        {error, [_]},
        decode(atom_to_term, <<"{\"__meta__\":{}}">>)
    ).

other_atom_keys_still_decode_test() ->
    ?assertEqual({ok, #{a => 1}}, decode(atom_to_term, <<"{\"a\":1}">>)).

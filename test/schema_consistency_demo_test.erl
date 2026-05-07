-module(schema_consistency_demo_test).

-include_lib("eunit/include/eunit.hrl").

-compile([nowarn_unused_type]).

%% This test demonstrates that spectra:schema/3 now returns consistent results
%% when called with an atom (type name) versus a 3-tuple reference {type, Name, Arity}.
%%
%% Previously, calling with an atom would lose the -spectra() attribute metadata
%% (title, description, examples), while the 3-tuple would include it.
%%
%% After the fix, both methods return identical schemas with full metadata.

-spectra(#{
    title => <<"User Age">>,
    description => <<"Age of a user in years">>,
    examples => [18, 25, 42, 65]
}).
-type user_age() :: non_neg_integer().

atom_vs_tuple_consistency_test() ->
    % Call schema/3 with an atom (type name)
    SchemaFromAtom = spectra:schema(json_schema, ?MODULE, user_age),
    DecodedFromAtom = json:decode(iolist_to_binary(SchemaFromAtom)),

    % Call schema/3 with a 3-tuple reference
    SchemaFromTuple = spectra:schema(json_schema, ?MODULE, {type, user_age, 0}),
    DecodedFromTuple = json:decode(iolist_to_binary(SchemaFromTuple)),

    % Both should be identical and include metadata
    ?assertEqual(DecodedFromTuple, DecodedFromAtom),

    ?assertMatch(
        #{
            <<"title">> := <<"User Age">>,
            <<"description">> := <<"Age of a user in years">>,
            <<"examples">> := [18, 25, 42, 65],
            <<"type">> := <<"integer">>,
            <<"minimum">> := 0
        },
        DecodedFromAtom
    ).

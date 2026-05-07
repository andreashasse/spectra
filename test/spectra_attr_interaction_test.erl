%% Regression tests for -spectra() attribute interactions when both the
%% definition site and the alias site define the same attribute.
%%
%% The base type used throughout is defined in spectra_attr_interaction_base:
%%   t/0      :: #{first_name, last_name, email} with first_name → "firstName"
%%   t_only/0 :: same but with only => [first_name, last_name] at definition site
%%
%% Summary of expected behaviour:
%%
%%   field_aliases
%%     - Definition-site and alias-site aliases MERGE.
%%     - Alias-site wins when both sides alias the SAME field.
%%
%%   only
%%     - The two filters INTERSECT (both must allow a field for it to survive).
%%     - Alias-site cannot add back fields the definition site removed.
-module(spectra_attr_interaction_test).

-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_unused_type).

%% field_aliases: different fields at each site → merge
%% Base aliases first_name → "firstName"; this module aliases last_name → "lastName".
-spectra(#{field_aliases => #{last_name => <<"lastName">>}}).
-type aliases_merge() :: spectra_attr_interaction_base:t().

%% field_aliases: same field at both sites → alias site wins
%% Base aliases first_name → "firstName"; this module re-aliases it to "fn".
-spectra(#{field_aliases => #{first_name => <<"fn">>}}).
-type aliases_override() :: spectra_attr_interaction_base:t().

%% only: definition site keeps [first_name, last_name]; alias site narrows to [first_name].
%% Expected intersection: [first_name].
-spectra(#{only => [first_name]}).
-type only_intersection() :: spectra_attr_interaction_base:t_only().

%% only: alias site requests a field the definition site already removed.
%% Definition site keeps [first_name, last_name] (email gone); alias site asks for [first_name, email].
%% Expected: only first_name survives — email cannot come back.
-spectra(#{only => [first_name, email]}).
-type only_cannot_expand() :: spectra_attr_interaction_base:t_only().

%% ---------------------------------------------------------------------------
%% field_aliases merge tests
%% ---------------------------------------------------------------------------

aliases_merge_encode_test() ->
    ?assertEqual(
        {ok, #{
            <<"firstName">> => <<"Alice">>, <<"lastName">> => <<"Smith">>, <<"email">> => <<"a@b">>
        }},
        spectra:encode(
            json,
            ?MODULE,
            {type, aliases_merge, 0},
            #{first_name => <<"Alice">>, last_name => <<"Smith">>, email => <<"a@b">>},
            [pre_encoded]
        )
    ).

aliases_merge_decode_test() ->
    ?assertEqual(
        {ok, #{first_name => <<"Alice">>, last_name => <<"Smith">>, email => <<"a@b">>}},
        spectra:decode(
            json,
            ?MODULE,
            {type, aliases_merge, 0},
            #{
                <<"firstName">> => <<"Alice">>,
                <<"lastName">> => <<"Smith">>,
                <<"email">> => <<"a@b">>
            },
            [pre_decoded]
        )
    ).

%% ---------------------------------------------------------------------------
%% field_aliases override tests
%% ---------------------------------------------------------------------------

aliases_override_encode_test() ->
    ?assertEqual(
        {ok, #{<<"fn">> => <<"Alice">>, <<"last_name">> => <<"Smith">>, <<"email">> => <<"a@b">>}},
        spectra:encode(
            json,
            ?MODULE,
            {type, aliases_override, 0},
            #{first_name => <<"Alice">>, last_name => <<"Smith">>, email => <<"a@b">>},
            [pre_encoded]
        )
    ).

aliases_override_decode_test() ->
    ?assertEqual(
        {ok, #{first_name => <<"Alice">>, last_name => <<"Smith">>, email => <<"a@b">>}},
        spectra:decode(
            json,
            ?MODULE,
            {type, aliases_override, 0},
            #{<<"fn">> => <<"Alice">>, <<"last_name">> => <<"Smith">>, <<"email">> => <<"a@b">>},
            [pre_decoded]
        )
    ).

%% ---------------------------------------------------------------------------
%% only intersection tests
%% ---------------------------------------------------------------------------

only_intersection_encode_test() ->
    ?assertEqual(
        {ok, #{<<"firstName">> => <<"Alice">>}},
        spectra:encode(
            json,
            ?MODULE,
            {type, only_intersection, 0},
            #{first_name => <<"Alice">>, last_name => <<"Smith">>, email => <<"a@b">>},
            [pre_encoded]
        )
    ).

only_intersection_decode_test() ->
    ?assertEqual(
        {ok, #{first_name => <<"Alice">>}},
        spectra:decode(
            json,
            ?MODULE,
            {type, only_intersection, 0},
            #{<<"firstName">> => <<"Alice">>, <<"last_name">> => <<"Smith">>},
            [pre_decoded]
        )
    ).

%% ---------------------------------------------------------------------------
%% only cannot expand tests
%% ---------------------------------------------------------------------------

only_cannot_expand_encode_test() ->
    ?assertEqual(
        {ok, #{<<"firstName">> => <<"Alice">>}},
        spectra:encode(
            json,
            ?MODULE,
            {type, only_cannot_expand, 0},
            #{first_name => <<"Alice">>, last_name => <<"Smith">>, email => <<"a@b">>},
            [pre_encoded]
        )
    ).

only_cannot_expand_decode_test() ->
    ?assertEqual(
        {ok, #{first_name => <<"Alice">>}},
        spectra:decode(
            json,
            ?MODULE,
            {type, only_cannot_expand, 0},
            #{<<"firstName">> => <<"Alice">>, <<"email">> => <<"a@b">>},
            [pre_decoded]
        )
    ).

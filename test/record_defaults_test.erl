-module(record_defaults_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-record(rec_with_defaults, {
    name :: binary(),
    status = active :: atom(),
    count = 0 :: integer(),
    opt = undefined :: undefined | binary(),
    items = [] :: [atom()],
    tag = <<"default">> :: binary()
}).

-record(rec_with_complex_defaults, {
    key :: binary(),
    pair = {ok, 1} :: {atom(), integer()},
    nonempty = [a, b] :: [atom()],
    meta = #{foo => bar} :: map()
}).

%% Expose only 'name'; excluded fields should fall back to their record defaults.
-spectra(#{only => [name]}).
-type name_only() :: #rec_with_defaults{}.

-spectra(#{only => [key]}).
-type key_only() :: #rec_with_complex_defaults{}.

-export_type([name_only/0, key_only/0]).

excluded_atom_default_test() ->
    {ok, Result} = spectra:decode(
        json, ?MODULE, {type, name_only, 0}, #{<<"name">> => <<"alice">>}, [pre_decoded]
    ),
    ?assertEqual(
        #rec_with_defaults{
            name = <<"alice">>,
            status = active,
            count = 0,
            opt = undefined,
            items = [],
            tag = <<"default">>
        },
        Result
    ).

excluded_integer_default_test() ->
    {ok, #rec_with_defaults{count = Count}} = spectra:decode(
        json, ?MODULE, {type, name_only, 0}, #{<<"name">> => <<"bob">>}, [pre_decoded]
    ),
    ?assertEqual(0, Count).

excluded_undefined_default_test() ->
    {ok, #rec_with_defaults{opt = Opt}} = spectra:decode(
        json, ?MODULE, {type, name_only, 0}, #{<<"name">> => <<"carol">>}, [pre_decoded]
    ),
    ?assertEqual(undefined, Opt).

excluded_list_default_test() ->
    {ok, #rec_with_defaults{items = Items}} = spectra:decode(
        json, ?MODULE, {type, name_only, 0}, #{<<"name">> => <<"dave">>}, [pre_decoded]
    ),
    ?assertEqual([], Items).

excluded_binary_default_test() ->
    {ok, #rec_with_defaults{tag = Tag}} = spectra:decode(
        json, ?MODULE, {type, name_only, 0}, #{<<"name">> => <<"eve">>}, [pre_decoded]
    ),
    ?assertEqual(<<"default">>, Tag).

included_field_missing_is_error_test() ->
    ?assertMatch(
        {error, [#sp_error{location = [name], type = missing_data}]},
        spectra:decode(json, ?MODULE, {type, name_only, 0}, #{}, [pre_decoded])
    ).

excluded_tuple_default_test() ->
    {ok, #rec_with_complex_defaults{pair = Pair}} = spectra:decode(
        json, ?MODULE, {type, key_only, 0}, #{<<"key">> => <<"x">>}, [pre_decoded]
    ),
    ?assertEqual({ok, 1}, Pair).

excluded_nonempty_list_default_test() ->
    {ok, #rec_with_complex_defaults{nonempty = List}} = spectra:decode(
        json, ?MODULE, {type, key_only, 0}, #{<<"key">> => <<"x">>}, [pre_decoded]
    ),
    ?assertEqual([a, b], List).

excluded_map_default_test() ->
    {ok, #rec_with_complex_defaults{meta = Meta}} = spectra:decode(
        json, ?MODULE, {type, key_only, 0}, #{<<"key">> => <<"x">>}, [pre_decoded]
    ),
    ?assertEqual(#{foo => bar}, Meta).

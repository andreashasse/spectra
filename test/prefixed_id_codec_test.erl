-module(prefixed_id_codec_test).

-compile(nowarn_missing_spec).

-include_lib("eunit/include/eunit.hrl").

%% -----------------------------------------------------------------------
%% decode
%% -----------------------------------------------------------------------

decode_user_id_valid_test() ->
    ?assertEqual(
        {ok, <<"abc123">>},
        spectra:decode(json, prefixed_id_codec, user_id, <<"user:abc123">>)
    ).

decode_org_id_valid_test() ->
    ?assertEqual(
        {ok, <<"abc123">>},
        spectra:decode(json, prefixed_id_codec, org_id, <<"org:abc123">>)
    ).

decode_user_id_wrong_prefix_test() ->
    ?assertMatch(
        {error, _},
        spectra:decode(json, prefixed_id_codec, user_id, <<"org:abc123">>)
    ).

decode_org_id_wrong_prefix_test() ->
    ?assertMatch(
        {error, _},
        spectra:decode(json, prefixed_id_codec, org_id, <<"user:abc123">>)
    ).

decode_empty_id_after_prefix_test() ->
    ?assertEqual(
        {ok, <<>>},
        spectra:decode(json, prefixed_id_codec, user_id, <<"user:">>)
    ).

%% -----------------------------------------------------------------------
%% encode
%% -----------------------------------------------------------------------

encode_user_id_test() ->
    ?assertEqual(
        {ok, <<"user:abc123">>},
        spectra:encode(json, prefixed_id_codec, user_id, <<"abc123">>, [pre_encoded])
    ).

encode_org_id_test() ->
    ?assertEqual(
        {ok, <<"org:abc123">>},
        spectra:encode(json, prefixed_id_codec, org_id, <<"abc123">>, [pre_encoded])
    ).

%% -----------------------------------------------------------------------
%% schema
%% -----------------------------------------------------------------------

schema_user_id_test() ->
    Schema = json:decode(iolist_to_binary(spectra:schema(json_schema, prefixed_id_codec, user_id))),
    ?assertMatch(
        #{<<"type">> := <<"string">>, <<"pattern">> := <<"^user:">>},
        Schema
    ).

schema_org_id_test() ->
    Schema = json:decode(iolist_to_binary(spectra:schema(json_schema, prefixed_id_codec, org_id))),
    ?assertMatch(
        #{<<"type">> := <<"string">>, <<"pattern">> := <<"^org:">>},
        Schema
    ).

%% -----------------------------------------------------------------------
%% roundtrip
%% -----------------------------------------------------------------------

roundtrip_user_id_test() ->
    {ok, Encoded} = spectra:encode(json, prefixed_id_codec, user_id, <<"abc123">>, [pre_encoded]),
    ?assertEqual(
        {ok, <<"abc123">>},
        spectra:decode(json, prefixed_id_codec, user_id, Encoded)
    ).

roundtrip_org_id_test() ->
    {ok, Encoded} = spectra:encode(json, prefixed_id_codec, org_id, <<"xyz789">>, [pre_encoded]),
    ?assertEqual(
        {ok, <<"xyz789">>},
        spectra:decode(json, prefixed_id_codec, org_id, Encoded)
    ).

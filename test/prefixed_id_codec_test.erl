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

%% -----------------------------------------------------------------------
%% openapi components — codec schema must be used for binary type_parameters
%% -----------------------------------------------------------------------

%% Regression: generate_components used to call to_inline_schema which resolved
%% user_id() to #sp_simple_type{type=binary} with parameters => <<"user:">> and
%% then crashed in apply_string_params/2 because it expected a map, not a binary.
%% The codec's schema/5 callback must be invoked instead.
%% The outer OpenAPI structure uses binary keys (typed via openapi_spec()),
%% but inner schema objects keep atom keys (openapi_schema() is a passthrough term).
openapi_components_user_id_uses_codec_schema_test() ->
    Endpoint =
        spectra_openapi:with_request_body(
            spectra_openapi:endpoint(post, <<"/items">>),
            prefixed_id_codec,
            {type, user_id, 0}
        ),
    {ok, Spec} = spectra_openapi:endpoints_to_openapi(
        #{title => <<"Test">>, version => <<"1">>},
        [Endpoint],
        [pre_encoded]
    ),
    #{<<"components">> := #{<<"schemas">> := #{<<"UserId0">> := UserIdSchema}}} = Spec,
    ?assertMatch(#{type := <<"string">>, pattern := <<"^user:">>}, UserIdSchema).

openapi_components_org_id_uses_codec_schema_test() ->
    Endpoint =
        spectra_openapi:with_request_body(
            spectra_openapi:endpoint(post, <<"/items">>),
            prefixed_id_codec,
            {type, org_id, 0}
        ),
    {ok, Spec} = spectra_openapi:endpoints_to_openapi(
        #{title => <<"Test">>, version => <<"1">>},
        [Endpoint],
        [pre_encoded]
    ),
    #{<<"components">> := #{<<"schemas">> := #{<<"OrgId0">> := OrgIdSchema}}} = Spec,
    ?assertMatch(#{type := <<"string">>, pattern := <<"^org:">>}, OrgIdSchema).

-module(dict_codec_test).

-compile(nowarn_missing_spec).

-include_lib("eunit/include/eunit.hrl").

%% Register the dict codec for dict:dict/2 before each test.
set_codec() ->
    application:set_env(spectra, codecs, #{
        {dict, {type, dict, 2}} => dict_codec
    }).

unset_codec() ->
    application:unset_env(spectra, codecs).

%% -----------------------------------------------------------------------
%% encode
%% -----------------------------------------------------------------------

encode_word_counts_test() ->
    set_codec(),
    try
        D = dict:from_list([{<<"hello">>, 3}, {<<"world">>, 1}]),
        {ok, Json} = spectra:encode(json, dict_codec_module, word_counts, D, [pre_encoded]),
        ?assertMatch(#{<<"hello">> := 3, <<"world">> := 1}, Json)
    after
        unset_codec()
    end.

encode_empty_dict_test() ->
    set_codec(),
    try
        D = dict:new(),
        ?assertEqual(
            {ok, #{}},
            spectra:encode(json, dict_codec_module, word_counts, D, [pre_encoded])
        )
    after
        unset_codec()
    end.

%% -----------------------------------------------------------------------
%% decode
%% -----------------------------------------------------------------------

decode_word_counts_test() ->
    set_codec(),
    try
        Json = #{<<"hello">> => 3, <<"world">> => 1},
        {ok, D} = spectra:decode(json, dict_codec_module, word_counts, Json, [pre_decoded]),
        ?assertEqual(3, dict:fetch(<<"hello">>, D)),
        ?assertEqual(1, dict:fetch(<<"world">>, D))
    after
        unset_codec()
    end.

decode_empty_dict_test() ->
    set_codec(),
    try
        {ok, D} = spectra:decode(json, dict_codec_module, word_counts, #{}, [pre_decoded]),
        ?assertEqual(0, dict:size(D))
    after
        unset_codec()
    end.

decode_type_error_test() ->
    set_codec(),
    try
        Json = #{<<"hello">> => <<"not_an_integer">>},
        ?assertMatch(
            {error, _},
            spectra:decode(json, dict_codec_module, word_counts, Json, [pre_decoded])
        )
    after
        unset_codec()
    end.

%% -----------------------------------------------------------------------
%% schema
%% -----------------------------------------------------------------------

schema_word_counts_test() ->
    set_codec(),
    try
        Schema = json:decode(
            iolist_to_binary(spectra:schema(json_schema, dict_codec_module, word_counts))
        ),
        ?assertMatch(
            #{
                <<"type">> := <<"object">>,
                <<"additionalProperties">> := #{<<"type">> := <<"integer">>, <<"minimum">> := 0}
            },
            Schema
        )
    after
        unset_codec()
    end.

%% -----------------------------------------------------------------------
%% roundtrip
%% -----------------------------------------------------------------------

roundtrip_word_counts_test() ->
    set_codec(),
    try
        D = dict:from_list([{<<"foo">>, 42}, {<<"bar">>, 7}]),
        {ok, Encoded} = spectra:encode(json, dict_codec_module, word_counts, D, [pre_encoded]),
        {ok, Decoded} = spectra:decode(json, dict_codec_module, word_counts, Encoded, [pre_decoded]),
        ?assertEqual(lists:sort(dict:to_list(D)), lists:sort(dict:to_list(Decoded)))
    after
        unset_codec()
    end.

%% -----------------------------------------------------------------------
%% nested dict (dict:dict(binary(), dict:dict(binary(), integer())))
%% -----------------------------------------------------------------------

nested_encode_test() ->
    set_codec(),
    try
        Inner = dict:from_list([{<<"x">>, 1}]),
        Outer = dict:from_list([{<<"a">>, Inner}]),
        {ok, Json} = spectra:encode(json, dict_codec_module, nested, Outer, [pre_encoded]),
        ?assertMatch(#{<<"a">> := #{<<"x">> := 1}}, Json)
    after
        unset_codec()
    end.

nested_decode_test() ->
    set_codec(),
    try
        Json = #{<<"a">> => #{<<"x">> => 1}},
        {ok, Outer} = spectra:decode(json, dict_codec_module, nested, Json, [pre_decoded]),
        Inner = dict:fetch(<<"a">>, Outer),
        ?assertEqual(1, dict:fetch(<<"x">>, Inner))
    after
        unset_codec()
    end.

-module(dict_codec_test).

-compile(nowarn_missing_spec).

-include_lib("eunit/include/eunit.hrl").

dict_codec_test_() ->
    {foreach,
        fun() ->
            application:set_env(spectra, codecs, #{
                {dict, {type, dict, 2}} => spectra_dict_codec
            })
        end,
        fun(_) -> application:unset_env(spectra, codecs) end, [
            fun(_) -> fun encode_word_counts/0 end,
            fun(_) -> fun encode_empty_dict/0 end,
            fun(_) -> fun decode_word_counts/0 end,
            fun(_) -> fun decode_empty_dict/0 end,
            fun(_) -> fun decode_type_error/0 end,
            fun(_) -> fun schema_word_counts/0 end,
            fun(_) -> fun roundtrip_word_counts/0 end,
            fun(_) -> fun nested_encode/0 end,
            fun(_) -> fun nested_decode/0 end
        ]}.

%% -----------------------------------------------------------------------
%% encode
%% -----------------------------------------------------------------------

encode_word_counts() ->
    D = dict:from_list([{<<"hello">>, 3}, {<<"world">>, 1}]),
    {ok, Json} = spectra:encode(json, dict_codec_module, word_counts, D, [pre_encoded]),
    ?assertMatch(#{<<"hello">> := 3, <<"world">> := 1}, Json).

encode_empty_dict() ->
    D = dict:new(),
    ?assertEqual(
        {ok, #{}},
        spectra:encode(json, dict_codec_module, word_counts, D, [pre_encoded])
    ).

%% -----------------------------------------------------------------------
%% decode
%% -----------------------------------------------------------------------

decode_word_counts() ->
    Json = #{<<"hello">> => 3, <<"world">> => 1},
    {ok, D} = spectra:decode(json, dict_codec_module, word_counts, Json, [pre_decoded]),
    ?assertEqual(3, dict:fetch(<<"hello">>, D)),
    ?assertEqual(1, dict:fetch(<<"world">>, D)).

decode_empty_dict() ->
    {ok, D} = spectra:decode(json, dict_codec_module, word_counts, #{}, [pre_decoded]),
    ?assertEqual(0, dict:size(D)).

decode_type_error() ->
    Json = #{<<"hello">> => <<"not_an_integer">>},
    ?assertMatch(
        {error, _},
        spectra:decode(json, dict_codec_module, word_counts, Json, [pre_decoded])
    ).

%% -----------------------------------------------------------------------
%% schema
%% -----------------------------------------------------------------------

schema_word_counts() ->
    Schema = json:decode(
        iolist_to_binary(spectra:schema(json_schema, dict_codec_module, word_counts))
    ),
    ?assertMatch(
        #{
            <<"type">> := <<"object">>,
            <<"additionalProperties">> := #{<<"type">> := <<"integer">>, <<"minimum">> := 0}
        },
        Schema
    ).

%% -----------------------------------------------------------------------
%% roundtrip
%% -----------------------------------------------------------------------

roundtrip_word_counts() ->
    D = dict:from_list([{<<"foo">>, 42}, {<<"bar">>, 7}]),
    {ok, Encoded} = spectra:encode(json, dict_codec_module, word_counts, D, [pre_encoded]),
    {ok, Decoded} = spectra:decode(json, dict_codec_module, word_counts, Encoded, [pre_decoded]),
    ?assertEqual(lists:sort(dict:to_list(D)), lists:sort(dict:to_list(Decoded))).

%% -----------------------------------------------------------------------
%% nested dict (dict:dict(binary(), dict:dict(binary(), integer())))
%% -----------------------------------------------------------------------

nested_encode() ->
    Inner = dict:from_list([{<<"x">>, 1}]),
    Outer = dict:from_list([{<<"a">>, Inner}]),
    {ok, Json} = spectra:encode(json, dict_codec_module, nested, Outer, [pre_encoded]),
    ?assertMatch(#{<<"a">> := #{<<"x">> := 1}}, Json).

nested_decode() ->
    Json = #{<<"a">> => #{<<"x">> => 1}},
    {ok, Outer} = spectra:decode(json, dict_codec_module, nested, Json, [pre_decoded]),
    Inner = dict:fetch(<<"a">>, Outer),
    ?assertEqual(1, dict:fetch(<<"x">>, Inner)).

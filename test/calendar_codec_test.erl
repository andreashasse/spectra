-module(calendar_codec_test).

-compile(nowarn_missing_spec).

-include_lib("eunit/include/eunit.hrl").

datetime_codec_test_() ->
    {foreach,
        fun() ->
            application:set_env(spectra, codecs, #{
                {calendar, {type, datetime, 0}} => spectra_calendar_codec
            })
        end,
        fun(_) -> application:unset_env(spectra, codecs) end, [
            fun(_) -> fun datetime_encode/0 end,
            fun(_) -> fun datetime_encode_bad_value/0 end,
            fun(_) -> fun datetime_decode/0 end,
            fun(_) -> fun datetime_decode_bad_string/0 end,
            fun(_) -> fun datetime_decode_non_binary/0 end,
            fun(_) -> fun datetime_schema/0 end,
            fun(_) -> fun datetime_roundtrip/0 end
        ]}.

date_codec_test_() ->
    {foreach,
        fun() ->
            application:set_env(spectra, codecs, #{
                {calendar, {type, date, 0}} => spectra_calendar_codec
            })
        end,
        fun(_) -> application:unset_env(spectra, codecs) end, [
            fun(_) -> fun date_encode/0 end,
            fun(_) -> fun date_encode_bad_value/0 end,
            fun(_) -> fun date_decode/0 end,
            fun(_) -> fun date_decode_bad_string/0 end,
            fun(_) -> fun date_schema/0 end,
            fun(_) -> fun date_roundtrip/0 end
        ]}.

%% -----------------------------------------------------------------------
%% calendar:datetime()
%% -----------------------------------------------------------------------

datetime_encode() ->
    ?assertEqual(
        {ok, #{<<"title">> => <<"Party">>, <<"at">> => <<"2024-01-15T10:30:00">>}},
        spectra:encode(
            json,
            calendar_codec_module,
            {type, event, 0},
            #{title => <<"Party">>, at => {{2024, 1, 15}, {10, 30, 0}}},
            [pre_encoded]
        )
    ).

datetime_encode_bad_value() ->
    ?assertMatch(
        {error, [_]},
        spectra:encode(
            json,
            calendar_codec_module,
            {type, event, 0},
            #{title => <<"Party">>, at => not_a_datetime},
            [pre_encoded]
        )
    ).

datetime_decode() ->
    ?assertEqual(
        {ok, #{title => <<"Party">>, at => {{2024, 1, 15}, {10, 30, 0}}}},
        spectra:decode(
            json,
            calendar_codec_module,
            {type, event, 0},
            #{<<"title">> => <<"Party">>, <<"at">> => <<"2024-01-15T10:30:00">>},
            [pre_decoded]
        )
    ).

datetime_decode_bad_string() ->
    ?assertMatch(
        {error, [_]},
        spectra:decode(
            json,
            calendar_codec_module,
            {type, event, 0},
            #{<<"title">> => <<"Party">>, <<"at">> => <<"not-a-date">>},
            [pre_decoded]
        )
    ).

datetime_decode_non_binary() ->
    ?assertMatch(
        {error, [_]},
        spectra:decode(
            json,
            calendar_codec_module,
            {type, event, 0},
            #{<<"title">> => <<"Party">>, <<"at">> => 12345},
            [pre_decoded]
        )
    ).

datetime_schema() ->
    Schema = spectra:schema(json_schema, calendar_codec_module, {type, event, 0}, [pre_encoded]),
    ?assertMatch(
        #{properties := #{<<"at">> := #{type := <<"string">>, format := <<"date-time">>}}},
        Schema
    ).

datetime_roundtrip() ->
    DT = {{2024, 6, 1}, {18, 0, 0}},
    Event = #{title => <<"Summer party">>, at => DT},
    {ok, Encoded} = spectra:encode(
        json, calendar_codec_module, {type, event, 0}, Event, [pre_encoded]
    ),
    ?assertEqual(
        {ok, Event},
        spectra:decode(json, calendar_codec_module, {type, event, 0}, Encoded, [pre_decoded])
    ).

%% -----------------------------------------------------------------------
%% calendar:date()
%% -----------------------------------------------------------------------

date_encode() ->
    ?assertEqual(
        {ok, #{<<"title">> => <<"Dentist">>, <<"on">> => <<"2024-03-20">>}},
        spectra:encode(
            json,
            calendar_codec_module,
            {type, appointment, 0},
            #{title => <<"Dentist">>, on => {2024, 3, 20}},
            [pre_encoded]
        )
    ).

date_encode_bad_value() ->
    ?assertMatch(
        {error, [_]},
        spectra:encode(
            json,
            calendar_codec_module,
            {type, appointment, 0},
            #{title => <<"Dentist">>, on => not_a_date},
            [pre_encoded]
        )
    ).

date_decode() ->
    ?assertEqual(
        {ok, #{title => <<"Dentist">>, on => {2024, 3, 20}}},
        spectra:decode(
            json,
            calendar_codec_module,
            {type, appointment, 0},
            #{<<"title">> => <<"Dentist">>, <<"on">> => <<"2024-03-20">>},
            [pre_decoded]
        )
    ).

date_decode_bad_string() ->
    ?assertMatch(
        {error, [_]},
        spectra:decode(
            json,
            calendar_codec_module,
            {type, appointment, 0},
            #{<<"title">> => <<"Dentist">>, <<"on">> => <<"not-a-date">>},
            [pre_decoded]
        )
    ).

date_schema() ->
    Schema = spectra:schema(
        json_schema, calendar_codec_module, {type, appointment, 0}, [pre_encoded]
    ),
    ?assertMatch(
        #{properties := #{<<"on">> := #{type := <<"string">>, format := <<"date">>}}},
        Schema
    ).

date_roundtrip() ->
    Date = {2024, 12, 25},
    Appt = #{title => <<"Christmas">>, on => Date},
    {ok, Encoded} = spectra:encode(
        json, calendar_codec_module, {type, appointment, 0}, Appt, [pre_encoded]
    ),
    ?assertEqual(
        {ok, Appt},
        spectra:decode(
            json, calendar_codec_module, {type, appointment, 0}, Encoded, [pre_decoded]
        )
    ).

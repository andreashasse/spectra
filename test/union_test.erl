-module(union_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").

-compile(nowarn_unused_type).

-record(address, {street :: string(), city :: string() | undefined}).

-type score() :: #{value := 1..10, comment => #{lang := string(), text := string()}}.
-type id_or_tags() :: integer() | [binary()].
-type color() :: red | green | blue.
-type optional_color() :: red | nil.
-type weird_union() :: #address{} | #{city => string(), score => score()}.

%% Test function to validate weird_union type
validate_weird_union_test() ->
    % Test JSON conversion using to_json
    ValidRecord = #address{street = "Main St", city = "New York"},
    ValidMap =
        #{city => "Boston", score => #{value => 8, comment => #{lang => "en", text => "Great"}}},
    InvalidData = #{invalid => "data"},

    % Test with valid record
    ?assertEqual(
        {ok, #{<<"street">> => <<"Main St">>, <<"city">> => <<"New York">>}},
        to_json_weird_union(ValidRecord)
    ),

    % Test with valid map
    ?assertEqual(
        {ok, #{
            <<"city">> => <<"Boston">>,
            <<"score">> => #{
                <<"value">> => 8,
                <<"comment">> => #{<<"lang">> => <<"en">>, <<"text">> => <<"Great">>}
            }
        }},
        to_json_weird_union(ValidMap)
    ),

    % Test with extra fields (now ignored)
    ?assertEqual(
        {ok, #{}},
        to_json_weird_union(InvalidData)
    ),

    % Test JSON conversion using from_json
    ValidRecordJson = #{<<"street">> => <<"Main St">>, <<"city">> => <<"New York">>},
    ValidMapJson =
        #{
            <<"city">> => <<"Boston">>,
            <<"score">> =>
                #{
                    <<"value">> => 8,
                    <<"comment">> => #{<<"lang">> => <<"en">>, <<"text">> => <<"Great">>}
                }
        },
    InvalidJson = #{<<"invalid">> => <<"data">>},

    % Test from_json with valid record
    ?assertEqual(
        {ok, #address{street = "Main St", city = "New York"}},
        from_json_weird_union(ValidRecordJson)
    ),

    % Test from_json with valid map
    ?assertEqual(
        {ok, #{
            city => "Boston",
            score => #{value => 8, comment => #{lang => "en", text => "Great"}}
        }},
        from_json_weird_union(ValidMapJson)
    ),

    % Test from_json with extra fields (now ignored)
    ?assertEqual(
        {ok, #{}},
        from_json_weird_union(InvalidJson)
    ).

-spec to_json_weird_union(weird_union()) ->
    {ok, json:encode_value()} | {error, [spectra:error()]}.
to_json_weird_union(Data) ->
    spectra:encode(json, ?MODULE, {type, weird_union, 0}, Data, [pre_encoded]).

-spec from_json_weird_union(json:encode_value()) ->
    {ok, weird_union()} | {error, [spectra:error()]}.
from_json_weird_union(Json) ->
    spectra:decode(json, ?MODULE, {type, weird_union, 0}, Json, [pre_decoded]).

decode_union_errors_in_declaration_order_test() ->
    ?assertMatch(
        {error, [
            #sp_error{
                type = no_match,
                ctx = #{
                    errors := [
                        {_, [#sp_error{type = type_mismatch, location = [], ctx = #{value := [1]}}]},
                        {_, [#sp_error{type = type_mismatch, location = [1], ctx = #{value := 1}}]}
                    ]
                }
            }
        ]},
        spectra:decode(json, ?MODULE, id_or_tags, [1], [pre_decoded])
    ).

encode_union_errors_in_declaration_order_test() ->
    ?assertMatch(
        {error, [
            #sp_error{
                type = no_match,
                ctx = #{
                    errors := [
                        {_, [#sp_error{type = type_mismatch, location = [], ctx = #{value := [1]}}]},
                        {_, [#sp_error{type = type_mismatch, location = [1], ctx = #{value := 1}}]}
                    ]
                }
            }
        ]},
        spectra:encode(json, ?MODULE, id_or_tags, [1], [pre_encoded])
    ).

decode_enum_mismatch_lists_allowed_values_test() ->
    ?assertMatch(
        {error, [
            #sp_error{
                type = type_mismatch,
                location = [],
                ctx = #{
                    value := <<"purple">>,
                    allowed_values := [<<"red">>, <<"green">>, <<"blue">>]
                }
            }
        ]},
        spectra:decode(json, ?MODULE, color, <<"purple">>, [pre_decoded])
    ).

encode_enum_mismatch_lists_allowed_values_test() ->
    ?assertMatch(
        {error, [
            #sp_error{
                type = type_mismatch,
                location = [],
                ctx = #{value := purple, allowed_values := [<<"red">>, <<"green">>, <<"blue">>]}
            }
        ]},
        spectra:encode(json, ?MODULE, color, purple, [pre_encoded])
    ).

decode_enum_mismatch_lists_null_for_nil_test() ->
    ?assertMatch(
        {error, [
            #sp_error{type = type_mismatch, ctx = #{allowed_values := [<<"red">>, null]}}
        ]},
        spectra:decode(json, ?MODULE, optional_color, <<"purple">>, [pre_decoded])
    ).

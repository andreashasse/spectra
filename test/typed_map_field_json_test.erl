-module(typed_map_field_json_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").

-compile(nowarn_unused_type).

%% Test types with typed map fields - string-like keys
-type string_key_map_assoc() :: #{string() => integer()}.
-type string_key_map_exact() :: #{string() := integer()}.
-type binary_key_map_assoc() :: #{binary() => atom()}.
-type atom_key_map_assoc() :: #{atom() => binary()}.

%% Test types with typed map fields - integer keys
-type integer_key_map_assoc() :: #{integer() => binary()}.

%% Mixed types
-type mixed_literal_and_typed() :: #{name := binary(), string() => integer()}.

%%====================================================================
%% decode Tests - JSON binary to Erlang term
%%====================================================================

decode_string_key_map_assoc_empty_test() ->
    JsonBinary = <<"{}">>,
    {ok, Data} = spectra:decode(json, ?MODULE, string_key_map_assoc, JsonBinary),
    ?assertEqual(#{}, Data).

decode_string_key_map_assoc_one_entry_test() ->
    JsonBinary = <<"{\"key1\":42}">>,
    {ok, Data} = spectra:decode(json, ?MODULE, string_key_map_assoc, JsonBinary),
    ?assertEqual(#{"key1" => 42}, Data).

decode_string_key_map_assoc_multiple_entries_test() ->
    JsonBinary = <<"{\"key1\":42,\"key2\":100}">>,
    {ok, Data} = spectra:decode(json, ?MODULE, string_key_map_assoc, JsonBinary),
    ?assertEqual(#{"key1" => 42, "key2" => 100}, Data).

decode_string_key_map_exact_empty_test() ->
    JsonBinary = <<"{}">>,
    ?assertMatch(
        {error, [#sp_error{type = not_matched_fields}]},
        spectra:decode(json, ?MODULE, string_key_map_exact, JsonBinary)
    ).

decode_string_key_map_exact_with_entry_test() ->
    JsonBinary = <<"{\"key1\":42}">>,
    {ok, Data} = spectra:decode(json, ?MODULE, string_key_map_exact, JsonBinary),
    ?assertEqual(#{"key1" => 42}, Data).

decode_binary_key_map_assoc_test() ->
    JsonBinary = <<"{\"key1\":\"foo\",\"key2\":\"bar\"}">>,
    {ok, Data} = spectra:decode(json, ?MODULE, binary_key_map_assoc, JsonBinary),
    ?assertEqual(#{<<"key1">> => foo, <<"key2">> => bar}, Data).

decode_atom_key_map_assoc_test() ->
    JsonBinary = <<"{\"key1\":\"value1\",\"key2\":\"value2\"}">>,
    {ok, Data} = spectra:decode(json, ?MODULE, atom_key_map_assoc, JsonBinary),
    ?assertEqual(#{key1 => <<"value1">>, key2 => <<"value2">>}, Data).

decode_string_key_wrong_value_type_test() ->
    JsonBinary = <<"{\"key1\":\"not_an_integer\"}">>,
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra:decode(json, ?MODULE, string_key_map_assoc, JsonBinary)
    ).

decode_atom_key_non_existing_atom_test() ->
    %% Atom keys require the atom to already exist
    JsonBinary = <<"{\"this_atom_does_not_exist_12345\":\"value\"}">>,
    {ok, Data} = spectra:decode(json, ?MODULE, atom_key_map_assoc, JsonBinary),
    ?assertEqual(#{}, Data).

decode_integer_key_map_assoc_test() ->
    %% JSON keys are strings, but the type expects integer() keys
    %% String keys don't match integer(), so entries are skipped
    JsonBinary = <<"{\"1\":\"value1\",\"2\":\"value2\"}">>,
    {ok, Data} = spectra:decode(json, ?MODULE, integer_key_map_assoc, JsonBinary),
    ?assertEqual(#{}, Data).

decode_mixed_literal_and_typed_test() ->
    JsonBinary = <<"{\"name\":\"Alice\",\"age\":30}">>,
    {ok, Data} = spectra:decode(json, ?MODULE, mixed_literal_and_typed, JsonBinary),
    ?assertEqual(#{name => <<"Alice">>, "age" => 30}, Data).

decode_integer_key_map_note_test() ->
    %% This demonstrates that integer keys DON'T work with decode!
    %% JSON has string keys, but type expects integer() keys
    JsonBinary = <<"{\"1\":\"value1\",\"2\":\"value2\"}">>,
    {ok, Decoded} = spectra:decode(json, ?MODULE, integer_key_map_assoc, JsonBinary),
    ?assertEqual(#{}, Decoded).

%%====================================================================
%% Round trip Tests - JSON binary -> Erlang term -> JSON binary
%%====================================================================

roundtrip_string_key_map_assoc_empty_test() ->
    InputJson = <<"{}">>,
    {ok, Data} = spectra:decode(json, ?MODULE, string_key_map_assoc, InputJson),
    {ok, ReturnedJson} = spectra:encode(json, ?MODULE, string_key_map_assoc, Data),
    ?assertEqual(json:decode(InputJson), json:decode(iolist_to_binary(ReturnedJson))).

roundtrip_string_key_map_assoc_one_entry_test() ->
    InputJson = <<"{\"key1\":42}">>,
    {ok, Data} = spectra:decode(json, ?MODULE, string_key_map_assoc, InputJson),
    {ok, ReturnedJson} = spectra:encode(json, ?MODULE, string_key_map_assoc, Data),
    ?assertEqual(json:decode(InputJson), json:decode(iolist_to_binary(ReturnedJson))).

roundtrip_string_key_map_assoc_multiple_entries_test() ->
    InputJson = <<"{\"key1\":42,\"key2\":100}">>,
    {ok, Data} = spectra:decode(json, ?MODULE, string_key_map_assoc, InputJson),
    {ok, ReturnedJson} = spectra:encode(json, ?MODULE, string_key_map_assoc, Data),
    ?assertEqual(json:decode(InputJson), json:decode(iolist_to_binary(ReturnedJson))).

roundtrip_string_key_map_exact_with_entry_test() ->
    InputJson = <<"{\"key1\":42}">>,
    {ok, Data} = spectra:decode(json, ?MODULE, string_key_map_exact, InputJson),
    {ok, ReturnedJson} = spectra:encode(json, ?MODULE, string_key_map_exact, Data),
    ?assertEqual(json:decode(InputJson), json:decode(iolist_to_binary(ReturnedJson))).

roundtrip_binary_key_map_assoc_test() ->
    InputJson = <<"{\"key1\":\"foo\",\"key2\":\"bar\"}">>,
    {ok, Data} = spectra:decode(json, ?MODULE, binary_key_map_assoc, InputJson),
    {ok, ReturnedJson} = spectra:encode(json, ?MODULE, binary_key_map_assoc, Data),
    ?assertEqual(json:decode(InputJson), json:decode(iolist_to_binary(ReturnedJson))).

roundtrip_atom_key_map_assoc_test() ->
    InputJson = <<"{\"key1\":\"value1\",\"key2\":\"value2\"}">>,
    {ok, Data} = spectra:decode(json, ?MODULE, atom_key_map_assoc, InputJson),
    {ok, ReturnedJson} = spectra:encode(json, ?MODULE, atom_key_map_assoc, Data),
    ?assertEqual(json:decode(InputJson), json:decode(iolist_to_binary(ReturnedJson))).

roundtrip_mixed_literal_and_typed_test() ->
    InputJson = <<"{\"name\":\"Alice\",\"age\":30}">>,
    {ok, Data} = spectra:decode(json, ?MODULE, mixed_literal_and_typed, InputJson),
    {ok, ReturnedJson} = spectra:encode(json, ?MODULE, mixed_literal_and_typed, Data),
    ?assertEqual(json:decode(InputJson), json:decode(iolist_to_binary(ReturnedJson))).

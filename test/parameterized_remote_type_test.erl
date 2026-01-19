-module(parameterized_remote_type_test).

-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_unused_type).

%% This type uses a remote parameterized type with a type variable
%% The bug occurs when type_replace_vars tries to process this
-type my_wrapper(X) :: parameterized_remote_helper:wrapper(X).

%% A concrete instantiation - wrapper contains integer
-type int_wrapper() :: my_wrapper(integer()).

%% Using a list-based parameterized remote type
-type string_list() :: parameterized_remote_helper:list_wrapper(binary()).

%% A more complex case: a map containing a parameterized remote type
-type user_response(T) :: #{
    status := binary(),
    data := parameterized_remote_helper:wrapper(T)
}.

%% Test generating JSON schema for a type using remote parameterized type
json_schema_with_remote_param_test() ->
    %% This should generate a schema for #{result := integer(), error := binary() | nil}
    Result = spectra:schema(json_schema, ?MODULE, int_wrapper),

    %% The test will fail with the bug because type_replace_vars doesn't
    %% properly handle the type variable substitution for remote types
    ?assertMatch({ok, _SchemaJson}, Result),

    {ok, SchemaJson} = Result,
    %% Parse the JSON schema to verify structure (convert iodata to binary first)
    Schema = json:decode(iolist_to_binary(SchemaJson)),

    %% Verify the schema structure - should be an object
    ?assertMatch(#{<<"$schema">> := _, <<"type">> := <<"object">>}, Schema).

%% Test JSON conversion with remote parameterized type
json_to_from_with_remote_param_test() ->
    %% Test encode with a map-based wrapper
    ValidData = #{result => 42, error => nil},
    {ok, JsonIodata} = spectra:encode(json, ?MODULE, int_wrapper, ValidData),
    JsonBinary = iolist_to_binary(JsonIodata),

    %% Parse to verify structure - nil becomes null, but optional fields may be omitted
    ParsedJson = json:decode(JsonBinary),
    ?assertMatch(#{<<"result">> := 42}, ParsedJson),

    %% Test decode - expects JSON binary
    {ok, Decoded} = spectra:decode(json, ?MODULE, int_wrapper, JsonBinary),
    %% The decoded value might not have the exact same structure due to JSON roundtrip
    ?assertMatch(#{result := 42}, Decoded).

%% Test with list-based parameterized remote type
json_schema_list_remote_param_test() ->
    %% This tests list_wrapper(binary()) which should become [binary()]
    Result = spectra:schema(json_schema, ?MODULE, string_list),

    ?assertMatch({ok, _SchemaJson}, Result),

    {ok, SchemaJson} = Result,
    %% Parse the JSON schema to verify structure (convert iodata to binary first)
    Schema = json:decode(iolist_to_binary(SchemaJson)),
    %% Should be an array of strings
    ?assertMatch(
        #{
            <<"$schema">> := _,
            <<"type">> := <<"array">>,
            <<"items">> := #{<<"type">> := <<"string">>}
        },
        Schema
    ).

%% Concrete type for nested test
-type int_response() :: user_response(integer()).

%% Test with nested parameterized remote type in a map
json_schema_nested_remote_param_test() ->
    %% This tests a more complex case with parameterized remote type inside a map
    %% user_response(integer()) contains a wrapper(integer()) inside
    Result = spectra:schema(json_schema, ?MODULE, int_response),

    %% With the bug, this will fail during type variable replacement
    ?assertMatch({ok, _SchemaJson}, Result),

    {ok, SchemaJson} = Result,
    %% Parse the JSON schema to verify structure (convert iodata to binary first)
    Schema = json:decode(iolist_to_binary(SchemaJson)),
    %% Should be an object with status and data fields
    ?assertMatch(#{<<"$schema">> := _, <<"type">> := <<"object">>}, Schema).

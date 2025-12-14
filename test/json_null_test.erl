-module(json_null_test).

-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_unused_type).

%% Type definitions for nil
-type maybe_nil_string() :: binary() | nil.
-type maybe_nil_integer() :: integer() | nil.

%% Type definitions for undefined
-type maybe_undef_string() :: binary() | undefined.
-type maybe_undef_integer() :: integer() | undefined.

%% Map types
-type map_with_nil_field() ::
    #{
        field := maybe_nil_string()
    }.

-type map_with_undef_field() ::
    #{
        field := maybe_undef_string()
    }.

-type map_with_multiple_fields() ::
    #{
        name := binary(),
        email := maybe_nil_string()
    }.

%% Nested map type
-type inner_map_nil() ::
    #{
        inner_field := maybe_nil_string()
    }.

-type outer_map_nil() ::
    #{
        nested := inner_map_nil()
    }.

%% List types
-type list_with_nil_elements() :: [maybe_nil_string()].

%% Multiple types union
-type multi_union_nil() :: integer() | binary() | nil.

%% Record definition with nil fields
-record(record_with_nil, {
    name :: binary(),
    email :: maybe_nil_string()
}).

%% Record with undefined fields
-record(record_with_undef, {
    name :: binary(),
    email :: maybe_undef_string()
}).

direct_nil_test() ->
    Json = <<"null">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {type, maybe_nil_string, 0}, Json),
    ?assertEqual(nil, Result).

direct_undefined_test() ->
    Json = <<"null">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {type, maybe_undef_string, 0}, Json),
    ?assertEqual(undefined, Result).

union_nil_null_test() ->
    Json = <<"null">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {type, maybe_nil_string, 0}, Json),
    ?assertEqual(nil, Result).

union_nil_string_test() ->
    Json = <<"\"hello\"">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {type, maybe_nil_string, 0}, Json),
    ?assertEqual(<<"hello">>, Result).

map_field_explicit_null_nil_test() ->
    Json = <<"{\"field\":null}">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {type, map_with_nil_field, 0}, Json),
    ?assertEqual(#{field => nil}, Result).

map_field_explicit_null_undefined_test() ->
    Json = <<"{\"field\":null}">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {type, map_with_undef_field, 0}, Json),
    ?assertEqual(#{field => undefined}, Result).

map_field_missing_vs_explicit_null_test() ->
    JsonWithNull = <<"{\"field\":null}">>,
    {ok, ResultWithNull} = spectra:decode(
        json, ?MODULE, {type, map_with_nil_field, 0}, JsonWithNull
    ),
    ?assertEqual(#{field => nil}, ResultWithNull),

    JsonMissing = <<"{}">>,
    {ok, ResultMissing} = spectra:decode(json, ?MODULE, {type, map_with_nil_field, 0}, JsonMissing),
    ?assertEqual(#{field => nil}, ResultMissing).

list_with_null_elements_test() ->
    Json = <<"[\"hello\",null,\"world\"]">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {type, list_with_nil_elements, 0}, Json),
    ?assertEqual([<<"hello">>, nil, <<"world">>], Result).

multiple_union_types_null_test() ->
    Json = <<"null">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {type, multi_union_nil, 0}, Json),
    ?assertEqual(nil, Result).

multiple_union_types_integer_test() ->
    Json = <<"42">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {type, multi_union_nil, 0}, Json),
    ?assertEqual(42, Result).

multiple_union_types_binary_test() ->
    Json = <<"\"hello\"">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {type, multi_union_nil, 0}, Json),
    ?assertEqual(<<"hello">>, Result).

nested_map_with_null_test() ->
    Json = <<"{\"nested\":{\"inner_field\":null}}">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {type, outer_map_nil, 0}, Json),
    ?assertEqual(#{nested => #{inner_field => nil}}, Result).

optional_field_with_string_test() ->
    Json = <<"{\"field\":\"value\"}">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {type, map_with_nil_field, 0}, Json),
    ?assertEqual(#{field => <<"value">>}, Result).

record_nil_explicit_null_test() ->
    Json = <<"{\"name\":\"John\",\"email\":null}">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {record, record_with_nil}, Json),
    ?assertEqual(
        #record_with_nil{name = <<"John">>, email = nil},
        Result
    ).

record_nil_missing_field_test() ->
    Json = <<"{\"name\":\"John\"}">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {record, record_with_nil}, Json),
    ?assertEqual(
        #record_with_nil{name = <<"John">>, email = nil},
        Result
    ).

record_undefined_explicit_null_test() ->
    Json = <<"{\"name\":\"Jane\",\"email\":null}">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {record, record_with_undef}, Json),
    ?assertEqual(
        #record_with_undef{name = <<"Jane">>, email = undefined},
        Result
    ).

record_undefined_missing_field_test() ->
    Json = <<"{\"name\":\"Jane\"}">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {record, record_with_undef}, Json),
    ?assertEqual(
        #record_with_undef{name = <<"Jane">>, email = undefined},
        Result
    ).

integer_or_nil_null_test() ->
    Json = <<"null">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {type, maybe_nil_integer, 0}, Json),
    ?assertEqual(nil, Result).

integer_or_nil_integer_test() ->
    Json = <<"42">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {type, maybe_nil_integer, 0}, Json),
    ?assertEqual(42, Result).

map_multiple_fields_one_null_test() ->
    Json = <<"{\"name\":\"Alice\",\"email\":null}">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {type, map_with_multiple_fields, 0}, Json),
    ?assertEqual(#{name => <<"Alice">>, email => nil}, Result).

map_multiple_fields_one_missing_test() ->
    Json = <<"{\"name\":\"Bob\"}">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {type, map_with_multiple_fields, 0}, Json),
    ?assertEqual(#{name => <<"Bob">>, email => nil}, Result).

map_multiple_fields_all_present_test() ->
    Json = <<"{\"name\":\"Charlie\",\"email\":\"charlie@example.com\"}">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {type, map_with_multiple_fields, 0}, Json),
    ?assertEqual(#{name => <<"Charlie">>, email => <<"charlie@example.com">>}, Result).

integer_or_undefined_null_test() ->
    Json = <<"null">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {type, maybe_undef_integer, 0}, Json),
    ?assertEqual(undefined, Result).

integer_or_undefined_integer_test() ->
    Json = <<"99">>,
    {ok, Result} = spectra:decode(json, ?MODULE, {type, maybe_undef_integer, 0}, Json),
    ?assertEqual(99, Result).

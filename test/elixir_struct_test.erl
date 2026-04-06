-module(elixir_struct_test).

-include_lib("eunit/include/eunit.hrl").

-define(SKIP_IF_NO_ELIXIR(Body),
    case code:is_loaded('Elixir.TestUserStruct') of
        false ->
            case code:load_file('Elixir.TestUserStruct') of
                {error, _} -> ok;
                {module, _} -> Body
            end;
        {file, _} ->
            Body
    end
).

struct_type() ->
    TypeInfo = spectra_abstract_code:types_in_module(elixir_test_user_struct_type),
    spectra_type_info:get_type(TypeInfo, t, 0).

to_json_excludes_struct_field_test() ->
    ?SKIP_IF_NO_ELIXIR(run_to_json()).

run_to_json() ->
    StructData = maps:merge(
        'Elixir.TestUserStruct':'__struct__'(),
        #{name => <<"John">>, age => 30, email => <<"john@example.com">>}
    ),
    TypeInfo = spectra_type_info:new(?MODULE, false),
    {ok, JsonIoList} = spectra:encode(json, TypeInfo, struct_type(), StructData),
    Decoded = json:decode(iolist_to_binary(JsonIoList)),
    ?assertEqual(
        #{
            <<"name">> => <<"John">>,
            <<"age">> => 30,
            <<"email">> => <<"john@example.com">>,
            <<"score">> => 100
        },
        Decoded
    ).

from_json_adds_struct_field_test() ->
    ?SKIP_IF_NO_ELIXIR(run_from_json()).

run_from_json() ->
    JsonBinary = <<"{\"name\":\"John\",\"age\":30,\"email\":\"john@example.com\"}">>,
    TypeInfo = spectra_type_info:new(?MODULE, false),
    {ok, Result} = spectra:decode(json, TypeInfo, struct_type(), JsonBinary),
    ?assertEqual(
        #{
            '__struct__' => 'Elixir.TestUserStruct',
            name => <<"John">>,
            age => 30,
            email => <<"john@example.com">>,
            score => 100
        },
        Result
    ).

to_json_with_nil_email_test() ->
    ?SKIP_IF_NO_ELIXIR(run_to_json_nil_email()).

run_to_json_nil_email() ->
    StructData = maps:merge(
        'Elixir.TestUserStruct':'__struct__'(),
        #{name => <<"Jane">>, age => 25, email => nil}
    ),
    TypeInfo = spectra_type_info:new(?MODULE, false),
    {ok, JsonIoList} = spectra:encode(json, TypeInfo, struct_type(), StructData),
    Decoded = json:decode(iolist_to_binary(JsonIoList)),
    ?assertEqual(
        #{
            <<"name">> => <<"Jane">>,
            <<"age">> => 25,
            <<"score">> => 100
        },
        Decoded
    ).

from_json_with_missing_email_test() ->
    ?SKIP_IF_NO_ELIXIR(run_from_json_missing_email()).

run_from_json_missing_email() ->
    JsonBinary = <<"{\"name\":\"Jane\",\"age\":25}">>,
    TypeInfo = spectra_type_info:new(?MODULE, false),
    {ok, Result} = spectra:decode(json, TypeInfo, struct_type(), JsonBinary),
    ?assertEqual(
        #{
            '__struct__' => 'Elixir.TestUserStruct',
            name => <<"Jane">>,
            age => 25,
            email => nil,
            score => 100
        },
        Result
    ).

from_json_uses_struct_defaults_test() ->
    ?SKIP_IF_NO_ELIXIR(run_from_json_uses_struct_defaults()).

run_from_json_uses_struct_defaults() ->
    %% JSON with only name and age — email and score get struct defaults (nil and 100)
    JsonBinary = <<"{\"name\":\"Bob\",\"age\":42}">>,
    TypeInfo = spectra_type_info:new(?MODULE, false),
    {ok, Result} = spectra:decode(json, TypeInfo, struct_type(), JsonBinary),
    ?assertEqual(
        #{
            '__struct__' => 'Elixir.TestUserStruct',
            name => <<"Bob">>,
            age => 42,
            email => nil,
            score => 100
        },
        Result
    ).

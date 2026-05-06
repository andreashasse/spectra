-module(field_alias_test).

-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_unused_type).
-compile(nowarn_unused_record).

-spectra(#{field_aliases => #{last_name => <<"lastName">>, first_name => <<"firstName">>}}).
-record(person, {first_name :: binary(), last_name :: binary()}).

-spectra(#{field_aliases => #{last_name => <<"lastName">>}}).
-type person_map() :: #{last_name := binary()}.

-spectra(#{description => <<"A person">>, field_aliases => #{last_name => <<"lastName">>}}).
-type person_with_doc() :: #{last_name := binary()}.

record_encode_test() ->
    ?assertEqual(
        {ok, #{<<"firstName">> => <<"John">>, <<"lastName">> => <<"Smith">>}},
        spectra:encode(
            json,
            ?MODULE,
            {record, person},
            #person{first_name = <<"John">>, last_name = <<"Smith">>},
            [pre_encoded]
        )
    ).

record_decode_test() ->
    ?assertEqual(
        {ok, #person{first_name = <<"John">>, last_name = <<"Smith">>}},
        spectra:decode(
            json,
            ?MODULE,
            {record, person},
            #{<<"firstName">> => <<"John">>, <<"lastName">> => <<"Smith">>},
            [pre_decoded]
        )
    ).

record_roundtrip_test() ->
    Original = #person{first_name = <<"Jane">>, last_name = <<"Doe">>},
    {ok, Json} = spectra:encode(json, ?MODULE, {record, person}, Original, [pre_encoded]),
    ?assertEqual(
        {ok, Original}, spectra:decode(json, ?MODULE, {record, person}, Json, [pre_decoded])
    ).

record_decode_rejects_unaliased_name_test() ->
    ?assertMatch(
        {error, _},
        spectra:decode(
            json,
            ?MODULE,
            {record, person},
            #{<<"first_name">> => <<"John">>, <<"last_name">> => <<"Smith">>},
            [pre_decoded]
        )
    ).

map_encode_test() ->
    ?assertEqual(
        {ok, #{<<"lastName">> => <<"Smith">>}},
        spectra:encode(json, ?MODULE, {type, person_map, 0}, #{last_name => <<"Smith">>}, [
            pre_encoded
        ])
    ).

map_decode_test() ->
    ?assertEqual(
        {ok, #{last_name => <<"Smith">>}},
        spectra:decode(json, ?MODULE, {type, person_map, 0}, #{<<"lastName">> => <<"Smith">>}, [
            pre_decoded
        ])
    ).

map_roundtrip_test() ->
    Original = #{last_name => <<"Doe">>},
    {ok, Json} = spectra:encode(json, ?MODULE, {type, person_map, 0}, Original, [pre_encoded]),
    ?assertEqual(
        {ok, Original}, spectra:decode(json, ?MODULE, {type, person_map, 0}, Json, [pre_decoded])
    ).

combined_with_doc_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    {ok, Type} = spectra_type_info:find_type(TypeInfo, person_with_doc, 0),
    #{doc := Doc} = spectra_type:get_meta(Type),
    ?assertEqual(#{description => <<"A person">>}, Doc),
    ?assertEqual(
        {ok, #{<<"lastName">> => <<"Smith">>}},
        spectra:encode(json, ?MODULE, {type, person_with_doc, 0}, #{last_name => <<"Smith">>}, [
            pre_encoded
        ])
    ).

invalid_field_aliases_not_a_map_test() ->
    Code =
        "-module(bad_aliases_not_a_map).\n"
        "-compile(nowarn_unused_type).\n"
        "-spectra(#{field_aliases => not_a_map}).\n"
        "-type t() :: #{name := binary()}.\n",
    {ok, bad_aliases_not_a_map, BeamBinary} = spectra_test_compile:compile_module(Code),
    TempFile = spectra_test_compile:temp_beam_path("bad_aliases_not_a_map"),
    ok = file:write_file(TempFile, BeamBinary),
    ?assertError(
        {invalid_spectra_field, field_aliases, not_a_map},
        spectra_abstract_code:types_in_module_path(TempFile)
    ),
    file:delete(TempFile).

invalid_field_aliases_bad_value_test() ->
    Code =
        "-module(bad_aliases_bad_value).\n"
        "-compile(nowarn_unused_type).\n"
        "-spectra(#{field_aliases => #{name => not_a_binary}}).\n"
        "-type t() :: #{name := binary()}.\n",
    {ok, bad_aliases_bad_value, BeamBinary} = spectra_test_compile:compile_module(Code),
    TempFile = spectra_test_compile:temp_beam_path("bad_aliases_bad_value"),
    ok = file:write_file(TempFile, BeamBinary),
    ?assertError(
        {invalid_spectra_field, field_aliases, {name, not_a_binary}},
        spectra_abstract_code:types_in_module_path(TempFile)
    ),
    file:delete(TempFile).

schema_uses_aliased_names_test() ->
    SchemaJson = spectra:schema(json_schema, ?MODULE, {type, person_map, 0}),
    Schema = json:decode(iolist_to_binary(SchemaJson)),
    ?assertMatch(#{<<"properties">> := #{<<"lastName">> := _}}, Schema).

schema_record_uses_aliased_names_test() ->
    SchemaJson = spectra:schema(json_schema, ?MODULE, {record, person}),
    Schema = json:decode(iolist_to_binary(SchemaJson)),
    ?assertMatch(#{<<"properties">> := #{<<"firstName">> := _, <<"lastName">> := _}}, Schema).

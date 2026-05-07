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

-spectra(#{field_aliases => #{1 => <<"one">>}}).
-type int_key_map() :: #{1 => binary()}.

%% Union: alias applies to the map branch inside the union
-spectra(#{field_aliases => #{last_name => <<"lastName">>}}).
-type person_or_nil() :: #{last_name := binary()} | nil.

%% Type with variables: alias applies to the map inside the parameterized type.
%% Tested via a concrete instantiation (named_binary/0) which inherits the alias.
-spectra(#{field_aliases => #{name => <<"fullName">>}}).
-type named(T) :: #{name := T}.
-type named_binary() :: named(binary()).

%% Local type reference: alias applied at the alias site, not the definition site.
-type base_person() :: #{first_name := binary(), last_name := binary()} | undefined.

-spectra(#{field_aliases => #{first_name => <<"firstName">>}}).
-type aliased_person() :: base_person().

%% only through a local type reference
-spectra(#{only => [first_name]}).
-type only_local_ref() :: base_person().

%% only through a remote type reference
-spectra(#{only => [first_name]}).
-type only_remote_ref() :: field_alias_remote_type_a:t().

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

map_decode_rejects_unaliased_name_test() ->
    ?assertMatch(
        {error, _},
        spectra:decode(
            json,
            ?MODULE,
            {type, person_map, 0},
            #{<<"last_name">> => <<"Smith">>},
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
    try
        ?assertError(
            {invalid_spectra_field, field_aliases, not_a_map},
            spectra_abstract_code:types_in_module_path(TempFile)
        )
    after
        file:delete(TempFile)
    end.

invalid_field_aliases_bad_value_test() ->
    Code =
        "-module(bad_aliases_bad_value).\n"
        "-compile(nowarn_unused_type).\n"
        "-spectra(#{field_aliases => #{name => not_a_binary}}).\n"
        "-type t() :: #{name := binary()}.\n",
    {ok, bad_aliases_bad_value, BeamBinary} = spectra_test_compile:compile_module(Code),
    TempFile = spectra_test_compile:temp_beam_path("bad_aliases_bad_value"),
    ok = file:write_file(TempFile, BeamBinary),
    try
        ?assertError(
            {invalid_spectra_field, field_aliases, {name, not_a_binary}},
            spectra_abstract_code:types_in_module_path(TempFile)
        )
    after
        file:delete(TempFile)
    end.

duplicate_alias_in_map_type_test() ->
    Code =
        "-module(bad_aliases_dup_map).\n"
        "-compile(nowarn_unused_type).\n"
        "-spectra(#{field_aliases => #{first_name => <<\"name\">>, last_name => <<\"name\">>}}).\n"
        "-type t() :: #{first_name := binary(), last_name := binary()}.\n",
    {ok, bad_aliases_dup_map, BeamBinary} = spectra_test_compile:compile_module(Code),
    TempFile = spectra_test_compile:temp_beam_path("bad_aliases_dup_map"),
    ok = file:write_file(TempFile, BeamBinary),
    try
        ?assertError(
            {invalid_spectra_field, field_aliases, {duplicate_json_name, <<"name">>}},
            spectra_abstract_code:types_in_module_path(TempFile)
        )
    after
        file:delete(TempFile)
    end.

duplicate_alias_in_record_test() ->
    Code =
        "-module(bad_aliases_dup_rec).\n"
        "-compile(nowarn_unused_record).\n"
        "-spectra(#{field_aliases => #{first_name => <<\"name\">>, last_name => <<\"name\">>}}).\n"
        "-record(t, {first_name :: binary(), last_name :: binary()}).\n",
    {ok, bad_aliases_dup_rec, BeamBinary} = spectra_test_compile:compile_module(Code),
    TempFile = spectra_test_compile:temp_beam_path("bad_aliases_dup_rec"),
    ok = file:write_file(TempFile, BeamBinary),
    try
        ?assertError(
            {invalid_spectra_field, field_aliases, {duplicate_json_name, <<"name">>}},
            spectra_abstract_code:types_in_module_path(TempFile)
        )
    after
        file:delete(TempFile)
    end.

alias_collides_with_default_name_test() ->
    Code =
        "-module(bad_aliases_collide).\n"
        "-compile(nowarn_unused_type).\n"
        "-spectra(#{field_aliases => #{first_name => <<\"last_name\">>}}).\n"
        "-type t() :: #{first_name := binary(), last_name := binary()}.\n",
    {ok, bad_aliases_collide, BeamBinary} = spectra_test_compile:compile_module(Code),
    TempFile = spectra_test_compile:temp_beam_path("bad_aliases_collide"),
    ok = file:write_file(TempFile, BeamBinary),
    try
        ?assertError(
            {invalid_spectra_field, field_aliases, {duplicate_json_name, <<"last_name">>}},
            spectra_abstract_code:types_in_module_path(TempFile)
        )
    after
        file:delete(TempFile)
    end.

schema_uses_aliased_names_test() ->
    SchemaJson = spectra:schema(json_schema, ?MODULE, {type, person_map, 0}),
    Schema = json:decode(iolist_to_binary(SchemaJson)),
    ?assertMatch(#{<<"properties">> := #{<<"lastName">> := _}}, Schema).

schema_record_uses_aliased_names_test() ->
    SchemaJson = spectra:schema(json_schema, ?MODULE, {record, person}),
    Schema = json:decode(iolist_to_binary(SchemaJson)),
    ?assertMatch(#{<<"properties">> := #{<<"firstName">> := _, <<"lastName">> := _}}, Schema).

int_key_map_alias_encode_test() ->
    ?assertEqual(
        {ok, #{<<"one">> => <<"val">>}},
        spectra:encode(json, ?MODULE, {type, int_key_map, 0}, #{1 => <<"val">>}, [pre_encoded])
    ).

int_key_map_alias_decode_test() ->
    ?assertEqual(
        {ok, #{1 => <<"val">>}},
        spectra:decode(json, ?MODULE, {type, int_key_map, 0}, #{<<"one">> => <<"val">>}, [
            pre_decoded
        ])
    ).

int_key_map_alias_roundtrip_test() ->
    Original = #{1 => <<"val">>},
    {ok, Json} = spectra:encode(json, ?MODULE, {type, int_key_map, 0}, Original, [pre_encoded]),
    ?assertEqual(
        {ok, Original}, spectra:decode(json, ?MODULE, {type, int_key_map, 0}, Json, [pre_decoded])
    ).

union_map_branch_alias_encode_test() ->
    ?assertEqual(
        {ok, #{<<"lastName">> => <<"Smith">>}},
        spectra:encode(
            json, ?MODULE, {type, person_or_nil, 0}, #{last_name => <<"Smith">>}, [pre_encoded]
        )
    ).

union_map_branch_alias_decode_test() ->
    ?assertEqual(
        {ok, #{last_name => <<"Smith">>}},
        spectra:decode(
            json, ?MODULE, {type, person_or_nil, 0}, #{<<"lastName">> => <<"Smith">>}, [pre_decoded]
        )
    ).

type_with_variables_alias_encode_test() ->
    ?assertEqual(
        {ok, #{<<"fullName">> => <<"Alice">>}},
        spectra:encode(json, ?MODULE, {type, named_binary, 0}, #{name => <<"Alice">>}, [pre_encoded])
    ).

type_with_variables_alias_decode_test() ->
    ?assertEqual(
        {ok, #{name => <<"Alice">>}},
        spectra:decode(json, ?MODULE, {type, named_binary, 0}, #{<<"fullName">> => <<"Alice">>}, [
            pre_decoded
        ])
    ).

%% Remote type aliasing: field_aliases on a type that is defined in another module.
%% Module B aliases first_name => <<"firstName">> on a:t() which is #{first_name, last_name} | undefined.

remote_type_encode_test() ->
    ?assertEqual(
        {ok, #{<<"firstName">> => <<"Alice">>, <<"last_name">> => <<"Smith">>}},
        spectra:encode(
            json,
            field_alias_remote_type_b,
            {type, my_t, 0},
            #{first_name => <<"Alice">>, last_name => <<"Smith">>},
            [pre_encoded]
        )
    ).

remote_type_decode_test() ->
    ?assertEqual(
        {ok, #{first_name => <<"Alice">>, last_name => <<"Smith">>}},
        spectra:decode(
            json,
            field_alias_remote_type_b,
            {type, my_t, 0},
            #{<<"firstName">> => <<"Alice">>, <<"last_name">> => <<"Smith">>},
            [pre_decoded]
        )
    ).

remote_type_undefined_branch_test() ->
    ?assertEqual(
        {ok, undefined},
        spectra:decode(
            json,
            field_alias_remote_type_b,
            {type, my_t, 0},
            null,
            [pre_decoded]
        )
    ).

remote_type_roundtrip_test() ->
    Value = #{first_name => <<"Alice">>, last_name => <<"Smith">>},
    {ok, JsonIO} = spectra:encode(json, field_alias_remote_type_b, {type, my_t, 0}, Value),
    Json = iolist_to_binary(JsonIO),
    ?assertEqual({ok, Value}, spectra:decode(json, field_alias_remote_type_b, {type, my_t, 0}, Json)).

remote_type_schema_test() ->
    Schema = spectra:schema(
        json_schema, field_alias_remote_type_b, {type, my_t, 0}, [pre_encoded]
    ),
    %% Locate the object branch (may be top-level or inside oneOf/anyOf).
    %% pre_encoded returns Erlang maps with atom keys.
    MapBranch =
        case Schema of
            #{oneOf := Branches} ->
                hd([B || B = #{type := <<"object">>} <- Branches]);
            #{type := <<"object">>} ->
                Schema
        end,
    #{properties := Props} = MapBranch,
    ?assert(maps:is_key(<<"firstName">>, Props)),
    ?assert(maps:is_key(<<"last_name">>, Props)),
    ?assertNot(maps:is_key(<<"first_name">>, Props)).

%% Local type reference aliasing: alias applied in the same module at the alias site.

local_type_ref_encode_test() ->
    ?assertEqual(
        {ok, #{<<"firstName">> => <<"Alice">>, <<"last_name">> => <<"Smith">>}},
        spectra:encode(
            json, ?MODULE, {type, aliased_person, 0},
            #{first_name => <<"Alice">>, last_name => <<"Smith">>},
            [pre_encoded]
        )
    ).

local_type_ref_decode_test() ->
    ?assertEqual(
        {ok, #{first_name => <<"Alice">>, last_name => <<"Smith">>}},
        spectra:decode(
            json, ?MODULE, {type, aliased_person, 0},
            #{<<"firstName">> => <<"Alice">>, <<"last_name">> => <<"Smith">>},
            [pre_decoded]
        )
    ).

local_type_ref_undefined_branch_test() ->
    ?assertEqual(
        {ok, undefined},
        spectra:decode(
            json, ?MODULE, {type, aliased_person, 0}, null, [pre_decoded]
        )
    ).

local_type_ref_roundtrip_test() ->
    Value = #{first_name => <<"Alice">>, last_name => <<"Smith">>},
    {ok, JsonIO} = spectra:encode(json, ?MODULE, {type, aliased_person, 0}, Value),
    Json = iolist_to_binary(JsonIO),
    ?assertEqual({ok, Value}, spectra:decode(json, ?MODULE, {type, aliased_person, 0}, Json)).

%% only through a local type reference

only_local_ref_encode_test() ->
    ?assertEqual(
        {ok, #{<<"first_name">> => <<"Alice">>}},
        spectra:encode(
            json, ?MODULE, {type, only_local_ref, 0},
            #{first_name => <<"Alice">>, last_name => <<"Smith">>},
            [pre_encoded]
        )
    ).

only_local_ref_decode_test() ->
    ?assertEqual(
        {ok, #{first_name => <<"Alice">>}},
        spectra:decode(
            json, ?MODULE, {type, only_local_ref, 0},
            #{<<"first_name">> => <<"Alice">>},
            [pre_decoded]
        )
    ).

%% only through a remote type reference

only_remote_ref_encode_test() ->
    ?assertEqual(
        {ok, #{<<"first_name">> => <<"Alice">>}},
        spectra:encode(
            json, ?MODULE, {type, only_remote_ref, 0},
            #{first_name => <<"Alice">>, last_name => <<"Smith">>},
            [pre_encoded]
        )
    ).

only_remote_ref_decode_test() ->
    ?assertEqual(
        {ok, #{first_name => <<"Alice">>}},
        spectra:decode(
            json, ?MODULE, {type, only_remote_ref, 0},
            #{<<"first_name">> => <<"Alice">>},
            [pre_decoded]
        )
    ).

-module(record_test).

-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_unused_type).

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-record(person, {name :: string(), age = 1 :: pos_integer()}).

-type person_alias() :: #person{name :: string(), age :: pos_integer()}.

missing_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    %% arity
    {ok, PersonRecord} = spectra_type_info:find_record(TypeInfo, person),
    ?assertEqual(
        #sp_rec{
            name = person,
            fields =
                [
                    #sp_rec_field{
                        name = name, binary_name = <<"name">>, type = #sp_simple_type{type = string}
                    },
                    #sp_rec_field{
                        name = age,
                        binary_name = <<"age">>,
                        type = #sp_simple_type{type = pos_integer},
                        default = {value, 1}
                    }
                ],
            arity = 3,
            meta = #{name => {record, person}}
        },
        PersonRecord
    ),
    ?assertEqual(
        {ok, #{<<"name">> => <<"John">>, <<"age">> => 1}},
        spectra:encode(json, ?MODULE, {record, person}, #person{name = "John"}, [pre_encoded]),
        "Default value for age picked up when constructing the record, no change needed for to_json"
    ),
    ?assertMatch(
        {error, [#sp_error{location = [age], type = missing_data}]},
        spectra:decode(json, ?MODULE, {record, person}, #{<<"name">> => <<"John">>}, [pre_decoded]),
        "Included fields are mandatory regardless of record default"
    ).

extra_fields_test() ->
    ?assertMatch(
        {ok, #person{name = "John", age = 30}},
        spectra:decode(
            json,
            ?MODULE,
            {record, person},
            #{
                <<"name">> => <<"John">>, <<"age">> => 30, <<"extra">> => <<"field">>
            },
            [pre_decoded]
        ),
        "Extra fields in JSON should be ignored"
    ),
    ?assertMatch(
        {ok, #person{name = "John", age = 30}},
        spectra:decode(
            json,
            ?MODULE,
            {record, person},
            #{
                <<"name">> => <<"John">>,
                <<"age">> => 30,
                <<"extra1">> => <<"value1">>,
                <<"extra2">> => <<"value2">>
            },
            [pre_decoded]
        ),
        "Multiple extra fields in JSON should be ignored"
    ).

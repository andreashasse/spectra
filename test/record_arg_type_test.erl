-module(record_arg_type_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-record(result, {value, errors = [] :: [atom()]}).

-type int_result() :: result_t(atom()).
-type result_t(ResultType) :: #result{value :: integer(), errors :: [ResultType]}.

type_in_form_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    {ok, IntResultType} = spectra_type_info:find_type(TypeInfo, int_result, 0),
    ?assertEqual(
        #sp_user_type_ref{
            type_name = result_t,
            variables = [#sp_simple_type{type = atom}]
        },
        IntResultType
    ),
    {ok, ResultTType} = spectra_type_info:find_type(TypeInfo, result_t, 1),
    ?assertEqual(
        #sp_type_with_variables{
            type =
                #sp_rec_ref{
                    record_name = result,
                    field_types =
                        [
                            {value, #sp_simple_type{type = integer}},
                            {errors, #sp_list{
                                type =
                                    #sp_var{
                                        name =
                                            'ResultType'
                                    }
                            }}
                        ]
                },
            vars = ['ResultType']
        },
        ResultTType
    ).

map1_to_json_test() ->
    ?assertEqual(
        {ok, #{<<"value">> => 1, <<"errors">> => []}},
        to_json_result_1(#result{value = 1, errors = []})
    ),
    ?assertEqual(
        {error, [
            sp_error:append_location(
                sp_error:type_mismatch(#sp_simple_type{type = integer}, pelle),
                value
            )
        ]},
        to_json_result_1(#result{value = pelle, errors = []})
    ).

map1_from_json_test() ->
    ?assertEqual(
        {ok, #result{value = 1, errors = []}},
        from_json_result_1(#{<<"value">> => 1, <<"errors">> => []})
    ),
    ?assertEqual(
        {error, [
            sp_error:append_location(
                sp_error:type_mismatch(#sp_simple_type{type = integer}, <<"hej">>),
                value
            )
        ]},
        from_json_result_1(#{<<"value">> => <<"hej">>, <<"errors">> => []})
    ).

record_to_json_schema_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {type, int_result, 0}),
    ?assertEqual(
        #{
            <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"object">>,
            properties =>
                #{
                    <<"value">> => #{type => <<"integer">>},
                    <<"errors">> => #{type => <<"array">>, items => #{type => <<"string">>}}
                },
            required => [<<"value">>, <<"errors">>]
        },
        Schema
    ),
    json_schema_validator_helper:validate_schema_2020_12(Schema).

-spec from_json_result_1(term()) -> int_result().
from_json_result_1(Data) ->
    spectra_json:from_json(?MODULE, {type, int_result, 0}, Data).

-spec to_json_result_1(int_result()) -> json:encode_value().
to_json_result_1(Data) ->
    spectra_json:to_json(?MODULE, {type, int_result, 0}, Data).

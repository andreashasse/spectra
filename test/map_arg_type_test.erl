-module(map_arg_type_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-type int_result() :: result(integer()).
%-type map_result_2() :: result(#{atom() => integer()}, atom()).
-type result(ResultType) :: #{result => ResultType, errors => [atom()]}.

type_in_form_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    IntResultType = spectra_type_info:get_type(TypeInfo, int_result, 0),
    ?assertEqual(
        #sp_user_type_ref{
            type_name = result,
            variables = [#sp_simple_type{type = integer}]
        },
        IntResultType
    ),

    ResultType = spectra_type_info:get_type(TypeInfo, result, 1),
    ?assertEqual(
        #sp_type_with_variables{
            type =
                #sp_map{
                    fields =
                        [
                            #literal_map_field{
                                kind = assoc,
                                name = result,
                                binary_name = <<"result">>,
                                val_type = #sp_var{name = 'ResultType'}
                            },
                            #literal_map_field{
                                kind = assoc,
                                name = errors,
                                binary_name = <<"errors">>,
                                val_type = #sp_list{
                                    type =
                                        #sp_simple_type{
                                            type =
                                                atom
                                        }
                                }
                            }
                        ]
                },
            vars = ['ResultType']
        },
        ResultType
    ).

map_to_json_test() ->
    ?assertEqual(
        {ok, #{<<"result">> => 1, <<"errors">> => []}},
        to_json_result_1(#{result => 1, errors => []})
    ),
    ?assertEqual(
        {error, [
            sp_error:append_location(
                sp_error:type_mismatch(#sp_simple_type{type = integer}, pelle),
                result
            )
        ]},
        to_json_result_1(#{result => pelle, errors => []})
    ).

map_from_json_test() ->
    ?assertEqual(
        {ok, #{result => 1, errors => []}},
        from_json_result_1(#{<<"result">> => 1, <<"errors">> => []})
    ),
    ?assertEqual(
        {error, [
            sp_error:append_location(
                sp_error:type_mismatch(#sp_simple_type{type = integer}, <<"hej">>),
                result
            )
        ]},
        from_json_result_1(#{<<"result">> => <<"hej">>, <<"errors">> => []})
    ).

map_to_json_schema_test() ->
    Schema = spectra_json_schema:to_schema(?MODULE, {type, int_result, 0}),
    ?assertEqual(
        #{
            '$schema' => <<"https://json-schema.org/draft/2020-12/schema">>,
            type => <<"object">>,
            properties =>
                #{
                    <<"result">> => #{type => <<"integer">>},
                    <<"errors">> => #{
                        type => <<"array">>, items => #{type => <<"string">>}
                    }
                },
            additionalProperties => false
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

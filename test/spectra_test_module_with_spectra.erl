-module(spectra_test_module_with_spectra).

-include("../include/spectra_internal.hrl").

-export(['__spectra_type_info__'/0]).

-spec '__spectra_type_info__'() -> spectra:type_info().
'__spectra_type_info__'() ->
    #type_info{
        types = #{
            {my_type, 0} => #sp_simple_type{
                type = string,
                meta = #{
                    doc => #{
                        title => <<"My Type">>,
                        description => <<"A test type">>
                    }
                }
            }
        },
        records = #{
            my_record => #sp_rec{
                name = my_record,
                fields = [
                    #sp_rec_field{
                        name = field1,
                        binary_name = <<"field1">>,
                        type = #sp_simple_type{type = integer}
                    },
                    #sp_rec_field{
                        name = field2,
                        binary_name = <<"field2">>,
                        type = #sp_simple_type{type = string}
                    }
                ],
                arity = 3,
                meta = #{
                    doc => #{
                        title => <<"My Record">>,
                        description => <<"A test record">>
                    }
                }
            }
        },
        functions = #{
            {my_function, 1} => [
                #sp_function_spec{
                    args = [#sp_simple_type{type = integer}],
                    return = #sp_simple_type{type = string}
                }
            ]
        }
    }.

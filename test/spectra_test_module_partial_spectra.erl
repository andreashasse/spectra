-module(spectra_test_module_partial_spectra).

-include("../include/spectra_internal.hrl").

-export(['__spectra_type_info__'/0]).

-spec '__spectra_type_info__'() -> spectra:type_info().
'__spectra_type_info__'() ->
    #type_info{
        types = #{
            {partial_type, 0} => #sp_simple_type{type = atom}
        }
    }.

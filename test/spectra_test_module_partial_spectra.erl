-module(spectra_test_module_partial_spectra).

-include("../include/spectra_internal.hrl").

-export(['__spectra__'/0]).

-spec '__spectra__'() -> spectra:type_info().
'__spectra__'() ->
    #type_info{
        types = #{
            {partial_type, 0} => #sp_simple_type{type = atom}
        }
    }.

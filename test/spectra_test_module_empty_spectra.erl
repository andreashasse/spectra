-module(spectra_test_module_empty_spectra).

-include("../include/spectra_internal.hrl").

-export(['__spectra__'/0]).

-spec '__spectra__'() -> spectra:type_info().
'__spectra__'() ->
    #type_info{}.

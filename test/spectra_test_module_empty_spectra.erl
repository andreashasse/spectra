-module(spectra_test_module_empty_spectra).

-export(['__spectra__'/0]).

%% This module has a __spectra__/0 function that returns an empty map
%% All fields should default to empty maps
%% NOTE: We don't include spectra_internal.hrl to avoid polluting the module with record definitions

-spec '__spectra__'() -> map().
'__spectra__'() ->
    #{}.

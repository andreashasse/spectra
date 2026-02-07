-module(spectra_test_module_empty_spectra).

-include("../include/spectra_internal.hrl").

-export(['__spectra__'/0]).

%% This module has a __spectra__/0 function that returns an empty type_info record
%% All fields use their default values (empty maps)

-spec '__spectra__'() -> spectra:type_info().
'__spectra__'() ->
    #type_info{}.

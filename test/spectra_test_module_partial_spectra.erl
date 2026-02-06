-module(spectra_test_module_partial_spectra).

-include("../include/spectra_internal.hrl").

-export(['__spectra__'/0]).

%% This module has a __spectra__/0 function that returns partial type info
%% Only types field is populated, others should default to empty maps

-spec '__spectra__'() -> map().
'__spectra__'() ->
    #{
        types => #{
            {partial_type, 0} => #sp_simple_type{type = atom}
        }
        %% Note: record, functions, docs, and record_docs are not included
        %% They should default to empty maps in the type_info record
    }.

-module(spectra_test_module_partial_spectra).

-include("../include/spectra_internal.hrl").

-export(['__spectra__'/0]).

%% This module has a __spectra__/0 function that returns partial type info
%% Only types field is populated, others should default to empty maps

-spec '__spectra__'() -> spectra:type_info().
'__spectra__'() ->
    #type_info{
        types = #{
            {partial_type, 0} => #sp_simple_type{type = atom}
        }
        %% Note: records, functions, docs, and record_docs use default values (empty maps)
    }.

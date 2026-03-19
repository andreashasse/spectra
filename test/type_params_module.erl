-module(type_params_module).

%% Module defining types with type_parameters for testing the feature.
%% The codec type_params_codec is registered via application env in each test.

-compile([nowarn_unused_record]).

-spectra(#{type_parameters => <<"^[a-z]+$">>}).
-type parameterized_type() :: binary().

-spectra(#{}).
-type no_params_type() :: binary().

-spectra(#{type_parameters => <<"^[a-z]+$">>}).
-record(parameterized_rec, {value :: binary()}).

-export_type([parameterized_type/0, no_params_type/0]).

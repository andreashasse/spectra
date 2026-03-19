-module(type_params_remote_consumer).

%% A module that references type_params_module:parameterized_type() as a remote
%% type, exercising that parameters travel from the defining module (AC5).
-type uses_remote() :: type_params_module:parameterized_type().

-export_type([uses_remote/0]).

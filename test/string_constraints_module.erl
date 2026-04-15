-module(string_constraints_module).

%% Types used to test structural string/binary constraint validation
%% and JSON Schema generation via type_parameters.

-compile([nowarn_unused_type]).

%% binary() with a regex pattern
-spectra(#{type_parameters => #{pattern => <<"^[a-z]+$">>}}).
-type lowercase_binary() :: binary().

%% binary() with min and max length
-spectra(#{type_parameters => #{min_length => 2, max_length => 5}}).
-type bounded_binary() :: binary().

%% string() with a pattern
-spectra(#{type_parameters => #{pattern => <<"^\\d+$">>}}).
-type digit_string() :: string().

%% binary() with format metadata (schema only, no runtime validation)
-spectra(#{type_parameters => #{format => <<"date">>}}).
-type date_binary() :: binary().

%% nonempty_binary() with an additional min_length (overrides baseline of 1)
-spectra(#{type_parameters => #{min_length => 3}}).
-type long_nonempty() :: nonempty_binary().

%% binary() with all constraint keys together
-spectra(#{
    type_parameters => #{
        pattern => <<"^[a-z]+$">>, min_length => 2, max_length => 10, format => <<"identifier">>
    }
}).
-type full_constraints() :: binary().

%% Remote type aliases — constraints must survive remote-type resolution (String.t() pattern)
-spectra(#{type_parameters => #{min_length => 2, max_length => 5}}).
-type bounded_remote() :: string_alias_module:t().

-spectra(#{type_parameters => #{pattern => <<"^[a-z]+$">>}}).
-type pattern_remote() :: string_alias_module:t().

%% binary() testing unicode behavior in regex
-spectra(#{type_parameters => #{pattern => <<"^.$">>}}).
-type single_char_binary() :: binary().

%% binary() testing unicode character properties behavior in regex (\w matches unicode letters)
-spectra(#{type_parameters => #{pattern => <<"^\\w+$">>}}).
-type ucp_word_binary() :: binary().

%% binary() with an invalid/unbalanced regex pattern (developer error; raises {invalid_string_pattern, _, _})
-spectra(#{type_parameters => #{pattern => <<"[invalid">>}}).
-type invalid_pattern_binary() :: binary().

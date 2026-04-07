-module(elixir_test_user_struct_extra_field_type).

-export_type([t/0]).

%% Same struct as TestUserStruct but declares 'address', which does not exist
%% in Elixir.TestUserStruct.__struct__/0. Used to test that a field missing from
%% both the JSON and the struct defaults errors gracefully instead of crashing.
-type t() :: #{
    '__struct__' := 'Elixir.TestUserStruct',
    name := binary(),
    age := non_neg_integer(),
    email := binary() | 'nil',
    score := non_neg_integer(),
    address := binary()
}.

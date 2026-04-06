-module(elixir_test_user_struct_type).

-export_type([t/0]).

%% Erlang-compatible type definition for Elixir.TestUserStruct.
%% Used in tests via spectra_abstract_code:types_in_module/1 to avoid
%% hardcoding internal #sp_map{} records directly in test assertions.
-type t() :: #{
    '__struct__' := 'Elixir.TestUserStruct',
    name := binary(),
    age := non_neg_integer(),
    email := binary() | 'nil',
    score := non_neg_integer()
}.

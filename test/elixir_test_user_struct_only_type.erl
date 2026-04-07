-module(elixir_test_user_struct_only_type).

-export_type([t/0, t_or_nil/0]).

%% Erlang-compatible type for Elixir.TestUserStruct with only [name, age] exposed.
-spectra(#{only => [name, age]}).
-type t() :: #{
    '__struct__' := 'Elixir.TestUserStruct',
    name := binary(),
    age := non_neg_integer(),
    email := binary() | 'nil',
    score := non_neg_integer()
}.

%% Same but in a union with nil, to test that apply_only propagates through #sp_union{}.
-spectra(#{only => [name, age]}).
-type t_or_nil() ::
    #{
        '__struct__' := 'Elixir.TestUserStruct',
        name := binary(),
        age := non_neg_integer(),
        email := binary() | 'nil',
        score := non_neg_integer()
    }
    | 'nil'.

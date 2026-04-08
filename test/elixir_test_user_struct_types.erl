-module(elixir_test_user_struct_types).

-export_type([t/0, t_only/0, t_or_nil/0, t_with_extra_field/0]).

%% Erlang-compatible type definition for Elixir.TestUserStruct — all fields.
-type t() :: #{
    '__struct__' := 'Elixir.TestUserStruct',
    name := binary(),
    age := non_neg_integer(),
    email := binary() | 'nil',
    score := non_neg_integer()
}.

%% Same struct with only [name, age] exposed.
-spectra(#{only => [name, age]}).
-type t_only() :: #{
    '__struct__' := 'Elixir.TestUserStruct',
    name := binary(),
    age := non_neg_integer(),
    email := binary() | 'nil',
    score := non_neg_integer()
}.

%% Same as t_only but in a union with nil — tests apply_only through #sp_union{}.
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

%% Declares 'address' which does not exist in Elixir.TestUserStruct.__struct__/0.
%% Used to test that a missing field absent from struct defaults errors gracefully.
-type t_with_extra_field() :: #{
    '__struct__' := 'Elixir.TestUserStruct',
    name := binary(),
    age := non_neg_integer(),
    email := binary() | 'nil',
    score := non_neg_integer(),
    address := binary()
}.

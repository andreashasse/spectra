-module(struct_shaped_map_types).

-export_type([t/0, list_of_t/0]).

%% A map type with a literal '__struct__' field, defined in plain Erlang.
%% spectra_abstract_code:extract_struct_name/1 recognizes this shape purely
%% from the type's abstract syntax -- it has nothing to do with Elixir, and
%% no Elixir module or runtime is involved anywhere in this file or in the
%% test that uses it.
-type t() :: #{
    '__struct__' := erlang_only_struct,
    name := binary(),
    age := integer()
}.

-type list_of_t() :: [t()].

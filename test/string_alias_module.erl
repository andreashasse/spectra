-module(string_alias_module).

%% A minimal module that mimics Elixir's String.t() — a remote binary() alias.
%% Used to test that string constraints survive remote-type resolution.

-type t() :: binary().

-export_type([t/0]).

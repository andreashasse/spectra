%% Helper module for spectra_attr_interaction_test.
%% Defines base types with definition-site -spectra() attributes.
-module(spectra_attr_interaction_base).

-export_type([t/0, t_only/0]).

%% t/0: first_name aliased to "firstName" at definition site.
-spectra(#{field_aliases => #{first_name => <<"firstName">>}}).
-type t() :: #{first_name := binary(), last_name := binary(), email := binary()}.

%% t_only/0: only [first_name, last_name] exposed at definition site (email excluded).
%% Also aliases first_name → "firstName".
-spectra(#{
    only => [first_name, last_name],
    field_aliases => #{first_name => <<"firstName">>}
}).
-type t_only() :: #{first_name := binary(), last_name := binary(), email := binary()}.

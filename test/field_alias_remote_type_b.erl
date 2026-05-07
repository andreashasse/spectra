-module(field_alias_remote_type_b).

-export_type([my_t/0]).

-spectra(#{field_aliases => #{first_name => <<"firstName">>}}).
-type my_t() :: field_alias_remote_type_a:t().

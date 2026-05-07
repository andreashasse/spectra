-module(field_alias_remote_type_a).

-export_type([t/0]).

-type t() :: #{first_name := binary(), last_name := binary()} | undefined.

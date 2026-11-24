-module(reserved_struct_key_types).

-export_type([atom_to_term/0, atom_to_atom/0, atom_key_with_literal/0]).

-type atom_to_term() :: #{atom() => term()}.
-type atom_to_atom() :: #{atom() => atom()}.
-type atom_key_with_literal() :: #{name := binary(), atom() => binary()}.

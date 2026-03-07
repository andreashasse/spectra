-module(codec_tuple_module).

%% Module with a tuple type and NO codec — tests that existing throw behaviour is preserved.
-type pair() :: {integer(), integer()}.

-export_type([pair/0]).

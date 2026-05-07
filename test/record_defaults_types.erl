-module(record_defaults_types).

-record(rec_with_defaults, {
    name :: binary(),
    status = active :: atom(),
    count = 0 :: integer(),
    opt = undefined :: undefined | binary(),
    items = [] :: [atom()]
}).

-export_type([rec_with_defaults/0]).
-type rec_with_defaults() :: #rec_with_defaults{}.

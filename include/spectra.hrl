-record(sp_error, {
    location :: [atom() | integer()],
    type :: term(),
    msg :: binary() | undefined,
    input :: term(),
    ctx :: map(),
    url :: binary() | undefined
}).

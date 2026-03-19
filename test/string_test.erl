-module(string_test).

-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_unused_type).

-type my_string() :: string().

string_test() ->
    %% not printable string
    %% FIXME: match more specific
    ?assertMatch(
        {error, _},
        spectra:encode(json, ?MODULE, {type, my_string, 0}, [1655379, 100, 210, 81], [pre_encoded])
    ),
    ?assertMatch(
        {error, _},
        spectra:decode(json, ?MODULE, my_string, <<255, 100, 210, 81>>, [
            pre_decoded
        ])
    ).

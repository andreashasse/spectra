-module(spectra_calendar_codec).

-doc """
Built-in codec for `calendar:datetime()` and `calendar:date()`.

Serialises to ISO 8601 strings and parses them back:
- `calendar:datetime()` ↔ `"YYYY-MM-DDTHH:MM:SSZ"` (e.g. `"2024-01-15T10:30:00Z"`)
- `calendar:date()` ↔ `"YYYY-MM-DD"` (e.g. `"2024-01-15"`)

`calendar:datetime()` carries no timezone information — values are treated as
UTC. Encoding always appends `Z`; decoding requires the `Z` suffix and rejects
any other offset (e.g. `+01:00`).

If you need full timezone support, use a dedicated datetime library (e.g.
[`qdate`](https://hex.pm/packages/qdate) or
[`calendar_extended`](https://hex.pm/packages/calendar_extended)) and implement
a custom `spectra_codec` behaviour for it.

## Registering

Not active by default. Add to the application environment:

```erlang
{spectra, [
    {codecs, #{
        {calendar, {type, datetime, 0}} => spectra_calendar_codec,
        {calendar, {type, date, 0}} => spectra_calendar_codec
    }}
]}
```

Register only the types you use — registering both is fine, and each is
independent.

## Example

```erlang
-type meeting() :: #{title => binary(), at => calendar:datetime()}.

DT = {{2024, 1, 15}, {10, 30, 0}},
{ok, Encoded} = spectra:encode(json, my_module, meeting, #{title => <<"Standup">>, at => DT}).
%% => {ok, <<"{\"title\":\"Standup\",\"at\":\"2024-01-15T10:30:00Z\"}">>}

{ok, Decoded} = spectra:decode(json, my_module, meeting, Encoded).
%% => {ok, #{title => <<"Standup">>, at => {{2024,1,15},{10,30,0}}}}
```
""".

-behaviour(spectra_codec).

-export([encode/7, decode/7, schema/6]).

-spec encode(
    atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type(), term(),
    spectra:sp_config()
) ->
    spectra:codec_encode_result().
encode(json, _Mod, {type, datetime, 0}, {{Y, Mo, D}, {H, Mi, S}}, _SpType, _Params, _Config) when
    is_integer(Y),
    is_integer(Mo),
    is_integer(D),
    is_integer(H),
    is_integer(Mi),
    is_integer(S)
->
    Bin = iolist_to_binary(
        io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ", [Y, Mo, D, H, Mi, S])
    ),
    {ok, Bin};
encode(json, _Mod, {type, datetime, 0} = TypeRef, Data, _SpType, _Params, _Config) ->
    {error, [sp_error:type_mismatch(TypeRef, Data)]};
encode(json, _Mod, {type, date, 0}, {Y, Mo, D}, _SpType, _Params, _Config) when
    is_integer(Y), is_integer(Mo), is_integer(D)
->
    Bin = iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0w", [Y, Mo, D])),
    {ok, Bin};
encode(json, _Mod, {type, date, 0} = TypeRef, Data, _SpType, _Params, _Config) ->
    {error, [sp_error:type_mismatch(TypeRef, Data)]}.

-spec decode(
    atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type(), term(),
    spectra:sp_config()
) ->
    spectra:codec_decode_result().
decode(json, _Mod, {type, datetime, 0}, Bin, _SpType, _Params, _Config) when is_binary(Bin) ->
    case parse_datetime(Bin) of
        {ok, DT} -> {ok, DT};
        error -> {error, [sp_error:type_mismatch({type, datetime, 0}, Bin)]}
    end;
decode(json, _Mod, {type, datetime, 0} = TypeRef, Data, _SpType, _Params, _Config) ->
    {error, [sp_error:type_mismatch(TypeRef, Data)]};
decode(json, _Mod, {type, date, 0}, Bin, _SpType, _Params, _Config) when is_binary(Bin) ->
    case parse_date(Bin) of
        {ok, D} -> {ok, D};
        error -> {error, [sp_error:type_mismatch({type, date, 0}, Bin)]}
    end;
decode(json, _Mod, {type, date, 0} = TypeRef, Data, _SpType, _Params, _Config) ->
    {error, [sp_error:type_mismatch(TypeRef, Data)]}.

-spec schema(
    atom(), module(), spectra:sp_type_reference(), spectra:sp_type(), term(), spectra:sp_config()
) ->
    dynamic().
schema(json_schema, _Mod, {type, datetime, 0}, _SpType, _Params, _Config) ->
    #{type => <<"string">>, format => <<"date-time">>};
schema(json_schema, _Mod, {type, date, 0}, _SpType, _Params, _Config) ->
    #{type => <<"string">>, format => <<"date">>}.

%% Internal helpers

-spec parse_datetime(binary()) -> {ok, calendar:datetime()} | error.
parse_datetime(
    <<Y1, Y2, Y3, Y4, $-, Mo1, Mo2, $-, D1, D2, $T, H1, H2, $:, Mi1, Mi2, $:, S1, S2, $Z>>
) ->
    try
        Date = {
            list_to_integer([Y1, Y2, Y3, Y4]),
            list_to_integer([Mo1, Mo2]),
            list_to_integer([D1, D2])
        },
        Time = {
            list_to_integer([H1, H2]),
            list_to_integer([Mi1, Mi2]),
            list_to_integer([S1, S2])
        },
        case calendar:valid_date(Date) andalso valid_time(Time) of
            true -> {ok, {Date, Time}};
            false -> error
        end
    catch
        error:badarg -> error
    end;
parse_datetime(_) ->
    error.

-spec parse_date(binary()) -> {ok, calendar:date()} | error.
parse_date(<<Y1, Y2, Y3, Y4, $-, Mo1, Mo2, $-, D1, D2>>) ->
    try
        Date = {
            list_to_integer([Y1, Y2, Y3, Y4]),
            list_to_integer([Mo1, Mo2]),
            list_to_integer([D1, D2])
        },
        case calendar:valid_date(Date) of
            true -> {ok, Date};
            false -> error
        end
    catch
        error:badarg -> error
    end;
parse_date(_) ->
    error.

-spec valid_time(calendar:time()) -> boolean().
valid_time({H, Mi, S}) ->
    H >= 0 andalso H =< 23 andalso
        Mi >= 0 andalso Mi =< 59 andalso
        S >= 0 andalso S =< 59.

-module(codec_calendar_codec).

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-export([encode/6, decode/6, schema/5]).

-spec encode(atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type(), term()) ->
    spectra:codec_encode_result().
encode(_, _Mod, {type, datetime, 0}, {{Y, Mo, D}, {H, Mi, S}}, _SpType, _Params) ->
    Bin = iolist_to_binary(
        io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w", [Y, Mo, D, H, Mi, S])
    ),
    {ok, Bin};
encode(_, _Mod, {type, datetime, 0}, Data, _SpType, _Params) ->
    {error, [sp_error:type_mismatch({type, datetime, 0}, Data)]}.

-spec decode(atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type(), term()) ->
    spectra:codec_decode_result().
decode(_, _Mod, {type, datetime, 0}, Bin, _SpType, _Params) when is_binary(Bin) ->
    case parse_datetime(Bin) of
        {ok, DT} -> {ok, DT};
        error -> {error, [sp_error:type_mismatch({type, datetime, 0}, Bin)]}
    end;
decode(_, _Mod, {type, datetime, 0}, Data, _SpType, _Params) ->
    {error, [sp_error:type_mismatch({type, datetime, 0}, Data)]}.

-spec schema(atom(), module(), spectra:sp_type_reference(), spectra:sp_type(), term()) -> map().
schema(json_schema, _Mod, {type, datetime, 0}, _SpType, _Params) ->
    #{type => <<"string">>, format => <<"date-time">>}.

parse_datetime(<<Y1, Y2, Y3, Y4, $-, Mo1, Mo2, $-, D1, D2, $T, H1, H2, $:, Mi1, Mi2, $:, S1, S2>>) ->
    Y = list_to_integer([Y1, Y2, Y3, Y4]),
    Mo = list_to_integer([Mo1, Mo2]),
    D = list_to_integer([D1, D2]),
    H = list_to_integer([H1, H2]),
    Mi = list_to_integer([Mi1, Mi2]),
    S = list_to_integer([S1, S2]),
    {ok, {{Y, Mo, D}, {H, Mi, S}}};
parse_datetime(_) ->
    error.

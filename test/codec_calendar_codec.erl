-module(codec_calendar_codec).

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-export([encode/4, decode/4, schema/3]).

-spec encode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    {ok, term()} | {error, [spectra:error()]}.
encode(_, {type, datetime, 0}, {{Y, Mo, D}, {H, Mi, S}}, _Opts) ->
    Bin = iolist_to_binary(
        io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w", [Y, Mo, D, H, Mi, S])
    ),
    {ok, Bin};
encode(_, TypeRef, Data, _Opts) ->
    {error, [sp_error:type_mismatch(TypeRef, Data)]}.

-spec decode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
decode(_, {type, datetime, 0}, Bin, _Opts) when is_binary(Bin) ->
    case parse_datetime(Bin) of
        {ok, DT} -> {ok, DT};
        error -> {error, [sp_error:type_mismatch({type, datetime, 0}, Bin)]}
    end;
decode(_, TypeRef, Data, _Opts) ->
    {error, [sp_error:type_mismatch(TypeRef, Data)]}.

-spec schema(atom(), spectra:sp_type_reference(), map()) -> map().
schema(json_schema, {type, datetime, 0}, _Opts) ->
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

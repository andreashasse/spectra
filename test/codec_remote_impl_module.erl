-module(codec_remote_impl_module).

%% Remote module that itself implements spectra_codec.
%% Any reference to its types should automatically use its encode/decode.
-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-opaque color() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.

-export_type([color/0]).
-export([encode/4, decode/4]).

-spec encode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    spectra:codec_encode_result().
encode(_, {type, color, 0}, {R, G, B}, _Opts) when
    is_integer(R),
    is_integer(G),
    is_integer(B),
    R >= 0,
    R =< 255,
    G >= 0,
    G =< 255,
    B >= 0,
    B =< 255
->
    {ok, iolist_to_binary(io_lib:format("#~2.16.0B~2.16.0B~2.16.0B", [R, G, B]))};
encode(_, {type, color, 0}, Data, _Opts) ->
    {error, [sp_error:type_mismatch({type, color, 0}, Data)]}.

-spec decode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    spectra:codec_decode_result().
decode(_, {type, color, 0}, <<"#", R1, R2, G1, G2, B1, B2>>, _Opts) ->
    R = list_to_integer([R1, R2], 16),
    G = list_to_integer([G1, G2], 16),
    B = list_to_integer([B1, B2], 16),
    {ok, {R, G, B}};
decode(_, {type, color, 0}, Data, _Opts) ->
    {error, [sp_error:type_mismatch({type, color, 0}, Data)]}.

-module(codec_appenv_type_codec).

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-export([encode/4, decode/4]).

-spec encode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    spectra:codec_encode_result().
encode(_, {type, token, 0}, {token, Bin}, _Opts) when is_binary(Bin) ->
    {ok, Bin};
encode(_, {type, token, 0}, Data, _Opts) ->
    {error, [sp_error:type_mismatch({type, token, 0}, Data)]}.

-spec decode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    spectra:codec_decode_result().
decode(_, {type, token, 0}, Bin, _Opts) when is_binary(Bin) ->
    {ok, {token, Bin}};
decode(_, {type, token, 0}, Data, _Opts) ->
    {error, [sp_error:type_mismatch({type, token, 0}, Data)]}.

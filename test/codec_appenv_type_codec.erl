-module(codec_appenv_type_codec).

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-export([encode/5, decode/5]).

-spec encode(atom(), module(), spectra:sp_type_reference(), dynamic(), map()) ->
    spectra:codec_encode_result().
encode(_, _Mod, {type, token, 0}, {token, Bin}, _Opts) when is_binary(Bin) ->
    {ok, Bin};
encode(_, _Mod, {type, token, 0}, Data, _Opts) ->
    {error, [sp_error:type_mismatch({type, token, 0}, Data)]}.

-spec decode(atom(), module(), spectra:sp_type_reference(), dynamic(), map()) ->
    spectra:codec_decode_result().
decode(_, _Mod, {type, token, 0}, Bin, _Opts) when is_binary(Bin) ->
    {ok, {token, Bin}};
decode(_, _Mod, {type, token, 0}, Data, _Opts) ->
    {error, [sp_error:type_mismatch({type, token, 0}, Data)]}.

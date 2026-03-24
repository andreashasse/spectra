-module(codec_no_schema_module).

%% Module that implements spectra_codec but NOT the optional schema/3 callback.
-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-opaque point() :: {float(), float()}.

-export_type([point/0]).
-export([encode/5, decode/5]).

-spec encode(atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type()) ->
    spectra:codec_encode_result().
encode(_, _Mod, {type, point, 0}, {X, Y}, _Opts) when is_number(X), is_number(Y) ->
    {ok, [X, Y]};
encode(_, _Mod, {type, point, 0}, Data, _Opts) ->
    {error, [sp_error:type_mismatch({type, point, 0}, Data)]}.

-spec decode(atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type()) ->
    spectra:codec_decode_result().
decode(_, _Mod, {type, point, 0}, [X, Y], _Opts) when is_number(X), is_number(Y) ->
    {ok, {X, Y}};
decode(_, _Mod, {type, point, 0}, Data, _Opts) ->
    {error, [sp_error:type_mismatch({type, point, 0}, Data)]}.

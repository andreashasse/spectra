-module(codec_no_schema_module).

%% Module that implements spectra_codec but NOT the optional schema/3 callback.
-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-opaque point() :: {float(), float()}.

-export_type([point/0]).
-export([encode/4, decode/4]).

-spec encode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    {ok, term()} | {error, [spectra:error()]}.
encode(_, {type, point, 0}, {X, Y}, _Opts) when is_number(X), is_number(Y) ->
    {ok, [X, Y]};
encode(_, TypeRef, Data, _Opts) ->
    {error, [sp_error:type_mismatch(TypeRef, Data)]}.

-spec decode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
decode(_, {type, point, 0}, [X, Y], _Opts) when is_number(X), is_number(Y) ->
    {ok, {X, Y}};
decode(_, TypeRef, Data, _Opts) ->
    {error, [sp_error:type_mismatch(TypeRef, Data)]}.

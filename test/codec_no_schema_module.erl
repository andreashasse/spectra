-module(codec_no_schema_module).

%% Module that implements spectra_codec but NOT the optional schema/2 callback.
-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-opaque point() :: {float(), float()}.

-export_type([point/0]).
-export([encode/3, decode/3]).

-spec encode(atom(), spectra:sp_type_reference(), dynamic()) ->
    {ok, term()} | {error, [spectra:error()]}.
encode(_, {type, point, 0}, {X, Y}) when is_number(X), is_number(Y) ->
    {ok, [X, Y]};
encode(_, TypeRef, Data) ->
    {error, [sp_error:type_mismatch(TypeRef, Data)]}.

-spec decode(atom(), spectra:sp_type_reference(), dynamic()) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
decode(_, {type, point, 0}, [X, Y]) when is_number(X), is_number(Y) ->
    {ok, {X, Y}};
decode(_, TypeRef, Data) ->
    {error, [sp_error:type_mismatch(TypeRef, Data)]}.

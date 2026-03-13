-module(codec_geo_module).

-behaviour(spectra_codec).

-opaque point() :: {float(), float()}.
-type maybe_point() :: point() | undefined.
%% A plain type alias for point(). When this type appears as a field inside a
%% containing type, to_json_inner/3 is called for it, resolves it to
%% #sp_user_type_ref{type_name=point}, then (before the fix) called itself
%% again instead of to_json/3, bypassing the codec and crashing.
-type named_point() :: point().
%% A map containing named_point() as a field value — forces the bug path.
-type location() :: #{coords => named_point()}.
%% A parameterized type pairing a point() with an arbitrary status value.
-type point_with_status(Status) :: #{point := point(), status := Status}.
%% A concrete instantiation using a union literal status.
-type active_passive_point() :: point_with_status(active | passive).

-export_type([
    point/0, maybe_point/0, named_point/0, location/0, point_with_status/1, active_passive_point/0
]).
-export([encode/4, decode/4, schema/3]).

-spec encode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    spectra:codec_encode_result().
encode(_, {type, point, 0}, {X, Y}, _Opts) when is_number(X), is_number(Y) ->
    {ok, [X, Y]};
encode(_, {type, point, 0}, Data, _Opts) ->
    {error, [sp_error:type_mismatch({type, point, 0}, Data)]};
encode(_, _, _, _) ->
    continue.

-spec decode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    spectra:codec_decode_result().
decode(_, {type, point, 0}, [X, Y], _Opts) when is_number(X), is_number(Y) ->
    {ok, {X, Y}};
decode(_, {type, point, 0}, Data, _Opts) ->
    {error, [sp_error:type_mismatch({type, point, 0}, Data)]};
decode(_, _, _, _) ->
    continue.

-spec schema(atom(), spectra:sp_type_reference(), map()) -> map().
schema(json_schema, {type, point, 0}, _Opts) ->
    #{
        type => <<"array">>,
        items => #{type => <<"number">>},
        minItems => 2,
        maxItems => 2
    };
schema(_, _, _) ->
    continue.

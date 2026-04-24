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
-export([encode/6, decode/6, schema/5]).

-spec encode(
    atom(),
    spectra:type_info(),
    spectra:sp_type_reference(),
    spectra:sp_type(),
    dynamic(),
    spectra:sp_config()
) ->
    spectra:codec_encode_result().
encode(_, _CallerTypeInfo, {type, point, 0}, _TargetType, {X, Y}, _Config) when
    is_number(X), is_number(Y)
->
    {ok, [X, Y]};
encode(_, _CallerTypeInfo, {type, point, 0}, _TargetType, Data, _Config) ->
    {error, [sp_error:type_mismatch({type, point, 0}, Data)]};
encode(_, _, _, _, _, _) ->
    continue.

-spec decode(
    atom(),
    spectra:type_info(),
    spectra:sp_type_reference(),
    spectra:sp_type(),
    dynamic(),
    spectra:sp_config()
) ->
    spectra:codec_decode_result().
decode(_, _CallerTypeInfo, {type, point, 0}, _TargetType, [X, Y], _Config) when
    is_number(X), is_number(Y)
->
    {ok, {X, Y}};
decode(_, _CallerTypeInfo, {type, point, 0}, _TargetType, Data, _Config) ->
    {error, [sp_error:type_mismatch({type, point, 0}, Data)]};
decode(_, _, _, _, _, _) ->
    continue.

-spec schema(
    atom(), spectra:type_info(), spectra:sp_type_reference(), spectra:sp_type(), spectra:sp_config()
) -> map().
schema(json_schema, _CallerTypeInfo, {type, point, 0}, _TargetType, _Config) ->
    #{
        type => <<"array">>,
        items => #{type => <<"number">>},
        minItems => 2,
        maxItems => 2
    };
schema(_, _, _, _, _) ->
    continue.

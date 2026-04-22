-module(codec_multi_behaviour_module).

%% spectra_codec is declared SECOND. This exposes the proplists:get_value/3 bug
%% in set_module_meta/2: only the first -behaviour attribute is checked, so this
%% module is silently not recognised as a codec.
-behaviour(codec_stub_behaviour).
-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-export([encode/6, decode/6, schema/5]).

-opaque point() :: {float(), float()}.
-export_type([point/0]).

-spec encode(
    atom(),
    spectra:type_info(),
    spectra:sp_type_reference(),
    dynamic(),
    spectra:sp_type(),
    spectra:sp_config()
) ->
    spectra:codec_encode_result().
encode(_, _CallerTypeInfo, {type, point, 0}, {X, Y}, _TargetType, _Config) when
    is_number(X), is_number(Y)
->
    {ok, [X, Y]};
encode(_, _CallerTypeInfo, {type, point, 0}, Data, _TargetType, _Config) ->
    {error, [sp_error:type_mismatch({type, point, 0}, Data)]}.

-spec decode(
    atom(),
    spectra:type_info(),
    spectra:sp_type_reference(),
    dynamic(),
    spectra:sp_type(),
    spectra:sp_config()
) ->
    spectra:codec_decode_result().
decode(_, _CallerTypeInfo, {type, point, 0}, [X, Y], _TargetType, _Config) when
    is_number(X), is_number(Y)
->
    {ok, {X, Y}};
decode(_, _CallerTypeInfo, {type, point, 0}, Data, _TargetType, _Config) ->
    {error, [sp_error:type_mismatch({type, point, 0}, Data)]}.

-spec schema(
    atom(), spectra:type_info(), spectra:sp_type_reference(), spectra:sp_type(), spectra:sp_config()
) -> dynamic().
schema(json_schema, _CallerTypeInfo, {type, point, 0}, _TargetType, _Config) ->
    #{
        type => <<"array">>,
        items => #{type => <<"number">>},
        minItems => 2,
        maxItems => 2
    };
schema(_, _, _, _, _) ->
    continue.

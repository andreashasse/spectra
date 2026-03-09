-module(codec_multi_behaviour_module).

%% spectra_codec is declared SECOND. This exposes the proplists:get_value/3 bug
%% in set_module_meta/2: only the first -behaviour attribute is checked, so this
%% module is silently not recognised as a codec.
-behaviour(codec_stub_behaviour).
-behaviour(spectra_codec).

-export([encode/4, decode/4, schema/3]).

-opaque point() :: {float(), float()}.
-export_type([point/0]).

-spec encode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    spectra:codec_encode_result().
encode(_, {type, point, 0}, {X, Y}, _Opts) when is_number(X), is_number(Y) ->
    {ok, [X, Y]};
encode(_, _, _, _) ->
    continue.

-spec decode(atom(), spectra:sp_type_reference(), dynamic(), map()) ->
    spectra:codec_decode_result().
decode(_, {type, point, 0}, [X, Y], _Opts) when is_number(X), is_number(Y) ->
    {ok, {X, Y}};
decode(_, _, _, _) ->
    continue.

-spec schema(atom(), spectra:sp_type_reference(), map()) -> dynamic().
schema(json_schema, {type, point, 0}, _Opts) ->
    #{
        type => <<"array">>,
        items => #{type => <<"number">>},
        minItems => 2,
        maxItems => 2
    };
schema(_, _, _) ->
    continue.

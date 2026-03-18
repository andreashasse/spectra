-module(prefixed_id_codec).
-behaviour(spectra_codec).

-export([encode/5, decode/5, schema/4]).

%% This codec handles any binary type whose type_parameters is a prefix binary.
%% The Erlang value is the raw ID (without prefix); the wire format includes the prefix.

%% Types defined here are automatically covered by this codec (no app env needed).
-spectra(#{type_parameters => <<"user:">>}).
-type user_id() :: binary().

-spectra(#{type_parameters => <<"org:">>}).
-type org_id() :: binary().

-export_type([user_id/0, org_id/0]).

%% Strips the prefix on decode, re-attaches it on encode.
decode(json, _Mod, TypeRef, Data, Prefix) when is_binary(Data), is_binary(Prefix) ->
    PrefixLen = byte_size(Prefix),
    case Data of
        <<Prefix:PrefixLen/binary, Rest/binary>> -> {ok, Rest};
        _ -> {error, [sp_error:type_mismatch(TypeRef, Data)]}
    end;
decode(_Format, _Mod, _TypeRef, _Data, _Params) ->
    continue.

encode(json, _Mod, _TypeRef, Data, Prefix) when is_binary(Data), is_binary(Prefix) ->
    {ok, <<Prefix/binary, Data/binary>>};
encode(_Format, _Mod, _TypeRef, _Data, _Params) ->
    continue.

schema(json_schema, _Mod, _TypeRef, Prefix) when is_binary(Prefix) ->
    #{type => <<"string">>, pattern => <<"^", Prefix/binary>>};
schema(_Format, _Mod, _TypeRef, _Params) ->
    continue.

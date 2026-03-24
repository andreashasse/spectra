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
decode(json, _Mod, TypeRef, Data, SpType) when is_binary(Data) ->
    case spectra_type:parameters(SpType) of
        Prefix when is_binary(Prefix) ->
            PrefixLen = byte_size(Prefix),
            case Data of
                <<Prefix:PrefixLen/binary, Rest/binary>> -> {ok, Rest};
                _ -> {error, [sp_error:type_mismatch(TypeRef, Data)]}
            end;
        _ ->
            continue
    end;
decode(_Format, _Mod, _TypeRef, _Data, _SpType) ->
    continue.

encode(json, _Mod, _TypeRef, Data, SpType) when is_binary(Data) ->
    case spectra_type:parameters(SpType) of
        Prefix when is_binary(Prefix) ->
            {ok, <<Prefix/binary, Data/binary>>};
        _ ->
            continue
    end;
encode(_Format, _Mod, _TypeRef, _Data, _SpType) ->
    continue.

schema(json_schema, _Mod, _TypeRef, SpType) ->
    case spectra_type:parameters(SpType) of
        Prefix when is_binary(Prefix) ->
            #{type => <<"string">>, pattern => <<"^", Prefix/binary>>};
        _ ->
            continue
    end;
schema(_Format, _Mod, _TypeRef, _SpType) ->
    continue.

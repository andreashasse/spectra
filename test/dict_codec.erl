-module(dict_codec).

%% Codec for dict:dict(Key, Value).
%%
%% JSON representation: a JSON object where each dict entry becomes a key-value
%% pair. Keys must encode to binary strings (required by JSON). The concrete Key
%% and Value types are extracted from the sp_type() node via
%% spectra_type:type_args/1, so this codec works for any instantiation such as
%% dict:dict(binary(), integer()) or dict:dict(binary(), dict:dict(binary(), float())).

-behaviour(spectra_codec).

-include("../include/spectra.hrl").

-export([encode/5, decode/5, schema/4]).

-spec encode(atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type()) ->
    spectra:codec_encode_result().
encode(json, Mod, _TypeRef, Data, SpType) ->
    TypeInfo = spectra_module_types:get(Mod),
    [KeyType, ValueType] = spectra_type:type_args(SpType),
    encode_pairs(TypeInfo, KeyType, ValueType, dict:to_list(Data), #{});
encode(_Format, _Mod, _TypeRef, _Data, _SpType) ->
    continue.

-spec decode(atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type()) ->
    spectra:codec_decode_result().
decode(json, Mod, _TypeRef, Data, SpType) when is_map(Data) ->
    TypeInfo = spectra_module_types:get(Mod),
    [KeyType, ValueType] = spectra_type:type_args(SpType),
    decode_pairs(TypeInfo, KeyType, ValueType, maps:to_list(Data), dict:new());
decode(_Format, _Mod, _TypeRef, _Data, _SpType) ->
    continue.

-spec schema(atom(), module(), spectra:sp_type_reference(), spectra:sp_type()) ->
    dynamic().
schema(json_schema, Mod, _TypeRef, SpType) ->
    TypeInfo = spectra_module_types:get(Mod),
    [_KeyType, ValueType] = spectra_type:type_args(SpType),
    ValueSchema = spectra_json_schema:to_schema(TypeInfo, ValueType),
    #{type => <<"object">>, additionalProperties => ValueSchema};
schema(_Format, _Mod, _TypeRef, _SpType) ->
    continue.

%% Internal helpers

encode_pairs(_TypeInfo, _KeyType, _ValueType, [], Acc) ->
    {ok, Acc};
encode_pairs(TypeInfo, KeyType, ValueType, [{Key, Value} | Rest], Acc) ->
    case spectra_json:to_json(TypeInfo, KeyType, Key) of
        {ok, KeyBin} when is_binary(KeyBin) ->
            case spectra_json:to_json(TypeInfo, ValueType, Value) of
                {ok, ValueJson} ->
                    encode_pairs(TypeInfo, KeyType, ValueType, Rest, Acc#{KeyBin => ValueJson});
                {error, _} = Err ->
                    Err
            end;
        {ok, _KeyJson} ->
            {error, [
                sp_error:type_mismatch({type, dict, 2}, Key, #{
                    message => "dict key must encode to binary string for JSON"
                })
            ]};
        {error, _} = Err ->
            Err
    end.

decode_pairs(_TypeInfo, _KeyType, _ValueType, [], Acc) ->
    {ok, Acc};
decode_pairs(TypeInfo, KeyType, ValueType, [{Key, Value} | Rest], Acc) ->
    case spectra_json:from_json(TypeInfo, KeyType, Key) of
        {ok, KeyDecoded} ->
            case spectra_json:from_json(TypeInfo, ValueType, Value) of
                {ok, ValueDecoded} ->
                    decode_pairs(
                        TypeInfo,
                        KeyType,
                        ValueType,
                        Rest,
                        dict:store(KeyDecoded, ValueDecoded, Acc)
                    );
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

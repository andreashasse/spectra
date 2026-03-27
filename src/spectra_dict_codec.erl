-module(spectra_dict_codec).

-doc """
Built-in codec for `dict:dict(Key, Value)`.

Encodes a dict as a JSON object and decodes a JSON object back into a dict.
Keys must encode to binary strings when encoding to JSON.

The concrete `Key` and `Value` types are extracted from the `sp_type()` node via
`spectra_type:type_args/1`, so this codec works for any instantiation such as
`dict:dict(binary(), integer())` or `dict:dict(binary(), dict:dict(binary(), float()))`.

## Registering

Add to the application environment before encoding or decoding:

```erlang
{spectra, [
    {codecs, #{
        {dict, {type, dict, 2}} => spectra_dict_codec
    }}
]}
```

## Example

```erlang
-type word_counts() :: dict:dict(binary(), non_neg_integer()).

D = dict:from_list([{<<"hello">>, 3}, {<<"world">>, 1}]),
{ok, Json} = spectra:encode(json, my_module, word_counts, D).
%% => {ok, <<"{\"hello\":3,\"world\":1}">>}

{ok, D2} = spectra:decode(json, my_module, word_counts, Json).
```
""".

-behaviour(spectra_codec).

-include("../include/spectra_internal.hrl").

-export([encode/6, decode/6, schema/5]).

-spec encode(atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type(), term()) ->
    spectra:codec_encode_result().
encode(json, Mod, {type, dict, 2} = TypeRef, Data, SpType, _Params) ->
    try dict:to_list(Data) of
        Pairs ->
            TypeInfo = spectra_module_types:get(Mod),
            [KeyType, ValueType] = spectra_type:type_args(SpType),
            encode_pairs(TypeInfo, KeyType, ValueType, Pairs, [])
    catch
        error:badarg ->
            {error, [sp_error:type_mismatch(TypeRef, Data)]}
    end.

-spec decode(atom(), module(), spectra:sp_type_reference(), dynamic(), spectra:sp_type(), term()) ->
    spectra:codec_decode_result().
decode(json, Mod, {type, dict, 2}, Data, SpType, _Params) when is_map(Data) ->
    TypeInfo = spectra_module_types:get(Mod),
    [KeyType, ValueType] = spectra_type:type_args(SpType),
    decode_pairs(TypeInfo, KeyType, ValueType, maps:to_list(Data), []);
decode(json, _Mod, {type, dict, 2} = TypeRef, Data, _SpType, _Params) ->
    {error, [sp_error:type_mismatch(TypeRef, Data)]}.

-spec schema(atom(), module(), spectra:sp_type_reference(), spectra:sp_type(), term()) ->
    dynamic().
schema(json_schema, Mod, {type, dict, 2}, SpType, _Params) ->
    TypeInfo = spectra_module_types:get(Mod),
    [_KeyType, ValueType] = spectra_type:type_args(SpType),
    ValueSchema = spectra_json_schema:to_schema(TypeInfo, ValueType),
    #{type => <<"object">>, additionalProperties => ValueSchema}.

%% Internal helpers

encode_pairs(_TypeInfo, _KeyType, _ValueType, [], Acc) ->
    {ok, maps:from_list(Acc)};
encode_pairs(TypeInfo, KeyType, ValueType, [{Key, Value} | Rest], Acc) ->
    case spectra_json:to_json(TypeInfo, KeyType, Key) of
        {ok, KeyBin} when is_binary(KeyBin) ->
            case spectra_json:to_json(TypeInfo, ValueType, Value) of
                {ok, ValueJson} ->
                    encode_pairs(TypeInfo, KeyType, ValueType, Rest, [{KeyBin, ValueJson} | Acc]);
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
    {ok, dict:from_list(Acc)};
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
                        [{KeyDecoded, ValueDecoded} | Acc]
                    );
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

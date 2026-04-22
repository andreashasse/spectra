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
encode(json, CallerTypeInfo, {type, dict, 2} = TargetTypeRef, TargetType, Data, Config) ->
    try dict:to_list(Data) of
        Pairs ->
            [KeyType, ValueType] = spectra_type:type_args(TargetType),
            encode_pairs(CallerTypeInfo, KeyType, ValueType, Pairs, [], Config)
    catch
        error:badarg ->
            {error, [sp_error:type_mismatch(TargetTypeRef, Data)]}
    end.

-spec decode(
    atom(),
    spectra:type_info(),
    spectra:sp_type_reference(),
    spectra:sp_type(),
    dynamic(),
    spectra:sp_config()
) ->
    spectra:codec_decode_result().
decode(json, CallerTypeInfo, {type, dict, 2}, TargetType, Data, Config) when is_map(Data) ->
    [KeyType, ValueType] = spectra_type:type_args(TargetType),
    decode_pairs(CallerTypeInfo, KeyType, ValueType, maps:to_list(Data), [], Config);
decode(json, _CallerTypeInfo, {type, dict, 2} = TargetTypeRef, _TargetType, Data, _Config) ->
    {error, [sp_error:type_mismatch(TargetTypeRef, Data)]}.

-spec schema(
    atom(), spectra:type_info(), spectra:sp_type_reference(), spectra:sp_type(), spectra:sp_config()
) ->
    dynamic().
schema(json_schema, CallerTypeInfo, {type, dict, 2}, TargetType, Config) ->
    [_KeyType, ValueType] = spectra_type:type_args(TargetType),
    ValueSchema = spectra_json_schema:to_schema(CallerTypeInfo, ValueType, Config),
    #{type => <<"object">>, additionalProperties => ValueSchema}.

%% Internal helpers

encode_pairs(_TypeInfo, _KeyType, _ValueType, [], Acc, _Config) ->
    {ok, maps:from_list(Acc)};
encode_pairs(TypeInfo, KeyType, ValueType, [{Key, Value} | Rest], Acc, Config) ->
    case spectra_json:to_json(TypeInfo, KeyType, Key, Config) of
        {ok, KeyBin} when is_binary(KeyBin) ->
            case spectra_json:to_json(TypeInfo, ValueType, Value, Config) of
                {ok, ValueJson} ->
                    encode_pairs(
                        TypeInfo, KeyType, ValueType, Rest, [{KeyBin, ValueJson} | Acc], Config
                    );
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

decode_pairs(_TypeInfo, _KeyType, _ValueType, [], Acc, _Config) ->
    {ok, dict:from_list(Acc)};
decode_pairs(TypeInfo, KeyType, ValueType, [{Key, Value} | Rest], Acc, Config) ->
    case spectra_json:from_json(TypeInfo, KeyType, Key, Config) of
        {ok, KeyDecoded} ->
            case spectra_json:from_json(TypeInfo, ValueType, Value, Config) of
                {ok, ValueDecoded} ->
                    decode_pairs(
                        TypeInfo,
                        KeyType,
                        ValueType,
                        Rest,
                        [{KeyDecoded, ValueDecoded} | Acc],
                        Config
                    );
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

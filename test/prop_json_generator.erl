-module(prop_json_generator).

-include_lib("proper/include/proper.hrl").

%% Property test that verifies all generated JSON values can be encoded
prop_json_encodable() ->
    ?FORALL(
        JsonValue,
        json_generator:json_value(),
        begin
            try
                EncodedJson = json:encode(JsonValue),
                is_binary(EncodedJson) orelse is_list(EncodedJson)
            catch
                _:_ ->
                    false
            end
        end
    ).

%% Property test that verifies encode/decode roundtrip
prop_json_roundtrip() ->
    ?FORALL(
        JsonValue,
        json_generator:json_value(),
        begin
            try
                EncodedJson = json:encode(JsonValue),
                % Convert iolist to binary for json:decode
                JsonBinary = iolist_to_binary(EncodedJson),
                DecodedValue = json:decode(JsonBinary),
                JsonValue =:= DecodedValue
            catch
                _:_ ->
                    false
            end
        end
    ).

-module(prop_json_generator).

-include_lib("proper/include/proper.hrl").

%% NOTE: This module tests the `json_generator` test utility, not the
%% Spectra project itself. It ensures that our property-based testing
%% infrastructure generates valid JSON structures that the standard
%% Erlang `json` module can encode.

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

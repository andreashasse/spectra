-module(prop_json_schema_validates_encoded).

-include_lib("proper/include/proper.hrl").
-include("../include/spectra_internal.hrl").

%% Property: JSON output of spectra:encode/4 validates against the
%% JSON schema produced by spectra:schema/3 for the same type.
%%
%% This pins down that the encoder and schema generator agree on what
%% valid JSON for a type looks like. A drift between them (e.g. encoder
%% emits a field the schema marks as additionalProperties: false) would
%% be caught here.
%%
%% Uses jesse for validation. jesse targets JSON Schema draft-04, so we
%% strip the `$schema` field (Spectra emits 2020-12). Schema features
%% that are 2020-12-only may cause false negatives — narrow the type
%% filter if that becomes a problem.

prop_schema_validates_encoded_data() ->
    ?FORALL(
        Type,
        ?SUCHTHAT(T, sp_type_generators:sp_type(), sp_type_filters:json_schema_roundtrip_safe(T)),
        begin
            TypeInfo = spectra_type_info:add_type(
                spectra_type_info:new(?MODULE, false), test_type, 0, Type
            ),
            ?FORALL(
                Data,
                sp_data_generators:gen_data(TypeInfo, Type),
                begin
                    EncodeResult = safe_encode(TypeInfo, Type, Data),
                    SchemaResult = safe_schema(TypeInfo, Type),
                    case {EncodeResult, SchemaResult} of
                        {{ok, PreEncoded}, {ok, Schema}} ->
                            validate(Type, Data, PreEncoded, Schema);
                        {{ok, _PreEncoded}, Other} ->
                            ?WHENFAIL(
                                io:format(
                                    "~nEncode succeeded but schema did not~n"
                                    "  Type: ~p~n"
                                    "  Schema result: ~p~n",
                                    [Type, Other]
                                ),
                                false
                            );
                        {Other, _} ->
                            ?WHENFAIL(
                                io:format(
                                    "~nEncode failed on generated data~n"
                                    "  Type: ~p~n"
                                    "  Data: ~p~n"
                                    "  Result: ~p~n",
                                    [Type, Data, Other]
                                ),
                                false
                            )
                    end
                end
            )
        end
    ).

validate(Type, Data, PreEncoded, Schema) ->
    %% Normalise to binary keys and strip the $schema field; jesse targets
    %% draft-04 and rejects explicit 2020-12 schemas.
    SchemaWithoutVersion = maps:remove('$schema', Schema),
    JesseSchema = json:decode(iolist_to_binary(json:encode(SchemaWithoutVersion))),
    JesseData = json:decode(iolist_to_binary(json:encode(PreEncoded))),
    case jesse:validate_with_schema(JesseSchema, JesseData) of
        {ok, _} ->
            true;
        {error, Errors} ->
            ?WHENFAIL(
                io:format(
                    "~nJesse rejected encoded data against generated schema~n"
                    "  Type:   ~p~n"
                    "  Data:   ~p~n"
                    "  Encoded:~p~n"
                    "  Schema: ~p~n"
                    "  Errors: ~p~n",
                    [Type, Data, PreEncoded, JesseSchema, Errors]
                ),
                false
            )
    end.

safe_encode(TypeInfo, Type, Data) ->
    try
        spectra:encode(json, TypeInfo, Type, Data, [pre_encoded])
    catch
        Class:Reason -> {exception, {Class, Reason}}
    end.

safe_schema(TypeInfo, Type) ->
    try
        Schema = spectra:schema(json_schema, TypeInfo, Type, [pre_encoded]),
        {ok, Schema}
    catch
        Class:Reason -> {exception, {Class, Reason}}
    end.

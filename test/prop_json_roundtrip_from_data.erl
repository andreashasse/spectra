-module(prop_json_roundtrip_from_data).

-include_lib("proper/include/proper.hrl").
-include("../include/spectra_internal.hrl").

prop_json_encode_decode_roundtrip() ->
    ?FORALL(
        Type,
        ?SUCHTHAT(T, sp_type_generators:sp_type(), sp_type_filters:json_roundtrip_safe(T)),
        begin
            TypeInfo = spectra_type_info:add_type(
                spectra_type_info:new(?MODULE, false), test_type, 0, Type
            ),
            ?FORALL(
                Data,
                sp_data_generators:gen_data(TypeInfo, Type),
                begin
                    case safe_encode(TypeInfo, Type, Data) of
                        {ok, Json} ->
                            case safe_decode(TypeInfo, Type, Json) of
                                {ok, Data2} ->
                                    {ok, Json2} = safe_encode(TypeInfo, Type, Data2),
                                    ?WHENFAIL(
                                        io:format(
                                            "~nRoundtrip / idempotence mismatch~n"
                                            "  Type:     ~p~n"
                                            "  Data in:  ~p~n"
                                            "  Json:     ~s~n"
                                            "  Data out: ~p~n"
                                            "  Json2:    ~s~n",
                                            [Type, Data, Json, Data2, Json2]
                                        ),
                                        Data =:= Data2 andalso Json =:= Json2
                                    );
                                Other ->
                                    ?WHENFAIL(
                                        io:format(
                                            "~nDecode failed on encoded data~n"
                                            "  Type: ~p~n"
                                            "  Data: ~p~n"
                                            "  Json: ~s~n"
                                            "  Decode result: ~p~n",
                                            [Type, Data, Json, Other]
                                        ),
                                        false
                                    )
                            end;
                        Other ->
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

safe_encode(TypeInfo, Type, Data) ->
    try
        case spectra:encode(json, TypeInfo, Type, Data) of
            {ok, IoData} -> {ok, iolist_to_binary(IoData)};
            Other -> Other
        end
    catch
        Class:Reason -> {exception, {Class, Reason}}
    end.

safe_decode(TypeInfo, Type, Json) ->
    try
        spectra:decode(json, TypeInfo, Type, Json)
    catch
        Class:Reason -> {exception, {Class, Reason}}
    end.

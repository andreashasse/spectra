-module(prop_string_roundtrip).

-include_lib("proper/include/proper.hrl").
-include("../include/spectra_internal.hrl").

prop_string_encode_decode_roundtrip() ->
    ?FORALL(
        Type,
        ?SUCHTHAT(T, sp_type_generators:sp_type(), sp_type_filters:string_roundtrip_safe(T)),
        begin
            TypeInfo = spectra_type_info:add_type(
                spectra_type_info:new(?MODULE, false), test_type, 0, Type
            ),
            ?FORALL(
                Data,
                sp_data_generators:gen_data(TypeInfo, Type),
                begin
                    case safe_encode(TypeInfo, Type, Data) of
                        {ok, Str} ->
                            case safe_decode(TypeInfo, Type, Str) of
                                {ok, Data2} ->
                                    ?WHENFAIL(
                                        io:format(
                                            "~nRoundtrip mismatch~n"
                                            "  Type:    ~p~n"
                                            "  Data in: ~p~n"
                                            "  Str:     ~p~n"
                                            "  Data out:~p~n",
                                            [Type, Data, Str, Data2]
                                        ),
                                        Data =:= Data2
                                    );
                                Other ->
                                    ?WHENFAIL(
                                        io:format(
                                            "~nDecode failed on encoded data~n"
                                            "  Type: ~p~n"
                                            "  Data: ~p~n"
                                            "  Str:  ~p~n"
                                            "  Decode result: ~p~n",
                                            [Type, Data, Str, Other]
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
        spectra:encode(string, TypeInfo, Type, Data)
    catch
        Class:Reason -> {exception, {Class, Reason}}
    end.

safe_decode(TypeInfo, Type, Str) ->
    try
        spectra:decode(string, TypeInfo, Type, Str)
    catch
        Class:Reason -> {exception, {Class, Reason}}
    end.

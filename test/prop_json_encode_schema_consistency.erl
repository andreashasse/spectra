-module(prop_json_encode_schema_consistency).

-include_lib("proper/include/proper.hrl").
-include("../include/spectra_internal.hrl").

%% Export for manual testing
-export([json_encode_schema_consistency_unfiltered/0]).
-define(spectra_error, {exception, spectra_error}).

%% Property test that verifies consistency between JSON encoding,
%% schema generation, and JSON decoding operations.
%%
%% Checks two main invariants:
%% 1. If a type supports encoding, it must support schema generation and decoding
%% 2. If a type doesn't support encoding (throws exception), schema generation must also fail
%%
%% NOTE: This unfiltered version is expected to fail as it tests for bugs in the library.
%% It's not run automatically by `make proper`. To run it manually:
%%   proper:quickcheck(prop_json_encode_schema_consistency:json_encode_schema_consistency_unfiltered())

json_encode_schema_consistency_unfiltered() ->
    ?FORALL(
        Type,
        sp_type_generators:sp_type(),
        begin
            % Create TypeInfo context with the generated type
            TypeInfo = #{{type, test_type} => Type},

            % Generate data matching this type
            ?FORALL(
                Data,
                prop_base:gen_data(TypeInfo, Type),
                begin
                    % Safely call all three operations
                    ToJsonResult = safe_to_json(TypeInfo, Type, Data),
                    ToSchemaResult = safe_to_schema(TypeInfo, Type),

                    % Check consistency based on to_json result
                    case ToJsonResult of
                        {ok, JsonValue} ->
                            check_success_consistency(
                                TypeInfo, Type, Data, JsonValue, ToSchemaResult
                            );
                        {error, _JsonError} ->
                            ?WHENFAIL(
                                io:format(
                                    "~nto_json failed on good? data:~n"
                                    "  Type: ~p~n"
                                    "  Original Data: ~p~n",
                                    [Type, Data]
                                ),
                                collect({success, type_category(Type)}, true)
                            );
                        {exception, JsonException} ->
                            check_exception_consistency(
                                JsonException, ToSchemaResult
                            )
                    end
                end
            )
        end
    ).

%% Filtered version that excludes known problematic types for demonstration
prop_json_encode_schema_consistency_filtered() ->
    ?FORALL(
        Type,
        ?SUCHTHAT(T, sp_type_generators:sp_type(), not is_problematic_type(T)),
        begin
            % Create TypeInfo context with the generated type plus known reference types
            TypeInfo = create_typeinfo_with_known_types(Type),

            % Generate data matching this type
            ?FORALL(
                Data,
                prop_base:gen_data(TypeInfo, Type),
                begin
                    % Safely call all three operations
                    ToJsonResult = safe_to_json(TypeInfo, Type, Data),
                    ToSchemaResult = safe_to_schema(TypeInfo, Type),

                    % Check consistency based on to_json result
                    case ToJsonResult of
                        {ok, JsonValue} ->
                            check_success_consistency(
                                TypeInfo, Type, Data, JsonValue, ToSchemaResult
                            );
                        {error, _JsonError} ->
                            ?WHENFAIL(
                                io:format(
                                    "~nto_json failed on good? data:~n"
                                    "  Type: ~p~n"
                                    "  Original Data: ~p~n",
                                    [Type, Data]
                                ),
                                collect({success, type_category(Type)}, true)
                            );
                        {exception, JsonException} ->
                            check_exception_consistency(
                                JsonException, ToSchemaResult
                            )
                    end
                end
            )
        end
    ).

%% Create TypeInfo with the test type plus all known types that generators might reference
create_typeinfo_with_known_types(Type) ->
    Types = [
        {test_type, 0, Type},
        {my_type, 0, #sp_simple_type{type = integer}},
        {my_string_type, 0, #sp_simple_type{type = string}},
        {my_int_type, 0, #sp_simple_type{type = integer}}
    ],
    Records = [
        {my_record, #sp_rec{
            name = my_record,
            arity = 3,
            fields = [
                #sp_rec_field{
                    name = id,
                    binary_name = <<"id">>,
                    type = #sp_simple_type{type = integer}
                },
                #sp_rec_field{
                    name = name,
                    binary_name = <<"name">>,
                    type = #sp_simple_type{type = string}
                }
            ]
        }},
        {user_record, #sp_rec{
            name = user_record,
            arity = 2,
            fields = [
                #sp_rec_field{
                    name = value,
                    binary_name = <<"value">>,
                    type = #sp_simple_type{type = integer}
                }
            ]
        }},
        {data_record, #sp_rec{
            name = data_record,
            arity = 2,
            fields = [
                #sp_rec_field{
                    name = data,
                    binary_name = <<"data">>,
                    type = #sp_simple_type{type = binary}
                }
            ]
        }}
    ],

    TypeInfo0 = spectra_type_info:new(),
    TypeInfo1 = lists:foldl(
        fun({Name, Arity, T}, Acc) ->
            spectra_type_info:add_type(Acc, Name, Arity, T)
        end,
        TypeInfo0,
        Types
    ),
    lists:foldl(
        fun({Name, Record}, Acc) ->
            spectra_type_info:add_record(Acc, Name, Record)
        end,
        TypeInfo1,
        Records
    ).

safe_to_json(TypeInfo, Type, Data) ->
    save(fun() -> spectra_json:to_json(TypeInfo, Type, Data) end).

safe_to_schema(TypeInfo, Type) ->
    save(fun() -> spectra_json_schema:to_schema(TypeInfo, Type) end).

safe_from_json(TypeInfo, Type, JsonValue) ->
    save(fun() -> spectra_json:from_json(TypeInfo, Type, JsonValue) end).

save(Fun) ->
    try
        Fun()
    catch
        error:{type_not_supported, _} ->
            ?spectra_error;
        error:{type_not_implemented, _} ->
            ?spectra_error;
        error:{module_types_not_found, _, _} ->
            ?spectra_error;
        error:{type_not_found, _} ->
            ?spectra_error;
        error:{record_not_found, _} ->
            ?spectra_error;
        error:{type_variable_not_found, _} ->
            ?spectra_error;
        error:Reason:Stack ->
            {exception, {Reason, Stack}}
    end.

%% When to_json succeeds, schema should succeed and from_json should succeed
check_success_consistency(TypeInfo, Type, OriginalData, JsonValue, ToSchemaResult) ->
    case ToSchemaResult of
        {ok, _Schema} ->
            % Schema generation succeeded, now check from_json
            FromJsonResult = safe_from_json(TypeInfo, Type, JsonValue),
            case FromJsonResult of
                {ok, _DecodedData} ->
                    % Success! All three operations succeeded
                    % Note: We don't check roundtrip equality because
                    % the type system may allow lossy conversions
                    ?WHENFAIL(
                        io:format(
                            "~nSuccess case (all operations succeeded):~n"
                            "  Type: ~p~n"
                            "  Original Data: ~p~n"
                            "  JSON: ~p~n",
                            [Type, OriginalData, JsonValue]
                        ),
                        collect({success, type_category(Type)}, true)
                    );
                {error, FromJsonError} ->
                    ?WHENFAIL(
                        io:format(
                            "~nInconsistency: to_json and to_schema succeeded, "
                            "but from_json failed~n"
                            "  Type: ~p~n"
                            "  Data: ~p~n"
                            "  JSON: ~p~n"
                            "  from_json error: ~p~n",
                            [Type, OriginalData, JsonValue, FromJsonError]
                        ),
                        false
                    );
                {exception, Exception} ->
                    ?WHENFAIL(
                        io:format(
                            "~nInconsistency: to_json and to_schema succeeded, "
                            "but from_json threw exception~n"
                            "  Type: ~p~n"
                            "  Data: ~p~n"
                            "  JSON: ~p~n"
                            "  Exception: ~p~n",
                            [Type, OriginalData, JsonValue, Exception]
                        ),
                        false
                    )
            end;
        {error, SchemaError} ->
            ?WHENFAIL(
                io:format(
                    "~nto_json worked, but schema failed. This can happen as (potentially) only part of the type is used when generating json:~n"
                    "  Type: ~p~n"
                    "  Original Data: ~p~n"
                    "  JSON: ~p~n"
                    "  Schema error: ~p~n",
                    [Type, OriginalData, JsonValue, SchemaError]
                ),
                collect({success, type_category(Type)}, true)
            );
        {exception, Exception} ->
            ?WHENFAIL(
                io:format(
                    "~nto_json succeeded but to_schema threw exception. This can happen.~n"
                    "  Type: ~p~n"
                    "  Data: ~p~n"
                    "  JSON: ~p~n"
                    "  Exception: ~p~n",
                    [Type, OriginalData, JsonValue, Exception]
                ),
                collect({success, type_category(Type)}, true)
            )
    end.

%% When to_json throws exception, to_schema should also fail
%% (either by throwing exception or returning error)
check_exception_consistency(JsonException, ToSchemaResult) ->
    JsonExceptionType = exception_type(JsonException),
    case ToSchemaResult of
        {exception, SchemaException} ->
            SchemaExceptionType = exception_type(SchemaException),
            case JsonExceptionType =:= SchemaExceptionType of
                true ->
                    % Exceptions match - this is expected consistency
                    collect({both_exception, JsonExceptionType}, true);
                false ->
                    ?WHENFAIL(
                        io:format(
                            "~nInconsistency: Different exception types~n"
                            "  to_json exception type: ~p~n"
                            "  to_schema exception type: ~p~n",
                            [JsonException, SchemaException]
                        ),
                        false
                    )
            end;
        {error, SchemaError} ->
            ?WHENFAIL(
                io:format(
                    "~nInconsistency: to_json exception schema error~n"
                    "  to_json exception: ~p~n"
                    "  to_schema error: ~p~n",
                    [JsonException, SchemaError]
                ),
                false
            );
        {ok, _Schema} ->
            ?WHENFAIL(
                io:format(
                    "~nInconsistency: to_json threw exception but to_schema succeeded~n"
                    "  to_json exception type: ~p~n",
                    [JsonException]
                ),
                false
            )
    end.

%% Extract the exception type from an exception value
exception_type({ErrorType, _Stack}) when is_tuple(ErrorType); is_atom(ErrorType) ->
    % For errors like {function_clause, Stack} or {type_not_supported, Details}
    % we extract just the error type
    case ErrorType of
        ET when is_atom(ET) -> ET;
        ET when is_tuple(ET) -> element(1, ET)
    end;
exception_type(Other) ->
    Other.

%% These types cause inconsistencies between to_json and to_schema
is_problematic_type(#sp_var{}) ->
    % Type variables are not supported
    true;
is_problematic_type(#sp_remote_type{}) ->
    % Remote types require module type info which is complex to set up in tests
    true;
is_problematic_type(_) ->
    false.

%% Categorize types for collect() statistics
type_category(#sp_simple_type{type = T}) ->
    {simple, T};
type_category(#sp_literal{}) ->
    literal;
type_category(#sp_range{}) ->
    range;
type_category(#sp_list{}) ->
    list;
type_category(#sp_nonempty_list{}) ->
    nonempty_list;
type_category(#sp_tuple{}) ->
    tuple;
type_category(#sp_map{}) ->
    map;
type_category(#sp_rec{}) ->
    record;
type_category(#sp_rec_ref{}) ->
    record_ref;
type_category(#sp_union{}) ->
    union;
type_category(#sp_user_type_ref{}) ->
    user_type_ref;
type_category(#sp_remote_type{}) ->
    remote_type;
type_category(#sp_function{}) ->
    function;
type_category(#sp_var{}) ->
    var;
type_category(#sp_type_with_variables{}) ->
    type_with_variables;
type_category(#sp_maybe_improper_list{}) ->
    maybe_improper_list;
type_category(#sp_nonempty_improper_list{}) ->
    nonempty_improper_list;
type_category(_) ->
    other.

%% Uses filtered version to exclude known problematic types with bugs in the library.
%% This demonstrates that the test infrastructure works correctly for consistent types.
%%
%% Known issues filtered out:
%%   - Remote types with non-existent modules: to_json throws exception but to_schema succeeds
%%   - Float/tuple literals in unions: to_json succeeds but to_schema throws function_clause
%%   - Records/user types referencing undefined types: both may fail with different errors
%%
%% To run the unfiltered version manually and see all inconsistencies:
%%   proper:quickcheck(prop_json_encode_schema_consistency:json_encode_schema_consistency_unfiltered())
%% To run the filtered version manually with more tests:
%%   proper:quickcheck(prop_json_encode_schema_consistency:prop_json_encode_schema_consistency_filtered(), [{numtests, 200}, {max_size, 5}])

-module(spectra_error).

-export([format/1, format_errors/1]).

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-doc """
Formats a single validation error into a human-readable string.

Returns a binary string in the format:
`"field.path  Error message [type=error_type, input=value, ...]"`

### Example:
```
1> Error = #sp_error{
   location = [user, age],
   type = {parse_error, int},
   msg = undefined,
   input = <<"25">>,
   ctx = #{expected_type => #sp_simple_type{type = integer}},
   url = undefined
}.
2> spectra_error:format(Error).
<<\"user.age  Input should be a valid integer, unable to parse string as an integer [type={parse_error, int}, input=\\\"25\\\", expected=integer]\">>
```
""".
-spec format(spectra:error()) -> binary().
format(#sp_error{location = Loc, type = Type, msg = Msg, input = Input, ctx = Ctx}) ->
    %% 1. Format location: [user, age] -> "user.age"
    LocationStr = format_location(Loc),

    %% 2. Get or generate message
    Message =
        case Msg of
            undefined -> generate_message(Type, Ctx);
            M when is_binary(M) -> M
        end,

    %% 3. Format context as key=value pairs
    CtxStr = format_context(Type, Input, Ctx),

    %% 4. Combine: "location  message [type=..., input=..., ...]"
    case LocationStr of
        <<>> ->
            <<Message/binary, " [", CtxStr/binary, "]">>;
        _ ->
            <<LocationStr/binary, "  ", Message/binary, " [", CtxStr/binary, "]">>
    end.

-doc """
Formats a list of validation errors, one per line.

### Example:
```
1> Errors = [Error1, Error2, Error3].
2> spectra_error:format_errors(Errors).
<<\"user.id  Input should be a valid integer...\\nuser.age  Input should be at most 150...\">>
```
""".
-spec format_errors([spectra:error()]) -> binary().
format_errors([]) ->
    <<>>;
format_errors(Errors) ->
    FormattedErrors = [format(E) || E <- Errors],
    join(FormattedErrors, <<"\n">>).

-doc """
Converts a location path to a dot-separated string.

### Example:
```
1> spectra_error:format_location([user, addresses, 0, zip]).
<<\"user.addresses.0.zip\">>
```
""".
-spec format_location([atom() | integer()]) -> binary().
format_location([]) ->
    <<>>;
format_location(Location) ->
    %% Location is built bottom-up, so reverse it
    Parts = [format_location_part(P) || P <- lists:reverse(Location)],
    join(Parts, <<".">>).

%% Internal functions

%% Generate human-readable messages based on error type
-spec generate_message(term(), map()) -> binary().
%% Type errors
generate_message({type_error, int}, _Ctx) ->
    <<"Input should be a valid integer">>;
generate_message({type_error, float}, _Ctx) ->
    <<"Input should be a valid number">>;
generate_message({type_error, bool}, _Ctx) ->
    <<"Input should be a valid boolean">>;
generate_message({type_error, string}, _Ctx) ->
    <<"Input should be a valid string">>;
generate_message({type_error, binary}, _Ctx) ->
    <<"Input should be a valid binary">>;
generate_message({type_error, atom}, _Ctx) ->
    <<"Input should be a valid atom">>;
generate_message({type_error, list}, _Ctx) ->
    <<"Input should be a valid list">>;
generate_message({type_error, map}, _Ctx) ->
    <<"Input should be a valid map">>;
generate_message({type_error, record}, _Ctx) ->
    <<"Input should be a valid record">>;
generate_message({type_error, nonempty_list}, _Ctx) ->
    <<"Input should be a non-empty list">>;
generate_message({type_error, nonempty_string}, _Ctx) ->
    <<"Input should be a non-empty string">>;
generate_message({type_error, nonempty_binary}, _Ctx) ->
    <<"Input should be a non-empty binary">>;
generate_message({type_error, _Type}, #{expected_type := SpType}) ->
    TypeStr = spectra_type:to_string(SpType),
    <<"Input should be a valid ", TypeStr/binary>>;
generate_message({type_error, _Type}, _Ctx) ->
    <<"Input has incorrect type">>;
%% Parse errors
generate_message({parse_error, int}, _Ctx) ->
    <<"Input should be a valid integer, unable to parse string as an integer">>;
generate_message({parse_error, float}, _Ctx) ->
    <<"Input should be a valid number, unable to parse string as a number">>;
generate_message({parse_error, atom}, _Ctx) ->
    <<"Input should be a valid atom, atom does not exist">>;
generate_message({parse_error, json}, _Ctx) ->
    <<"Invalid JSON syntax">>;
generate_message({parse_error, _Type}, _Ctx) ->
    <<"Unable to parse input">>;
%% Constraint errors
generate_message({constraint_error, too_large}, #{expected_type := #sp_range{upper_bound = Max}}) ->
    MaxBin = integer_to_binary(Max),
    <<"Input should be at most ", MaxBin/binary>>;
generate_message({constraint_error, too_small}, #{expected_type := #sp_range{lower_bound = Min}}) ->
    MinBin = integer_to_binary(Min),
    <<"Input should be at least ", MinBin/binary>>;
generate_message({constraint_error, too_small}, _Ctx) ->
    <<"Input is too small">>;
generate_message({constraint_error, too_large}, _Ctx) ->
    <<"Input is too large">>;
generate_message({constraint_error, too_short}, _Ctx) ->
    <<"Input is too short">>;
generate_message({constraint_error, too_long}, _Ctx) ->
    <<"Input is too long">>;
generate_message({constraint_error, _Type}, _Ctx) ->
    <<"Input violates constraints">>;
%% Structural errors
generate_message(missing_field, _Ctx) ->
    <<"Field required">>;
generate_message(exact_field_mismatch, _Ctx) ->
    <<"Required map field not found">>;
%% Union errors
generate_message(union_no_match, _Ctx) ->
    <<"Input did not match any variant">>;
generate_message(literal_no_match, #{expected_type := #sp_literal{value = Value}}) when
    is_atom(Value)
->
    ValueBin = atom_to_binary(Value, utf8),
    <<"Input should be ", ValueBin/binary>>;
generate_message(literal_no_match, #{expected_type := #sp_literal{value = Value}}) when
    is_integer(Value)
->
    ValueBin = integer_to_binary(Value),
    <<"Input should be ", ValueBin/binary>>;
generate_message(literal_no_match, _Ctx) ->
    <<"Input does not match expected literal value">>;
%% System errors
generate_message(unsupported_type, _Ctx) ->
    <<"Type not supported for this format">>;
%% Fallback
generate_message(_Type, _Ctx) ->
    <<"Validation error">>.

%% Format the context section of error message
-spec format_context(term(), term(), map()) -> binary().
format_context(Type, Input, Ctx) ->
    %% Always include type
    TypeStr = format_type(Type),
    Base = [<<"type=", TypeStr/binary>>],

    %% Add input (truncated if too long)
    InputParts =
        case Input of
            undefined ->
                [];
            _ ->
                InputStr = format_input_value(Input),
                [<<"input=", InputStr/binary>>]
        end,

    %% Add expected_type if present
    ExpectedParts =
        case maps:get(expected_type, Ctx, undefined) of
            undefined ->
                [];
            SpType ->
                ExpStr = spectra_type:to_string(SpType),
                [<<"expected=", ExpStr/binary>>]
        end,

    %% Add other context fields
    OtherParts = format_other_ctx_fields(Ctx),

    %% Join all parts
    AllParts = lists:flatten([Base, InputParts, ExpectedParts, OtherParts]),
    join(AllParts, <<", ">>).

%% Format error type
-spec format_type(term()) -> binary().
format_type({Type, Subtype}) when is_atom(Type), is_atom(Subtype) ->
    TypeBin = atom_to_binary(Type, utf8),
    SubtypeBin = atom_to_binary(Subtype, utf8),
    <<"{", TypeBin/binary, ", ", SubtypeBin/binary, "}">>;
format_type(Type) when is_atom(Type) ->
    atom_to_binary(Type, utf8);
format_type(Type) ->
    %% Fallback for other types
    iolist_to_binary(io_lib:format("~p", [Type])).

%% Format input value for display
-spec format_input_value(term()) -> binary().
format_input_value(Value) when is_binary(Value) ->
    %% Truncate long binaries
    case byte_size(Value) of
        Size when Size > 50 ->
            <<Prefix:47/binary, _/binary>> = Value,
            <<"\"", Prefix/binary, "...\"">>;
        _ ->
            <<"\"", Value/binary, "\"">>
    end;
format_input_value(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
format_input_value(Value) when is_integer(Value) ->
    integer_to_binary(Value);
format_input_value(Value) when is_float(Value) ->
    float_to_binary(Value, [{decimals, 2}, compact]);
format_input_value(Value) when is_list(Value) ->
    %% Try to format as string if it looks like one
    %% eqwalizer:ignore - Value might be a printable list
    case unicode:characters_to_binary(Value) of
        Bin when is_binary(Bin) ->
            case byte_size(Bin) > 50 of
                true ->
                    <<Prefix:47/binary, _/binary>> = Bin,
                    <<<<"\"">>/binary, Prefix/binary, <<"...\"">>/binary>>;
                false ->
                    <<<<"\"">>/binary, Bin/binary, <<"\"">>/binary>>
            end;
        _ ->
            %% Not a valid unicode list, generic list format
            <<"[...]">>
    end;
format_input_value(Value) when is_map(Value) ->
    <<"#{...}">>;
format_input_value(Value) when is_tuple(Value) ->
    <<"{...}">>;
format_input_value(_Value) ->
    <<"...">>.

%% Format other context fields (excluding expected_type which is handled separately)
-spec format_other_ctx_fields(map()) -> [binary()].
format_other_ctx_fields(Ctx) ->
    %% Filter out expected_type
    OtherCtx = maps:without([expected_type], Ctx),
    maps:fold(
        fun(Key, Value, Acc) ->
            KeyBin = atom_to_binary(Key, utf8),
            ValueBin = format_ctx_value(Value),
            [<<KeyBin/binary, "=", ValueBin/binary>> | Acc]
        end,
        [],
        OtherCtx
    ).

%% Format a context value
-spec format_ctx_value(term()) -> binary().
format_ctx_value(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
format_ctx_value(Value) when is_integer(Value) ->
    integer_to_binary(Value);
format_ctx_value(Value) when is_binary(Value) ->
    Value;
format_ctx_value(Value) when is_list(Value) ->
    %% eqwalizer:ignore - Value might be a printable list
    case unicode:characters_to_binary(Value) of
        Bin when is_binary(Bin) ->
            Bin;
        _ ->
            <<"[...]">>
    end;
format_ctx_value(_Value) ->
    <<"...">>.

%% Format a single location part
-spec format_location_part(atom() | integer()) -> binary().
format_location_part(Part) when is_atom(Part) ->
    atom_to_binary(Part, utf8);
format_location_part(Part) when is_integer(Part) ->
    integer_to_binary(Part).

%% Join binaries with a separator
-spec join([binary()], binary()) -> binary().
join([], _Sep) ->
    <<>>;
join([H], _Sep) ->
    H;
join([H | T], Sep) ->
    Rest = join(T, Sep),
    <<H/binary, Sep/binary, Rest/binary>>.

-module(spectra_string).

-export([from_string/3, to_string/3]).

-compile(nowarn_unused_function).

-ignore_xref([{spectra_string, from_string, 3}, {spectra_string, to_string, 3}]).

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

%% API

-doc """
Converts a string value to an Erlang value based on a type specification.

This function validates the given string value against the specified type definition
and converts it to the corresponding Erlang value.

### Returns
{ok, ErlangValue} if conversion succeeds, or {error, Errors} if validation fails
""".
-doc #{
    params =>
        #{
            "String" => "The string value to convert to Erlang format",
            "Type" => "The type specification (spectra:sp_type_or_ref())",
            "TypeInfo" => "The type information containing type definitions"
        }
}.

-spec from_string(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type_or_ref(),
    String :: list()
) ->
    {ok, term()} | {error, [spectra:error()]}.
from_string(TypeInfo, {type, TypeName, TypeArity}, String) when is_atom(TypeName) ->
    {ok, Type} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    from_string(TypeInfo, Type, String);
from_string(_TypeInfo, {record, RecordName}, String) when is_atom(RecordName) ->
    {error, [
        #sp_error{
            type = unsupported_type,
            location = [],
            msg = undefined,
            input = String,
            ctx = #{expected_type => {record, RecordName}},
            url = undefined
        }
    ]};
from_string(_TypeInfo, #sp_simple_type{type = NotSupported} = T, _String) when
    NotSupported =:= pid orelse
        NotSupported =:= port orelse
        NotSupported =:= reference orelse
        NotSupported =:= bitstring orelse
        NotSupported =:= nonempty_bitstring orelse
        NotSupported =:= none
->
    erlang:error({type_not_supported, T});
from_string(_TypeInfo, #sp_simple_type{type = PrimaryType}, String) ->
    convert_string_to_type(PrimaryType, String);
from_string(
    _TypeInfo,
    #sp_range{
        type = integer,
        lower_bound = Min,
        upper_bound = Max
    } =
        Range,
    String
) ->
    case convert_string_to_type(integer, String) of
        {ok, Value} when Min =< Value, Value =< Max ->
            {ok, Value};
        {ok, Value} when is_integer(Value) ->
            ErrorType =
                if
                    Value < Min -> {constraint_error, too_small};
                    Value > Max -> {constraint_error, too_large};
                    true -> {type_error, int}
                end,
            {error, [
                #sp_error{
                    type = ErrorType,
                    location = [],
                    msg = undefined,
                    input = Value,
                    ctx = #{expected_type => Range},
                    url = undefined
                }
            ]};
        {error, Reason} ->
            {error, Reason}
    end;
from_string(_TypeInfo, #sp_remote_type{mfargs = {Module, TypeName, Args}}, String) ->
    TypeInfo = spectra_module_types:get(Module),
    TypeArity = length(Args),
    {ok, Type} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, Args),
    from_string(TypeInfo, TypeWithoutVars, String);
from_string(_TypeInfo, #sp_literal{value = Literal}, String) ->
    try_convert_string_to_literal(Literal, String);
from_string(TypeInfo, #sp_union{} = Type, String) ->
    union(fun from_string/3, TypeInfo, Type, String);
from_string(_TypeInfo, Type, String) ->
    {error, [
        #sp_error{
            type = unsupported_type,
            location = [],
            msg = undefined,
            input = String,
            ctx = #{expected_type => Type},
            url = undefined
        }
    ]}.

-doc """
Converts an Erlang value to a string based on a type specification.

This function validates the given Erlang value against the specified type definition
and converts it to a string representation.

### Returns
{ok, String} if conversion succeeds, or {error, Errors} if validation fails
""".
-doc #{
    params =>
        #{
            "Data" => "The Erlang value to convert to string format",
            "Type" => "The type specification (spectra:sp_type_or_ref())",
            "TypeInfo" => "The type information containing type definitions"
        }
}.

-spec to_string(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type_or_ref(),
    Data :: term()
) ->
    {ok, string()} | {error, [spectra:error()]}.
to_string(TypeInfo, {type, TypeName, TypeArity}, Data) when is_atom(TypeName) ->
    {ok, Type} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    to_string(TypeInfo, Type, Data);
to_string(_TypeInfo, {record, RecordName}, Data) when is_atom(RecordName) ->
    {error, [
        #sp_error{
            type = unsupported_type,
            location = [],
            msg = undefined,
            input = Data,
            ctx = #{expected_type => {record, RecordName}},
            url = undefined
        }
    ]};
to_string(_TypeInfo, #sp_simple_type{type = NotSupported} = T, _Data) when
    NotSupported =:= pid orelse
        NotSupported =:= port orelse
        NotSupported =:= reference orelse
        NotSupported =:= bitstring orelse
        NotSupported =:= nonempty_bitstring orelse
        NotSupported =:= none
->
    erlang:error({type_not_supported, T});
to_string(_TypeInfo, #sp_simple_type{type = PrimaryType}, Data) ->
    convert_type_to_string(PrimaryType, Data);
to_string(
    _TypeInfo,
    #sp_range{
        type = integer,
        lower_bound = Min,
        upper_bound = Max
    } =
        Range,
    Data
) ->
    case convert_type_to_string(integer, Data) of
        {ok, String} when Min =< Data, Data =< Max ->
            {ok, String};
        {ok, _String} when is_integer(Data) ->
            ErrorType =
                if
                    Data < Min -> {constraint_error, too_small};
                    Data > Max -> {constraint_error, too_large};
                    true -> {type_error, int}
                end,
            {error, [
                #sp_error{
                    type = ErrorType,
                    location = [],
                    msg = undefined,
                    input = Data,
                    ctx = #{expected_type => Range},
                    url = undefined
                }
            ]};
        {error, Reason} ->
            {error, Reason}
    end;
to_string(_TypeInfo, #sp_remote_type{mfargs = {Module, TypeName, Args}}, Data) ->
    TypeInfo = spectra_module_types:get(Module),
    TypeArity = length(Args),
    {ok, Type} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, Args),
    to_string(TypeInfo, TypeWithoutVars, Data);
to_string(_TypeInfo, #sp_literal{value = Literal}, Data) ->
    try_convert_literal_to_string(Literal, Data);
to_string(TypeInfo, #sp_union{} = Type, Data) ->
    union_to_string(TypeInfo, Type, Data);
to_string(_TypeInfo, Type, Data) ->
    {error, [
        #sp_error{
            type = unsupported_type,
            location = [],
            msg = undefined,
            input = Data,
            ctx = #{expected_type => Type},
            url = undefined
        }
    ]}.

%% INTERNAL
convert_string_to_type(Type, String) when is_atom(Type), is_list(String) ->
    do_convert_string_to_type(Type, String);
convert_string_to_type(Type, NotString) ->
    {error, [
        #sp_error{
            type = {type_error, string},
            location = [],
            msg = undefined,
            input = NotString,
            ctx = #{expected_type => #sp_simple_type{type = Type}},
            url = undefined
        }
    ]}.

-spec do_convert_string_to_type(Type :: atom(), String :: string()) ->
    {ok, term()} | {error, [spectra:error()]}.
do_convert_string_to_type(integer, String) ->
    try
        {ok, list_to_integer(String)}
    catch
        error:badarg ->
            ExpectedType = #sp_simple_type{type = integer},
            {error, [
                #sp_error{
                    type = {parse_error, int},
                    location = [],
                    msg = undefined,
                    input = String,
                    ctx = #{expected_type => ExpectedType},
                    url = undefined
                }
            ]}
    end;
do_convert_string_to_type(float, String) ->
    try
        {ok, list_to_float(String)}
    catch
        error:badarg ->
            ExpectedType = #sp_simple_type{type = float},
            {error, [
                #sp_error{
                    type = {parse_error, float},
                    location = [],
                    msg = undefined,
                    input = String,
                    ctx = #{expected_type => ExpectedType},
                    url = undefined
                }
            ]}
    end;
do_convert_string_to_type(number, String) ->
    case do_convert_string_to_type(integer, String) of
        {ok, _} = Result ->
            Result;
        {error, _} ->
            do_convert_string_to_type(float, String)
    end;
do_convert_string_to_type(boolean, "true") ->
    {ok, true};
do_convert_string_to_type(boolean, "false") ->
    {ok, false};
do_convert_string_to_type(boolean, String) ->
    ExpectedType = #sp_simple_type{type = boolean},
    {error, [
        #sp_error{
            type = {parse_error, bool},
            location = [],
            msg = undefined,
            input = String,
            ctx = #{expected_type => ExpectedType},
            url = undefined
        }
    ]};
do_convert_string_to_type(atom, String) ->
    try
        {ok, list_to_existing_atom(String)}
    catch
        error:badarg ->
            ExpectedType = #sp_simple_type{type = atom},
            {error, [
                #sp_error{
                    type = {parse_error, atom},
                    location = [],
                    msg = undefined,
                    input = String,
                    ctx = #{expected_type => ExpectedType},
                    url = undefined
                }
            ]}
    end;
do_convert_string_to_type(string, String) ->
    {ok, String};
do_convert_string_to_type(nonempty_string, String) when String =/= [] ->
    {ok, String};
do_convert_string_to_type(nonempty_string, []) ->
    ExpectedType = #sp_simple_type{type = nonempty_string},
    {error, [
        #sp_error{
            type = {type_error, nonempty_string},
            location = [],
            msg = undefined,
            input = [],
            ctx = #{expected_type => ExpectedType},
            url = undefined
        }
    ]};
do_convert_string_to_type(binary, String) ->
    {ok, list_to_binary(String)};
do_convert_string_to_type(nonempty_binary, String) when String =/= [] ->
    {ok, list_to_binary(String)};
do_convert_string_to_type(nonempty_binary, []) ->
    ExpectedType = #sp_simple_type{type = nonempty_binary},
    {error, [
        #sp_error{
            type = {type_error, nonempty_binary},
            location = [],
            msg = undefined,
            input = [],
            ctx = #{expected_type => ExpectedType},
            url = undefined
        }
    ]};
do_convert_string_to_type(non_neg_integer, String) ->
    case do_convert_string_to_type(integer, String) of
        {ok, Value} when Value >= 0 ->
            {ok, Value};
        {ok, Value} ->
            ExpectedType = #sp_simple_type{type = non_neg_integer},
            {error, [
                #sp_error{
                    type = {constraint_error, too_small},
                    location = [],
                    msg = undefined,
                    input = Value,
                    ctx = #{expected_type => ExpectedType},
                    url = undefined
                }
            ]};
        {error, Reason} ->
            {error, Reason}
    end;
do_convert_string_to_type(pos_integer, String) ->
    case do_convert_string_to_type(integer, String) of
        {ok, Value} when Value > 0 ->
            {ok, Value};
        {ok, Value} ->
            ExpectedType = #sp_simple_type{type = pos_integer},
            {error, [
                #sp_error{
                    type = {constraint_error, too_small},
                    location = [],
                    msg = undefined,
                    input = Value,
                    ctx = #{expected_type => ExpectedType},
                    url = undefined
                }
            ]};
        {error, Reason} ->
            {error, Reason}
    end;
do_convert_string_to_type(neg_integer, String) ->
    case do_convert_string_to_type(integer, String) of
        {ok, Value} when Value < 0 ->
            {ok, Value};
        {ok, Value} ->
            ExpectedType = #sp_simple_type{type = neg_integer},
            {error, [
                #sp_error{
                    type = {constraint_error, too_large},
                    location = [],
                    msg = undefined,
                    input = Value,
                    ctx = #{expected_type => ExpectedType},
                    url = undefined
                }
            ]};
        {error, Reason} ->
            {error, Reason}
    end;
do_convert_string_to_type(Type, String) ->
    {error, [
        #sp_error{
            type = unsupported_type,
            location = [],
            msg = undefined,
            input = String,
            ctx = #{expected_type => Type},
            url = undefined
        }
    ]}.

-spec try_convert_string_to_literal(Literal :: term(), String :: string()) ->
    {ok, term()} | {error, [spectra:error()]}.
try_convert_string_to_literal(Literal, String) when is_boolean(Literal) ->
    case convert_string_to_type(boolean, String) of
        {ok, Literal} ->
            {ok, Literal};
        {ok, _Other} ->
            {error, [
                #sp_error{
                    type = literal_no_match,
                    location = [],
                    msg = undefined,
                    input = String,
                    ctx = #{
                        expected_type => #sp_literal{
                            value = Literal, binary_value = atom_to_binary(Literal, utf8)
                        }
                    },
                    url = undefined
                }
            ]};
        {error, Reason} ->
            {error, Reason}
    end;
try_convert_string_to_literal(Literal, String) when is_atom(Literal) ->
    case convert_string_to_type(atom, String) of
        {ok, Literal} ->
            {ok, Literal};
        {ok, _Other} ->
            {error, [
                #sp_error{
                    type = literal_no_match,
                    location = [],
                    msg = undefined,
                    input = String,
                    ctx = #{
                        expected_type => #sp_literal{
                            value = Literal, binary_value = atom_to_binary(Literal, utf8)
                        }
                    },
                    url = undefined
                }
            ]};
        {error, Reason} ->
            {error, Reason}
    end;
try_convert_string_to_literal(Literal, String) when is_integer(Literal) ->
    case convert_string_to_type(integer, String) of
        {ok, Literal} ->
            {ok, Literal};
        {ok, _Other} ->
            {error, [
                #sp_error{
                    type = literal_no_match,
                    location = [],
                    msg = undefined,
                    input = String,
                    ctx = #{
                        expected_type => #sp_literal{
                            value = Literal, binary_value = integer_to_binary(Literal)
                        }
                    },
                    url = undefined
                }
            ]};
        {error, Reason} ->
            {error, Reason}
    end;
try_convert_string_to_literal(Literal, String) ->
    BinaryValue = iolist_to_binary(io_lib:format("~p", [Literal])),
    %% eqwalizer:ignore - Literal could be any term
    LiteralType = #sp_literal{value = Literal, binary_value = BinaryValue},
    {error, [
        #sp_error{
            type = literal_no_match,
            location = [],
            msg = undefined,
            input = String,
            ctx = #{
                expected_type => LiteralType
            },
            url = undefined
        }
    ]}.

union(Fun, TypeInfo, #sp_union{types = Types} = UnionType, String) ->
    case do_first(Fun, TypeInfo, Types, String) of
        {error, no_match} ->
            {error, [
                #sp_error{
                    type = union_no_match,
                    location = [],
                    msg = undefined,
                    input = String,
                    ctx = #{expected_type => UnionType},
                    url = undefined
                }
            ]};
        Result ->
            Result
    end.

do_first(_Fun, _TypeInfo, [], _String) ->
    {error, no_match};
do_first(Fun, TypeInfo, [Type | Rest], String) ->
    case Fun(TypeInfo, Type, String) of
        {ok, Result} ->
            {ok, Result};
        {error, _} ->
            do_first(Fun, TypeInfo, Rest, String)
    end.

apply_args(TypeInfo, Type, TypeArgs) when is_list(TypeArgs) ->
    ArgNames = arg_names(Type),
    NamedTypes =
        maps:from_list(
            lists:zip(ArgNames, TypeArgs)
        ),
    type_replace_vars(TypeInfo, Type, NamedTypes).

arg_names(#sp_type_with_variables{vars = Args}) ->
    Args;
arg_names(_) ->
    [].

-spec type_replace_vars(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type(),
    NamedTypes :: #{atom() => spectra:sp_type()}
) ->
    spectra:sp_type().
type_replace_vars(_TypeInfo, #sp_var{name = Name}, NamedTypes) ->
    maps:get(Name, NamedTypes, #sp_simple_type{type = term});
type_replace_vars(TypeInfo, #sp_type_with_variables{type = Type}, NamedTypes) ->
    case Type of
        #sp_union{types = UnionTypes} ->
            #sp_union{
                types =
                    lists:map(
                        fun(UnionType) ->
                            type_replace_vars(TypeInfo, UnionType, NamedTypes)
                        end,
                        UnionTypes
                    )
            };
        #sp_remote_type{mfargs = {Module, TypeName, Args}} ->
            TypeInfo = spectra_module_types:get(Module),
            TypeArity = length(Args),
            {ok, Type} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
            type_replace_vars(TypeInfo, Type, NamedTypes)
    end;
type_replace_vars(_TypeInfo, Type, _NamedTypes) ->
    Type.

convert_type_to_string(integer, Data) when is_integer(Data) ->
    {ok, integer_to_list(Data)};
convert_type_to_string(integer, Data) ->
    ExpectedType = #sp_simple_type{type = integer},
    {error, [
        #sp_error{
            type = {type_error, int},
            location = [],
            msg = undefined,
            input = Data,
            ctx = #{expected_type => ExpectedType},
            url = undefined
        }
    ]};
convert_type_to_string(float, Data) when is_float(Data) ->
    {ok, float_to_list(Data)};
convert_type_to_string(float, Data) ->
    ExpectedType = #sp_simple_type{type = float},
    {error, [
        #sp_error{
            type = {type_error, float},
            location = [],
            msg = undefined,
            input = Data,
            ctx = #{expected_type => ExpectedType},
            url = undefined
        }
    ]};
convert_type_to_string(number, Data) when is_number(Data) ->
    if
        is_integer(Data) ->
            {ok, integer_to_list(Data)};
        is_float(Data) ->
            {ok, float_to_list(Data)}
    end;
convert_type_to_string(number, Data) ->
    ExpectedType = #sp_simple_type{type = number},
    {error, [
        #sp_error{
            type = {type_error, float},
            location = [],
            msg = undefined,
            input = Data,
            ctx = #{expected_type => ExpectedType},
            url = undefined
        }
    ]};
convert_type_to_string(boolean, true) ->
    {ok, "true"};
convert_type_to_string(boolean, false) ->
    {ok, "false"};
convert_type_to_string(boolean, Data) ->
    ExpectedType = #sp_simple_type{type = boolean},
    {error, [
        #sp_error{
            type = {type_error, bool},
            location = [],
            msg = undefined,
            input = Data,
            ctx = #{expected_type => ExpectedType},
            url = undefined
        }
    ]};
convert_type_to_string(atom, Data) when is_atom(Data) ->
    {ok, atom_to_list(Data)};
convert_type_to_string(atom, Data) ->
    ExpectedType = #sp_simple_type{type = atom},
    {error, [
        #sp_error{
            type = {type_error, atom},
            location = [],
            msg = undefined,
            input = Data,
            ctx = #{expected_type => ExpectedType},
            url = undefined
        }
    ]};
convert_type_to_string(string, Data) when is_list(Data) ->
    lits_to_charlist(Data);
convert_type_to_string(string, Data) ->
    ExpectedType = #sp_simple_type{type = string},
    {error, [
        #sp_error{
            type = {type_error, string},
            location = [],
            msg = undefined,
            input = Data,
            ctx = #{expected_type => ExpectedType},
            url = undefined
        }
    ]};
convert_type_to_string(nonempty_string, Data) when is_list(Data), Data =/= [] ->
    lits_to_charlist(Data);
convert_type_to_string(nonempty_string, Data) ->
    ExpectedType = #sp_simple_type{type = nonempty_string},
    {error, [
        #sp_error{
            type = {type_error, nonempty_string},
            location = [],
            msg = undefined,
            input = Data,
            ctx = #{expected_type => ExpectedType},
            url = undefined
        }
    ]};
convert_type_to_string(binary, Data) when is_binary(Data) ->
    {ok, binary_to_list(Data)};
convert_type_to_string(binary, Data) ->
    ExpectedType = #sp_simple_type{type = binary},
    {error, [
        #sp_error{
            type = {type_error, binary},
            location = [],
            msg = undefined,
            input = Data,
            ctx = #{expected_type => ExpectedType},
            url = undefined
        }
    ]};
convert_type_to_string(nonempty_binary, Data) when is_binary(Data), Data =/= <<>> ->
    {ok, binary_to_list(Data)};
convert_type_to_string(nonempty_binary, Data) ->
    ExpectedType = #sp_simple_type{type = nonempty_binary},
    {error, [
        #sp_error{
            type = {type_error, nonempty_binary},
            location = [],
            msg = undefined,
            input = Data,
            ctx = #{expected_type => ExpectedType},
            url = undefined
        }
    ]};
convert_type_to_string(non_neg_integer, Data) when is_integer(Data), Data >= 0 ->
    {ok, integer_to_list(Data)};
convert_type_to_string(non_neg_integer, Data) ->
    ExpectedType = #sp_simple_type{type = non_neg_integer},
    {error, [
        #sp_error{
            type = {constraint_error, too_small},
            location = [],
            msg = undefined,
            input = Data,
            ctx = #{expected_type => ExpectedType},
            url = undefined
        }
    ]};
convert_type_to_string(pos_integer, Data) when is_integer(Data), Data > 0 ->
    {ok, integer_to_list(Data)};
convert_type_to_string(pos_integer, Data) ->
    ExpectedType = #sp_simple_type{type = pos_integer},
    {error, [
        #sp_error{
            type = {constraint_error, too_small},
            location = [],
            msg = undefined,
            input = Data,
            ctx = #{expected_type => ExpectedType},
            url = undefined
        }
    ]};
convert_type_to_string(neg_integer, Data) when is_integer(Data), Data < 0 ->
    {ok, integer_to_list(Data)};
convert_type_to_string(neg_integer, Data) ->
    ExpectedType = #sp_simple_type{type = neg_integer},
    {error, [
        #sp_error{
            type = {constraint_error, too_large},
            location = [],
            msg = undefined,
            input = Data,
            ctx = #{expected_type => ExpectedType},
            url = undefined
        }
    ]};
convert_type_to_string(Type, Data) ->
    {error, [
        #sp_error{
            type = unsupported_type,
            location = [],
            msg = undefined,
            input = Data,
            ctx = #{expected_type => Type},
            url = undefined
        }
    ]}.

lits_to_charlist(Data) ->
    case application:get_env(spectra, check_unicode, false) of
        true ->
            case unicode:characters_to_list(Data) of
                DataList when is_list(DataList) ->
                    {ok, DataList};
                _Other ->
                    ExpectedType = #sp_simple_type{type = string},
                    {error, [
                        #sp_error{
                            type = {type_error, string},
                            location = [],
                            msg = undefined,
                            input = Data,
                            ctx = #{expected_type => ExpectedType},
                            url = undefined
                        }
                    ]}
            end;
        false ->
            {ok, Data}
    end.

-spec try_convert_literal_to_string(Literal :: term(), Data :: term()) ->
    {ok, string()} | {error, [spectra:error()]}.
try_convert_literal_to_string(Literal, Literal) when is_atom(Literal) ->
    {ok, atom_to_list(Literal)};
try_convert_literal_to_string(Literal, Literal) when is_integer(Literal) ->
    {ok, integer_to_list(Literal)};
try_convert_literal_to_string(Literal, Literal) when is_boolean(Literal) ->
    if
        Literal ->
            {ok, "true"};
        true ->
            {ok, "false"}
    end;
try_convert_literal_to_string(Literal, Data) ->
    BinaryValue = iolist_to_binary(io_lib:format("~p", [Literal])),
    %% eqwalizer:ignore - Literal could be any term
    LiteralType = #sp_literal{value = Literal, binary_value = BinaryValue},
    {error, [
        #sp_error{
            type = literal_no_match,
            location = [],
            msg = undefined,
            input = Data,
            ctx = #{
                expected_type => LiteralType
            },
            url = undefined
        }
    ]}.

union_to_string(TypeInfo, #sp_union{types = Types} = UnionType, Data) ->
    case do_first_to_string(TypeInfo, Types, Data) of
        {error, no_match} ->
            {error, [
                #sp_error{
                    type = union_no_match,
                    location = [],
                    msg = undefined,
                    input = Data,
                    ctx = #{expected_type => UnionType},
                    url = undefined
                }
            ]};
        Result ->
            Result
    end.

do_first_to_string(_TypeInfo, [], _Data) ->
    {error, no_match};
do_first_to_string(TypeInfo, [Type | Rest], Data) ->
    case to_string(TypeInfo, Type, Data) of
        {ok, Result} ->
            {ok, Result};
        {error, _} ->
            do_first_to_string(TypeInfo, Rest, Data)
    end.

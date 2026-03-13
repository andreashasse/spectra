-module(spectra_binary_string).

-export([from_binary_string/3, from_binary_string/4, to_binary_string/3, to_binary_string/4]).

-ignore_xref([
    {spectra_binary_string, from_binary_string, 3},
    {spectra_binary_string, from_binary_string, 4},
    {spectra_binary_string, to_binary_string, 3},
    {spectra_binary_string, to_binary_string, 4}
]).

-include("../include/spectra_internal.hrl").

%% API

-doc """
Converts a binary string value to an Erlang value based on a type specification.

This function validates the given binary string value against the specified type definition
and converts it to the corresponding Erlang value.

Equivalent to calling from_binary_string/4 with an empty options map.

### Returns
{ok, ErlangValue} if conversion succeeds, or {error, Errors} if validation fails
""".
-doc #{
    equiv => from_binary_string(TypeInfo, Type, BinaryString, #{}),
    params =>
        #{
            "BinaryString" => "The binary string value to convert to Erlang format",
            "Type" => "The type specification (spectra:sp_type())",
            "TypeInfo" => "The type information containing type definitions"
        }
}.

-spec from_binary_string(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type(),
    BinaryString :: binary()
) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
from_binary_string(TypeInfo, Type, BinaryString) ->
    from_binary_string(TypeInfo, Type, BinaryString, #{}).

-doc """
Converts a binary string value to an Erlang value based on a type specification.

This function validates the given binary string value against the specified type definition
and converts it to the corresponding Erlang value.

### Returns
{ok, ErlangValue} if conversion succeeds, or {error, Errors} if validation fails
""".
-doc #{
    params =>
        #{
            "BinaryString" => "The binary string value to convert to Erlang format",
            "Opts" => "Decode options",
            "Type" => "The type specification (spectra:sp_type())",
            "TypeInfo" => "The type information containing type definitions"
        }
}.

-spec from_binary_string(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type(),
    BinaryString :: binary(),
    Opts :: spectra:binary_string_decode_opts()
) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
from_binary_string(
    TypeInfo, #sp_user_type_ref{type_name = N, variables = Args}, BinaryString, Opts
) ->
    Arity = length(Args),
    Mod = spectra_type_info:get_module(TypeInfo),
    Type = spectra_type_info:get_type(TypeInfo, N, Arity),
    case spectra_type_info:find_codec(Mod, N, Arity) of
        {ok, M} ->
            Params = spectra_type:parameters(Type),
            case M:decode(binary_string, {type, N, Arity}, BinaryString, Params) of
                continue ->
                    TypeWithoutVars = apply_args(TypeInfo, Type, Args),
                    from_binary_string(TypeInfo, TypeWithoutVars, BinaryString, Opts);
                Result ->
                    Result
            end;
        error ->
            TypeWithoutVars = apply_args(TypeInfo, Type, Args),
            from_binary_string(TypeInfo, TypeWithoutVars, BinaryString, Opts)
    end;
from_binary_string(
    _TypeInfo, #sp_remote_type{mfargs = {Module, TypeName, Args}}, BinaryString, Opts
) ->
    TypeArity = length(Args),
    {RemoteTypeInfo, TypeWithoutVars} = resolve_remote_type(Module, TypeName, Args),
    case spectra_type_info:find_codec(Module, TypeName, TypeArity) of
        {ok, M} ->
            Params = spectra_type:parameters(TypeWithoutVars),
            case M:decode(binary_string, {type, TypeName, TypeArity}, BinaryString, Params) of
                continue ->
                    from_binary_string(RemoteTypeInfo, TypeWithoutVars, BinaryString, Opts);
                Result ->
                    Result
            end;
        error ->
            from_binary_string(RemoteTypeInfo, TypeWithoutVars, BinaryString, Opts)
    end;
from_binary_string(TypeInfo, #sp_rec_ref{record_name = RecordName} = RecRef, BinaryString, Opts) ->
    Mod = spectra_type_info:get_module(TypeInfo),
    case spectra_type_info:find_codec_for_record(Mod, RecordName) of
        {ok, M} ->
            RecordType = spectra_type_info:get_record(TypeInfo, RecordName),
            Params = spectra_type:parameters(RecordType),
            case M:decode(binary_string, {record, RecordName}, BinaryString, Params) of
                continue -> from_binary_string_inner(TypeInfo, RecRef, BinaryString, Opts);
                Result -> Result
            end;
        error ->
            from_binary_string_inner(TypeInfo, RecRef, BinaryString, Opts)
    end;
from_binary_string(TypeInfo, Type, BinaryString, Opts) ->
    from_binary_string_inner(TypeInfo, Type, BinaryString, Opts).

-spec from_binary_string_inner(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type(),
    BinaryString :: binary(),
    Opts :: spectra:binary_string_decode_opts()
) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
from_binary_string_inner(
    _TypeInfo, #sp_simple_type{type = NotSupported} = T, _BinaryString, _Opts
) when
    NotSupported =:= pid orelse
        NotSupported =:= port orelse
        NotSupported =:= reference orelse
        NotSupported =:= bitstring orelse
        NotSupported =:= nonempty_bitstring orelse
        NotSupported =:= none
->
    erlang:error({type_not_supported, T});
from_binary_string_inner(_TypeInfo, #sp_simple_type{type = PrimaryType}, BinaryString, _Opts) ->
    convert_binary_string_to_type(PrimaryType, BinaryString);
from_binary_string_inner(
    _TypeInfo,
    #sp_range{
        type = integer,
        lower_bound = Min,
        upper_bound = Max
    } =
        Range,
    BinaryString,
    _Opts
) ->
    case convert_binary_string_to_type(integer, BinaryString) of
        {ok, Value} when Min =< Value, Value =< Max ->
            {ok, Value};
        {ok, Value} when is_integer(Value) ->
            {error, [sp_error:type_mismatch(Range, Value)]};
        {error, Reason} ->
            {error, Reason}
    end;
from_binary_string_inner(_TypeInfo, #sp_literal{value = Literal}, BinaryString, _Opts) ->
    try_convert_binary_string_to_literal(Literal, BinaryString);
from_binary_string_inner(TypeInfo, #sp_union{} = Type, BinaryString, Opts) ->
    union(fun from_binary_string/4, TypeInfo, Type, BinaryString, Opts);
from_binary_string_inner(_TypeInfo, #sp_rec{} = T, _BinaryString, _Opts) ->
    erlang:error({type_not_supported, T});
from_binary_string_inner(_TypeInfo, Type, BinaryString, _Opts) ->
    {error, [sp_error:type_mismatch(Type, BinaryString)]}.

-doc """
Converts an Erlang value to a binary string based on a type specification.

This function validates the given Erlang value against the specified type definition
and converts it to a binary string representation.

Equivalent to calling to_binary_string/4 with an empty options map.

### Returns
{ok, BinaryString} if conversion succeeds, or {error, Errors} if validation fails
""".
-doc #{
    equiv => to_binary_string(TypeInfo, Type, Data, #{}),
    params =>
        #{
            "Data" => "The Erlang value to convert to binary string format",
            "Type" => "The type specification (spectra:sp_type())",
            "TypeInfo" => "The type information containing type definitions"
        }
}.

-spec to_binary_string(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type(),
    Data :: dynamic()
) ->
    {ok, binary()} | {error, [spectra:error()]}.
to_binary_string(TypeInfo, Type, Data) ->
    to_binary_string(TypeInfo, Type, Data, #{}).

-doc """
Converts an Erlang value to a binary string based on a type specification.

This function validates the given Erlang value against the specified type definition
and converts it to a binary string representation.

### Returns
{ok, BinaryString} if conversion succeeds, or {error, Errors} if validation fails
""".
-doc #{
    params =>
        #{
            "Data" => "The Erlang value to convert to binary string format",
            "Opts" => "Encode options",
            "Type" => "The type specification (spectra:sp_type())",
            "TypeInfo" => "The type information containing type definitions"
        }
}.

-spec to_binary_string(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type(),
    Data :: dynamic(),
    Opts :: spectra:binary_string_encode_opts()
) ->
    {ok, binary()} | {error, [spectra:error()]}.
to_binary_string(TypeInfo, #sp_user_type_ref{type_name = TypeName, variables = Args}, Data, Opts) ->
    Arity = length(Args),
    Mod = spectra_type_info:get_module(TypeInfo),
    Type = spectra_type_info:get_type(TypeInfo, TypeName, Arity),
    case spectra_type_info:find_codec(Mod, TypeName, Arity) of
        {ok, M} ->
            Params = spectra_type:parameters(Type),
            case M:encode(binary_string, {type, TypeName, Arity}, Data, Params) of
                continue ->
                    TypeWithoutVars = apply_args(TypeInfo, Type, Args),
                    to_binary_string(TypeInfo, TypeWithoutVars, Data, Opts);
                Result ->
                    Result
            end;
        error ->
            TypeWithoutVars = apply_args(TypeInfo, Type, Args),
            to_binary_string(TypeInfo, TypeWithoutVars, Data, Opts)
    end;
to_binary_string(
    _TypeInfo, #sp_remote_type{mfargs = {Module, TypeName, Args}}, Data, Opts
) ->
    TypeArity = length(Args),
    {RemoteTypeInfo, TypeWithoutVars} = resolve_remote_type(Module, TypeName, Args),
    case spectra_type_info:find_codec(Module, TypeName, TypeArity) of
        {ok, M} ->
            Params = spectra_type:parameters(TypeWithoutVars),
            case M:encode(binary_string, {type, TypeName, TypeArity}, Data, Params) of
                continue ->
                    to_binary_string(RemoteTypeInfo, TypeWithoutVars, Data, Opts);
                Result ->
                    Result
            end;
        error ->
            to_binary_string(RemoteTypeInfo, TypeWithoutVars, Data, Opts)
    end;
to_binary_string(TypeInfo, #sp_rec_ref{record_name = RecordName} = RecRef, Data, Opts) ->
    Mod = spectra_type_info:get_module(TypeInfo),
    case spectra_type_info:find_codec_for_record(Mod, RecordName) of
        {ok, M} ->
            RecordType = spectra_type_info:get_record(TypeInfo, RecordName),
            Params = spectra_type:parameters(RecordType),
            case M:encode(binary_string, {record, RecordName}, Data, Params) of
                continue -> to_binary_string_inner(TypeInfo, RecRef, Data, Opts);
                Result -> Result
            end;
        error ->
            to_binary_string_inner(TypeInfo, RecRef, Data, Opts)
    end;
to_binary_string(TypeInfo, Type, Data, Opts) ->
    to_binary_string_inner(TypeInfo, Type, Data, Opts).

-spec to_binary_string_inner(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type(),
    Data :: dynamic(),
    Opts :: spectra:binary_string_encode_opts()
) ->
    {ok, binary()} | {error, [spectra:error()]}.
to_binary_string_inner(_TypeInfo, #sp_simple_type{type = NotSupported} = T, _Data, _Opts) when
    NotSupported =:= pid orelse
        NotSupported =:= port orelse
        NotSupported =:= reference orelse
        NotSupported =:= bitstring orelse
        NotSupported =:= nonempty_bitstring orelse
        NotSupported =:= none
->
    erlang:error({type_not_supported, T});
to_binary_string_inner(_TypeInfo, #sp_simple_type{type = PrimaryType}, Data, _Opts) ->
    convert_type_to_binary_string(PrimaryType, Data);
to_binary_string_inner(
    _TypeInfo,
    #sp_range{
        type = integer,
        lower_bound = Min,
        upper_bound = Max
    } =
        Range,
    Data,
    _Opts
) ->
    case convert_type_to_binary_string(integer, Data) of
        {ok, BinaryString} when Min =< Data, Data =< Max ->
            {ok, BinaryString};
        {ok, _BinaryString} when is_integer(Data) ->
            {error, [sp_error:type_mismatch(Range, Data)]};
        {error, Reason} ->
            {error, Reason}
    end;
to_binary_string_inner(_TypeInfo, #sp_literal{} = Type, Data, _Opts) ->
    try_convert_literal_to_binary_string(Type, Data);
to_binary_string_inner(TypeInfo, #sp_union{} = Type, Data, Opts) ->
    union_to_binary_string(TypeInfo, Type, Data, Opts);
to_binary_string_inner(_TypeInfo, #sp_rec{} = T, _Data, _Opts) ->
    erlang:error({type_not_supported, T});
to_binary_string_inner(_TypeInfo, Type, Data, _Opts) ->
    {error, [sp_error:type_mismatch(Type, Data)]}.

%% INTERNAL

-spec convert_binary_string_to_type(Type :: spectra:simple_types(), BinaryString :: binary()) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
convert_binary_string_to_type(Type, BinaryString) when is_binary(BinaryString) ->
    do_convert_binary_string_to_type(Type, BinaryString);
convert_binary_string_to_type(Type, NonBinary) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = Type}, NonBinary)]}.

-spec do_convert_binary_string_to_type(Type :: spectra:simple_types(), BinaryString :: binary()) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
do_convert_binary_string_to_type(integer, BinaryString) ->
    try
        {ok, binary_to_integer(BinaryString)}
    catch
        error:badarg ->
            {error, [sp_error:type_mismatch(#sp_simple_type{type = integer}, BinaryString)]}
    end;
do_convert_binary_string_to_type(float, BinaryString) ->
    try
        {ok, binary_to_float(BinaryString)}
    catch
        error:badarg ->
            {error, [sp_error:type_mismatch(#sp_simple_type{type = float}, BinaryString)]}
    end;
do_convert_binary_string_to_type(number, BinaryString) ->
    case do_convert_binary_string_to_type(integer, BinaryString) of
        {ok, _} = Result ->
            Result;
        {error, _} ->
            do_convert_binary_string_to_type(float, BinaryString)
    end;
do_convert_binary_string_to_type(boolean, <<"true">>) ->
    {ok, true};
do_convert_binary_string_to_type(boolean, <<"false">>) ->
    {ok, false};
do_convert_binary_string_to_type(boolean, BinaryString) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = boolean}, BinaryString)]};
do_convert_binary_string_to_type(atom, BinaryString) ->
    try
        {ok, binary_to_existing_atom(BinaryString, utf8)}
    catch
        error:badarg ->
            {error, [sp_error:type_mismatch(#sp_simple_type{type = atom}, BinaryString)]}
    end;
do_convert_binary_string_to_type(string, BinaryString) ->
    {ok, binary_to_list(BinaryString)};
do_convert_binary_string_to_type(nonempty_string, BinaryString) when
    BinaryString =/= <<>>
->
    {ok, binary_to_list(BinaryString)};
do_convert_binary_string_to_type(nonempty_string, <<>>) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = nonempty_string}, <<>>)]};
do_convert_binary_string_to_type(binary, BinaryString) ->
    {ok, BinaryString};
do_convert_binary_string_to_type(nonempty_binary, BinaryString) when
    BinaryString =/= <<>>
->
    {ok, BinaryString};
do_convert_binary_string_to_type(nonempty_binary, <<>>) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = nonempty_binary}, <<>>)]};
do_convert_binary_string_to_type(non_neg_integer, BinaryString) ->
    case do_convert_binary_string_to_type(integer, BinaryString) of
        {ok, Value} when Value >= 0 ->
            {ok, Value};
        {ok, Value} ->
            {error, [sp_error:type_mismatch(#sp_simple_type{type = non_neg_integer}, Value)]};
        {error, Reason} ->
            {error, Reason}
    end;
do_convert_binary_string_to_type(pos_integer, BinaryString) ->
    case do_convert_binary_string_to_type(integer, BinaryString) of
        {ok, Value} when Value > 0 ->
            {ok, Value};
        {ok, Value} ->
            {error, [sp_error:type_mismatch(#sp_simple_type{type = pos_integer}, Value)]};
        {error, Reason} ->
            {error, Reason}
    end;
do_convert_binary_string_to_type(neg_integer, BinaryString) ->
    case do_convert_binary_string_to_type(integer, BinaryString) of
        {ok, Value} when Value < 0 ->
            {ok, Value};
        {ok, Value} ->
            {error, [sp_error:type_mismatch(#sp_simple_type{type = neg_integer}, Value)]};
        {error, Reason} ->
            {error, Reason}
    end;
do_convert_binary_string_to_type(Type, BinaryString) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = Type}, BinaryString)]}.

-spec try_convert_binary_string_to_literal(
    Literal :: spectra:literal_value(), BinaryString :: binary()
) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
try_convert_binary_string_to_literal(Literal, BinaryString) when is_boolean(Literal) ->
    case convert_binary_string_to_type(boolean, BinaryString) of
        {ok, Literal} ->
            {ok, Literal};
        {ok, _Other} ->
            {error, [
                sp_error:type_mismatch(
                    #sp_literal{value = Literal, binary_value = atom_to_binary(Literal, utf8)},
                    BinaryString
                )
            ]};
        {error, Reason} ->
            {error, Reason}
    end;
try_convert_binary_string_to_literal(Literal, BinaryString) when is_atom(Literal) ->
    case convert_binary_string_to_type(atom, BinaryString) of
        {ok, Literal} ->
            {ok, Literal};
        {ok, _Other} ->
            {error, [
                sp_error:type_mismatch(
                    #sp_literal{value = Literal, binary_value = atom_to_binary(Literal, utf8)},
                    BinaryString
                )
            ]};
        {error, Reason} ->
            {error, Reason}
    end;
try_convert_binary_string_to_literal(Literal, BinaryString) when is_integer(Literal) ->
    case convert_binary_string_to_type(integer, BinaryString) of
        {ok, Literal} ->
            {ok, Literal};
        {ok, _Other} ->
            {error, [
                sp_error:type_mismatch(
                    #sp_literal{value = Literal, binary_value = integer_to_binary(Literal)},
                    BinaryString
                )
            ]};
        {error, Reason} ->
            {error, Reason}
    end;
try_convert_binary_string_to_literal(Literal, BinaryString) ->
    {error, [
        sp_error:type_mismatch(
            #sp_literal{value = Literal, binary_value = <<>>},
            BinaryString
        )
    ]}.

union(Fun, TypeInfo, #sp_union{types = Types} = T, BinaryString, Opts) ->
    case do_first(Fun, TypeInfo, Types, BinaryString, Opts, []) of
        {error, UnionErrors} ->
            {error, [sp_error:no_match(T, BinaryString, UnionErrors)]};
        Result ->
            Result
    end.

do_first(_Fun, _TypeInfo, [], _BinaryString, _Opts, Errors) ->
    {error, Errors};
do_first(Fun, TypeInfo, [Type | Rest], BinaryString, Opts, ErrorsAcc) ->
    case Fun(TypeInfo, Type, BinaryString, Opts) of
        {ok, Result} ->
            {ok, Result};
        {error, Errors} ->
            do_first(Fun, TypeInfo, Rest, BinaryString, Opts, [{Type, Errors} | ErrorsAcc])
    end.

-spec resolve_remote_type(module(), atom(), [spectra:sp_type()]) ->
    {spectra:type_info(), spectra:sp_type()}.
resolve_remote_type(Module, TypeName, Args) ->
    TypeInfo = spectra_module_types:get(Module),
    TypeArity = length(Args),
    Type = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    {TypeInfo, apply_args(TypeInfo, Type, Args)}.

apply_args(TypeInfo, Type, TypeArgs) when is_list(TypeArgs) ->
    ArgNames = arg_names(Type),
    NamedTypes =
        maps:from_list(
            lists:zip(ArgNames, TypeArgs)
        ),
    spectra_util:type_replace_vars(TypeInfo, Type, NamedTypes).

arg_names(#sp_type_with_variables{vars = Args}) ->
    Args;
arg_names(_) ->
    [].

convert_type_to_binary_string(integer, Data) when is_integer(Data) ->
    {ok, integer_to_binary(Data)};
convert_type_to_binary_string(integer, Data) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = integer}, Data)]};
convert_type_to_binary_string(float, Data) when is_float(Data) ->
    {ok, float_to_binary(Data)};
convert_type_to_binary_string(float, Data) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = float}, Data)]};
convert_type_to_binary_string(number, Data) when is_number(Data) ->
    if
        is_integer(Data) ->
            {ok, integer_to_binary(Data)};
        is_float(Data) ->
            {ok, float_to_binary(Data)}
    end;
convert_type_to_binary_string(number, Data) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = number}, Data)]};
convert_type_to_binary_string(boolean, true) ->
    {ok, <<"true">>};
convert_type_to_binary_string(boolean, false) ->
    {ok, <<"false">>};
convert_type_to_binary_string(boolean, Data) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = boolean}, Data)]};
convert_type_to_binary_string(atom, Data) when is_atom(Data) ->
    {ok, atom_to_binary(Data, utf8)};
convert_type_to_binary_string(atom, Data) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = atom}, Data)]};
convert_type_to_binary_string(string, Data) when is_list(Data) ->
    case unicode:characters_to_binary(Data) of
        DataBinary when is_binary(DataBinary) ->
            {ok, DataBinary};
        _Other ->
            {error, [sp_error:type_mismatch(#sp_simple_type{type = string}, Data)]}
    end;
convert_type_to_binary_string(string, Data) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = string}, Data)]};
convert_type_to_binary_string(nonempty_string, Data) when is_list(Data), Data =/= [] ->
    case unicode:characters_to_binary(Data) of
        DataBinary when is_binary(DataBinary) ->
            {ok, DataBinary};
        _Other ->
            {error, [sp_error:type_mismatch(#sp_simple_type{type = nonempty_string}, Data)]}
    end;
convert_type_to_binary_string(nonempty_string, Data) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = nonempty_string}, Data)]};
convert_type_to_binary_string(binary, Data) when is_binary(Data) ->
    {ok, Data};
convert_type_to_binary_string(binary, Data) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = binary}, Data)]};
convert_type_to_binary_string(nonempty_binary, Data) when
    is_binary(Data), Data =/= <<>>
->
    {ok, Data};
convert_type_to_binary_string(nonempty_binary, Data) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = nonempty_binary}, Data)]};
convert_type_to_binary_string(non_neg_integer, Data) when is_integer(Data), Data >= 0 ->
    {ok, integer_to_binary(Data)};
convert_type_to_binary_string(non_neg_integer, Data) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = non_neg_integer}, Data)]};
convert_type_to_binary_string(pos_integer, Data) when is_integer(Data), Data > 0 ->
    {ok, integer_to_binary(Data)};
convert_type_to_binary_string(pos_integer, Data) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = pos_integer}, Data)]};
convert_type_to_binary_string(neg_integer, Data) when is_integer(Data), Data < 0 ->
    {ok, integer_to_binary(Data)};
convert_type_to_binary_string(neg_integer, Data) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = neg_integer}, Data)]};
convert_type_to_binary_string(Type, Data) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = Type}, Data)]}.

-spec try_convert_literal_to_binary_string(Literal :: #sp_literal{}, Data :: dynamic()) ->
    {ok, binary()} | {error, [spectra:error()]}.
try_convert_literal_to_binary_string(#sp_literal{value = Literal}, Literal) when is_atom(Literal) ->
    {ok, atom_to_binary(Literal, utf8)};
try_convert_literal_to_binary_string(#sp_literal{value = Literal}, Literal) when
    is_integer(Literal)
->
    {ok, integer_to_binary(Literal)};
try_convert_literal_to_binary_string(#sp_literal{value = Literal}, Literal) when
    is_boolean(Literal)
->
    if
        Literal ->
            {ok, <<"true">>};
        true ->
            {ok, <<"false">>}
    end;
try_convert_literal_to_binary_string(#sp_literal{} = Type, Data) ->
    {error, [sp_error:type_mismatch(Type, Data)]}.

union_to_binary_string(TypeInfo, #sp_union{types = Types} = T, Data, Opts) ->
    case do_first_to_binary_string(TypeInfo, Types, Data, Opts, []) of
        {error, UnionErrors} ->
            {error, [sp_error:no_match(T, Data, UnionErrors)]};
        Result ->
            Result
    end.

do_first_to_binary_string(_TypeInfo, [], _Data, _Opts, Errors) ->
    {error, Errors};
do_first_to_binary_string(TypeInfo, [Type | Rest], Data, Opts, ErrorsAcc) ->
    case to_binary_string(TypeInfo, Type, Data, Opts) of
        {ok, Result} ->
            {ok, Result};
        {error, Errors} ->
            do_first_to_binary_string(TypeInfo, Rest, Data, Opts, [{Type, Errors} | ErrorsAcc])
    end.

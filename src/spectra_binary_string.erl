-module(spectra_binary_string).

-export([
    from_binary_string/3,
    from_binary_string/4,
    from_binary_string/5,
    to_binary_string/3,
    to_binary_string/4,
    to_binary_string/5
]).

-ignore_xref([
    {spectra_binary_string, from_binary_string, 3},
    {spectra_binary_string, from_binary_string, 4},
    {spectra_binary_string, from_binary_string, 5},
    {spectra_binary_string, to_binary_string, 3},
    {spectra_binary_string, to_binary_string, 4},
    {spectra_binary_string, to_binary_string, 5}
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

Equivalent to calling from_binary_string/5 with a default configuration.

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
from_binary_string(TypeInfo, Type, BinaryString, Opts) ->
    UseCache = application:get_env(spectra, use_module_types_cache, false),
    Codecs = application:get_env(spectra, codecs, #{}),
    Config = #sp_config{use_module_types_cache = UseCache, codecs = Codecs},
    from_binary_string(TypeInfo, Type, BinaryString, Opts, Config).

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
            "Config" => "Runtime configuration",
            "Opts" => "Decode options",
            "Type" => "The type specification (spectra:sp_type())",
            "TypeInfo" => "The type information containing type definitions"
        }
}.

-spec from_binary_string(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type(),
    BinaryString :: binary(),
    Opts :: spectra:binary_string_decode_opts(),
    Config :: spectra:sp_config()
) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
from_binary_string(
    TypeInfo,
    #sp_user_type_ref{type_name = N, variables = Args, arity = Arity} = UserTypeRef,
    BinaryString,
    Opts,
    Config
) ->
    Mod = spectra_type_info:get_module(TypeInfo),
    Type = spectra_type_info:get_type(TypeInfo, N, Arity),
    case
        spectra_codec:try_codec_decode(
            Mod, binary_string, Type, BinaryString, UserTypeRef, Config#sp_config.codecs
        )
    of
        continue ->
            TypeWithoutVars = apply_args(TypeInfo, Type, Args),
            from_binary_string(TypeInfo, TypeWithoutVars, BinaryString, Opts, Config);
        Result ->
            Result
    end;
from_binary_string(
    _TypeInfo,
    #sp_remote_type{mfargs = {Module, TypeName, Args}, arity = TypeArity} = RemoteRef,
    BinaryString,
    Opts,
    Config
) ->
    RemoteTypeInfo = spectra_module_types:get(Module, Config#sp_config.use_module_types_cache),
    RemoteType = spectra_type_info:get_type(RemoteTypeInfo, TypeName, TypeArity),
    case
        spectra_codec:try_codec_decode(
            Module, binary_string, RemoteType, BinaryString, RemoteRef, Config#sp_config.codecs
        )
    of
        continue ->
            TypeResolved = spectra_type:propagate_params(
                RemoteRef, apply_args(RemoteTypeInfo, RemoteType, Args)
            ),
            from_binary_string(RemoteTypeInfo, TypeResolved, BinaryString, Opts, Config);
        Result ->
            Result
    end;
from_binary_string(
    TypeInfo, #sp_rec_ref{record_name = RecordName} = RecordRef, BinaryString, _Opts, Config
) ->
    Mod = spectra_type_info:get_module(TypeInfo),
    RecordType = spectra_type_info:get_record(TypeInfo, RecordName),
    case
        spectra_codec:try_codec_decode(
            Mod, binary_string, RecordType, BinaryString, RecordRef, Config#sp_config.codecs
        )
    of
        continue -> erlang:error({type_not_supported, RecordType});
        Result -> Result
    end;
from_binary_string(
    _TypeInfo, #sp_simple_type{type = NotSupported} = T, _BinaryString, _Opts, _Config
) when
    NotSupported =:= pid orelse
        NotSupported =:= port orelse
        NotSupported =:= reference orelse
        NotSupported =:= bitstring orelse
        NotSupported =:= nonempty_bitstring orelse
        NotSupported =:= none
->
    erlang:error({type_not_supported, T});
from_binary_string(_TypeInfo, #sp_simple_type{type = PrimaryType}, BinaryString, _Opts, _Config) ->
    convert_binary_string_to_type(PrimaryType, BinaryString);
from_binary_string(
    _TypeInfo,
    #sp_range{
        type = integer,
        lower_bound = Min,
        upper_bound = Max
    } =
        Range,
    BinaryString,
    _Opts,
    _Config
) ->
    case convert_binary_string_to_type(integer, BinaryString) of
        {ok, Value} when Min =< Value, Value =< Max ->
            {ok, Value};
        {ok, Value} when is_integer(Value) ->
            {error, [sp_error:type_mismatch(Range, Value)]};
        {error, Reason} ->
            {error, Reason}
    end;
from_binary_string(_TypeInfo, #sp_literal{value = Literal}, BinaryString, _Opts, _Config) ->
    try_convert_binary_string_to_literal(Literal, BinaryString);
from_binary_string(TypeInfo, #sp_union{} = Type, BinaryString, Opts, Config) ->
    union(fun from_binary_string/5, TypeInfo, Type, BinaryString, Opts, Config);
from_binary_string(_TypeInfo, #sp_rec{} = T, _BinaryString, _Opts, _Config) ->
    erlang:error({type_not_supported, T});
from_binary_string(_TypeInfo, Type, BinaryString, _Opts, _Config) ->
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

Equivalent to calling to_binary_string/5 with a default configuration.

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
to_binary_string(TypeInfo, Type, Data, Opts) ->
    UseCache = application:get_env(spectra, use_module_types_cache, false),
    Codecs = application:get_env(spectra, codecs, #{}),
    Config = #sp_config{use_module_types_cache = UseCache, codecs = Codecs},
    to_binary_string(TypeInfo, Type, Data, Opts, Config).

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
            "Config" => "Runtime configuration",
            "Opts" => "Encode options",
            "Type" => "The type specification (spectra:sp_type())",
            "TypeInfo" => "The type information containing type definitions"
        }
}.

-spec to_binary_string(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type(),
    Data :: dynamic(),
    Opts :: spectra:binary_string_encode_opts(),
    Config :: spectra:sp_config()
) ->
    {ok, binary()} | {error, [spectra:error()]}.
to_binary_string(
    TypeInfo,
    #sp_user_type_ref{type_name = TypeName, variables = Args, arity = Arity} = UserTypeRef,
    Data,
    Opts,
    Config
) ->
    Mod = spectra_type_info:get_module(TypeInfo),
    Type = spectra_type_info:get_type(TypeInfo, TypeName, Arity),
    case
        spectra_codec:try_codec_encode(
            Mod, binary_string, Type, Data, UserTypeRef, Config#sp_config.codecs
        )
    of
        continue ->
            TypeWithoutVars = apply_args(TypeInfo, Type, Args),
            to_binary_string(TypeInfo, TypeWithoutVars, Data, Opts, Config);
        Result ->
            Result
    end;
to_binary_string(
    _TypeInfo,
    #sp_remote_type{mfargs = {Module, TypeName, Args}, arity = TypeArity} = RemoteRef,
    Data,
    Opts,
    Config
) ->
    RemoteTypeInfo = spectra_module_types:get(Module, Config#sp_config.use_module_types_cache),
    RemoteType = spectra_type_info:get_type(RemoteTypeInfo, TypeName, TypeArity),
    case
        spectra_codec:try_codec_encode(
            Module, binary_string, RemoteType, Data, RemoteRef, Config#sp_config.codecs
        )
    of
        continue ->
            TypeWithoutVars = apply_args(RemoteTypeInfo, RemoteType, Args),
            to_binary_string(RemoteTypeInfo, TypeWithoutVars, Data, Opts, Config);
        Result ->
            Result
    end;
to_binary_string(TypeInfo, #sp_rec_ref{record_name = RecordName} = RecordRef, Data, _Opts, Config) ->
    Mod = spectra_type_info:get_module(TypeInfo),
    RecordType = spectra_type_info:get_record(TypeInfo, RecordName),
    case
        spectra_codec:try_codec_encode(
            Mod, binary_string, RecordType, Data, RecordRef, Config#sp_config.codecs
        )
    of
        continue -> erlang:error({type_not_supported, RecordType});
        Result -> Result
    end;
to_binary_string(_TypeInfo, #sp_simple_type{type = NotSupported} = T, _Data, _Opts, _Config) when
    NotSupported =:= pid orelse
        NotSupported =:= port orelse
        NotSupported =:= reference orelse
        NotSupported =:= bitstring orelse
        NotSupported =:= nonempty_bitstring orelse
        NotSupported =:= none
->
    erlang:error({type_not_supported, T});
to_binary_string(_TypeInfo, #sp_simple_type{type = PrimaryType}, Data, _Opts, _Config) ->
    convert_type_to_binary_string(PrimaryType, Data);
to_binary_string(
    _TypeInfo,
    #sp_range{
        type = integer,
        lower_bound = Min,
        upper_bound = Max
    } =
        Range,
    Data,
    _Opts,
    _Config
) ->
    case convert_type_to_binary_string(integer, Data) of
        {ok, BinaryString} when Min =< Data, Data =< Max ->
            {ok, BinaryString};
        {ok, _BinaryString} when is_integer(Data) ->
            {error, [sp_error:type_mismatch(Range, Data)]};
        {error, Reason} ->
            {error, Reason}
    end;
to_binary_string(_TypeInfo, #sp_literal{} = Type, Data, _Opts, _Config) ->
    try_convert_literal_to_binary_string(Type, Data);
to_binary_string(TypeInfo, #sp_union{} = Type, Data, Opts, Config) ->
    union_to_binary_string(TypeInfo, Type, Data, Opts, Config);
to_binary_string(_TypeInfo, #sp_rec{} = T, _Data, _Opts, _Config) ->
    erlang:error({type_not_supported, T});
to_binary_string(_TypeInfo, Type, Data, _Opts, _Config) ->
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

union(Fun, TypeInfo, #sp_union{types = Types} = T, BinaryString, Opts, Config) ->
    case do_first(Fun, TypeInfo, Types, BinaryString, Opts, Config, []) of
        {error, UnionErrors} ->
            {error, [sp_error:no_match(T, BinaryString, UnionErrors)]};
        Result ->
            Result
    end.

do_first(_Fun, _TypeInfo, [], _BinaryString, _Opts, _Config, Errors) ->
    {error, Errors};
do_first(Fun, TypeInfo, [Type | Rest], BinaryString, Opts, Config, ErrorsAcc) ->
    case Fun(TypeInfo, Type, BinaryString, Opts, Config) of
        {ok, Result} ->
            {ok, Result};
        {error, Errors} ->
            do_first(Fun, TypeInfo, Rest, BinaryString, Opts, Config, [{Type, Errors} | ErrorsAcc])
    end.

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

union_to_binary_string(TypeInfo, #sp_union{types = Types} = T, Data, Opts, Config) ->
    case do_first_to_binary_string(TypeInfo, Types, Data, Opts, Config, []) of
        {error, UnionErrors} ->
            {error, [sp_error:no_match(T, Data, UnionErrors)]};
        Result ->
            Result
    end.

do_first_to_binary_string(_TypeInfo, [], _Data, _Opts, _Config, Errors) ->
    {error, Errors};
do_first_to_binary_string(TypeInfo, [Type | Rest], Data, Opts, Config, ErrorsAcc) ->
    case to_binary_string(TypeInfo, Type, Data, Opts, Config) of
        {ok, Result} ->
            {ok, Result};
        {error, Errors} ->
            do_first_to_binary_string(TypeInfo, Rest, Data, Opts, Config, [
                {Type, Errors} | ErrorsAcc
            ])
    end.

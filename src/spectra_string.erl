-module(spectra_string).

-export([from_string/4, to_string/4]).

-ignore_xref([
    {spectra_string, from_string, 4},
    {spectra_string, to_string, 4}
]).

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
            "Config" => "Runtime configuration",
            "String" => "The string value to convert to Erlang format",
            "Type" => "The type specification (spectra:sp_type())",
            "TypeInfo" => "The type information containing type definitions"
        }
}.

-spec from_string(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type(),
    String :: list(),
    Config :: spectra:sp_config()
) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
from_string(
    TypeInfo,
    #sp_user_type_ref{type_name = TypeName, variables = Args, arity = Arity} = UserTypeRef,
    String,
    Config
) ->
    Mod = spectra_type_info:get_module(TypeInfo),
    Type = spectra_type_info:get_type(TypeInfo, TypeName, Arity),
    case
        spectra_codec:try_codec_decode(
            Mod,
            string,
            Type,
            String,
            UserTypeRef,
            Config#sp_config.codecs,
            Config#sp_config.module_types_cache
        )
    of
        continue ->
            TypeWithoutVars = apply_args(TypeInfo, Type, Args),
            from_string(TypeInfo, TypeWithoutVars, String, Config);
        Result ->
            Result
    end;
from_string(TypeInfo, #sp_rec_ref{record_name = RecordName} = RecordRef, String, Config) ->
    Mod = spectra_type_info:get_module(TypeInfo),
    RecordType = spectra_type_info:get_record(TypeInfo, RecordName),
    case
        spectra_codec:try_codec_decode(
            Mod,
            string,
            RecordType,
            String,
            RecordRef,
            Config#sp_config.codecs,
            Config#sp_config.module_types_cache
        )
    of
        continue -> erlang:error({type_not_supported, RecordType});
        Result -> Result
    end;
from_string(
    _TypeInfo,
    #sp_remote_type{mfargs = {Module, TypeName, Args}, arity = TypeArity} = RemoteRef,
    String,
    Config
) ->
    RemoteTypeInfo = spectra_module_types:get(Module, Config#sp_config.module_types_cache),
    RemoteType = spectra_type_info:get_type(RemoteTypeInfo, TypeName, TypeArity),
    case
        spectra_codec:try_codec_decode(
            Module,
            string,
            RemoteType,
            String,
            RemoteRef,
            Config#sp_config.codecs,
            Config#sp_config.module_types_cache
        )
    of
        continue ->
            TypeWithoutVars = apply_args(RemoteTypeInfo, RemoteType, Args),
            from_string(RemoteTypeInfo, TypeWithoutVars, String, Config);
        Result ->
            Result
    end;
from_string(_TypeInfo, #sp_simple_type{type = NotSupported} = T, _String, _Config) when
    NotSupported =:= pid orelse
        NotSupported =:= port orelse
        NotSupported =:= reference orelse
        NotSupported =:= bitstring orelse
        NotSupported =:= nonempty_bitstring orelse
        NotSupported =:= none
->
    erlang:error({type_not_supported, T});
from_string(_TypeInfo, #sp_simple_type{type = PrimaryType}, String, _Config) ->
    convert_string_to_type(PrimaryType, String);
from_string(
    _TypeInfo,
    #sp_range{
        type = integer,
        lower_bound = Min,
        upper_bound = Max
    } =
        Range,
    String,
    _Config
) ->
    case convert_string_to_type(integer, String) of
        {ok, Value} when Min =< Value, Value =< Max ->
            {ok, Value};
        {ok, Value} when is_integer(Value) ->
            {error, [sp_error:type_mismatch(Range, Value)]};
        {error, Reason} ->
            {error, Reason}
    end;
from_string(_TypeInfo, #sp_literal{value = Literal}, String, _Config) ->
    try_convert_string_to_literal(Literal, String);
from_string(TypeInfo, #sp_union{} = Type, String, Config) ->
    union(fun from_string/4, TypeInfo, Type, String, Config);
from_string(_TypeInfo, #sp_rec{} = T, _String, _Config) ->
    erlang:error({type_not_supported, T});
from_string(_TypeInfo, Type, String, _Config) ->
    {error, [sp_error:type_mismatch(Type, String)]}.

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
            "Config" => "Runtime configuration",
            "Data" => "The Erlang value to convert to string format",
            "Type" => "The type specification (spectra:sp_type())",
            "TypeInfo" => "The type information containing type definitions"
        }
}.

-spec to_string(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type(),
    Data :: dynamic(),
    Config :: spectra:sp_config()
) ->
    {ok, string()} | {error, [spectra:error()]}.
to_string(
    TypeInfo,
    #sp_user_type_ref{type_name = TypeName, variables = Args, arity = Arity} = UserTypeRef,
    Data,
    Config
) ->
    Mod = spectra_type_info:get_module(TypeInfo),
    Type = spectra_type_info:get_type(TypeInfo, TypeName, Arity),
    case
        spectra_codec:try_codec_encode(
            Mod,
            string,
            Type,
            Data,
            UserTypeRef,
            Config#sp_config.codecs,
            Config#sp_config.module_types_cache
        )
    of
        continue ->
            TypeWithoutVars = apply_args(TypeInfo, Type, Args),
            to_string(TypeInfo, TypeWithoutVars, Data, Config);
        Result ->
            Result
    end;
to_string(TypeInfo, #sp_rec_ref{record_name = RecordName} = RecordRef, Data, Config) ->
    Mod = spectra_type_info:get_module(TypeInfo),
    RecordType = spectra_type_info:get_record(TypeInfo, RecordName),
    case
        spectra_codec:try_codec_encode(
            Mod,
            string,
            RecordType,
            Data,
            RecordRef,
            Config#sp_config.codecs,
            Config#sp_config.module_types_cache
        )
    of
        continue -> erlang:error({type_not_supported, RecordType});
        Result -> Result
    end;
to_string(
    _TypeInfo,
    #sp_remote_type{mfargs = {Module, TypeName, Args}, arity = TypeArity} = RemoteRef,
    Data,
    Config
) ->
    RemoteTypeInfo = spectra_module_types:get(Module, Config#sp_config.module_types_cache),
    RemoteType = spectra_type_info:get_type(RemoteTypeInfo, TypeName, TypeArity),
    case
        spectra_codec:try_codec_encode(
            Module,
            string,
            RemoteType,
            Data,
            RemoteRef,
            Config#sp_config.codecs,
            Config#sp_config.module_types_cache
        )
    of
        continue ->
            TypeWithoutVars = apply_args(RemoteTypeInfo, RemoteType, Args),
            to_string(RemoteTypeInfo, TypeWithoutVars, Data, Config);
        Result ->
            Result
    end;
to_string(_TypeInfo, #sp_simple_type{type = NotSupported} = T, _Data, _Config) when
    NotSupported =:= pid orelse
        NotSupported =:= port orelse
        NotSupported =:= reference orelse
        NotSupported =:= bitstring orelse
        NotSupported =:= nonempty_bitstring orelse
        NotSupported =:= none
->
    erlang:error({type_not_supported, T});
to_string(_TypeInfo, #sp_simple_type{type = PrimaryType}, Data, Config) ->
    convert_type_to_string(PrimaryType, Data, Config);
to_string(
    _TypeInfo,
    #sp_range{
        type = integer,
        lower_bound = Min,
        upper_bound = Max
    } =
        Range,
    Data,
    _Config
) ->
    case convert_type_to_string(integer, Data, undefined) of
        {ok, String} when Min =< Data, Data =< Max ->
            {ok, String};
        {ok, _String} when is_integer(Data) ->
            {error, [sp_error:type_mismatch(Range, Data)]};
        {error, Reason} ->
            {error, Reason}
    end;
to_string(_TypeInfo, #sp_literal{value = Literal}, Data, _Config) ->
    try_convert_literal_to_string(Literal, Data);
to_string(TypeInfo, #sp_union{} = Type, Data, Config) ->
    union(fun to_string/4, TypeInfo, Type, Data, Config);
to_string(_TypeInfo, #sp_rec{} = T, _Data, _Config) ->
    erlang:error({type_not_supported, T});
to_string(_TypeInfo, Type, Data, _Config) ->
    {error, [sp_error:type_mismatch(Type, Data)]}.

%% INTERNAL
-spec convert_string_to_type(
    Type :: spectra:simple_types(),
    String :: string()
) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
convert_string_to_type(Type, String) when is_atom(Type), is_list(String) ->
    do_convert_string_to_type(Type, String);
convert_string_to_type(Type, NotString) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = Type}, NotString)]}.

-spec do_convert_string_to_type(Type :: spectra:simple_types(), String :: string()) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
do_convert_string_to_type(integer, String) ->
    try
        {ok, list_to_integer(String)}
    catch
        error:badarg ->
            {error, [sp_error:type_mismatch(#sp_simple_type{type = integer}, String)]}
    end;
do_convert_string_to_type(float, String) ->
    try
        {ok, list_to_float(String)}
    catch
        error:badarg ->
            {error, [sp_error:type_mismatch(#sp_simple_type{type = float}, String)]}
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
    {error, [sp_error:type_mismatch(#sp_simple_type{type = boolean}, String)]};
do_convert_string_to_type(atom, String) ->
    try
        {ok, list_to_existing_atom(String)}
    catch
        error:badarg ->
            {error, [sp_error:type_mismatch(#sp_simple_type{type = atom}, String)]}
    end;
do_convert_string_to_type(string, String) ->
    {ok, String};
do_convert_string_to_type(nonempty_string, String) when String =/= [] ->
    {ok, String};
do_convert_string_to_type(nonempty_string, []) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = nonempty_string}, [])]};
do_convert_string_to_type(binary, String) ->
    {ok, list_to_binary(String)};
do_convert_string_to_type(nonempty_binary, String) when String =/= [] ->
    {ok, list_to_binary(String)};
do_convert_string_to_type(nonempty_binary, []) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = nonempty_binary}, [])]};
do_convert_string_to_type(non_neg_integer, String) ->
    case do_convert_string_to_type(integer, String) of
        {ok, Value} when Value >= 0 ->
            {ok, Value};
        {ok, Value} ->
            {error, [sp_error:type_mismatch(#sp_simple_type{type = non_neg_integer}, Value)]};
        {error, Reason} ->
            {error, Reason}
    end;
do_convert_string_to_type(pos_integer, String) ->
    case do_convert_string_to_type(integer, String) of
        {ok, Value} when Value > 0 ->
            {ok, Value};
        {ok, Value} ->
            {error, [sp_error:type_mismatch(#sp_simple_type{type = pos_integer}, Value)]};
        {error, Reason} ->
            {error, Reason}
    end;
do_convert_string_to_type(neg_integer, String) ->
    case do_convert_string_to_type(integer, String) of
        {ok, Value} when Value < 0 ->
            {ok, Value};
        {ok, Value} ->
            {error, [sp_error:type_mismatch(#sp_simple_type{type = neg_integer}, Value)]};
        {error, Reason} ->
            {error, Reason}
    end;
do_convert_string_to_type(Type, String) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = Type}, String)]}.

-spec try_convert_string_to_literal(Literal :: spectra:literal_value(), String :: string()) ->
    {ok, dynamic()} | {error, [spectra:error()]}.
try_convert_string_to_literal(Literal, String) when is_boolean(Literal) ->
    case convert_string_to_type(boolean, String) of
        {ok, Literal} ->
            {ok, Literal};
        {ok, _Other} ->
            {error, [
                sp_error:type_mismatch(
                    #sp_literal{value = Literal, binary_value = atom_to_binary(Literal, utf8)},
                    String
                )
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
                sp_error:type_mismatch(
                    #sp_literal{value = Literal, binary_value = atom_to_binary(Literal, utf8)},
                    String
                )
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
                sp_error:type_mismatch(
                    #sp_literal{value = Literal, binary_value = integer_to_binary(Literal)},
                    String
                )
            ]};
        {error, Reason} ->
            {error, Reason}
    end;
try_convert_string_to_literal(Literal, String) ->
    {error, [
        sp_error:type_mismatch(
            #sp_literal{value = Literal, binary_value = <<>>},
            String
        )
    ]}.

union(Fun, TypeInfo, #sp_union{types = Types} = T, String, Config) ->
    case do_first(Fun, TypeInfo, Types, String, Config, []) of
        {error, UnionErrors} ->
            {error, [sp_error:no_match(T, String, UnionErrors)]};
        Result ->
            Result
    end.

do_first(_Fun, _TypeInfo, [], _String, _Config, Errors) ->
    {error, Errors};
do_first(Fun, TypeInfo, [Type | Rest], String, Config, ErrorsAcc) ->
    case Fun(TypeInfo, Type, String, Config) of
        {ok, Result} ->
            {ok, Result};
        {error, Errors} ->
            do_first(Fun, TypeInfo, Rest, String, Config, [{Type, Errors} | ErrorsAcc])
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

-spec convert_type_to_string(
    Type :: spectra:simple_types(), Data :: dynamic(), Config :: spectra:sp_config() | undefined
) ->
    {ok, string()} | {error, [spectra:error()]}.
convert_type_to_string(integer, Data, _Config) when is_integer(Data) ->
    {ok, integer_to_list(Data)};
convert_type_to_string(integer, Data, _Config) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = integer}, Data)]};
convert_type_to_string(float, Data, _Config) when is_float(Data) ->
    {ok, float_to_list(Data)};
convert_type_to_string(float, Data, _Config) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = float}, Data)]};
convert_type_to_string(number, Data, _Config) when is_number(Data) ->
    if
        is_integer(Data) ->
            {ok, integer_to_list(Data)};
        is_float(Data) ->
            {ok, float_to_list(Data)}
    end;
convert_type_to_string(number, Data, _Config) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = number}, Data)]};
convert_type_to_string(boolean, true, _Config) ->
    {ok, "true"};
convert_type_to_string(boolean, false, _Config) ->
    {ok, "false"};
convert_type_to_string(boolean, Data, _Config) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = boolean}, Data)]};
convert_type_to_string(atom, Data, _Config) when is_atom(Data) ->
    {ok, atom_to_list(Data)};
convert_type_to_string(atom, Data, _Config) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = atom}, Data)]};
convert_type_to_string(string, Data, Config) when is_list(Data) ->
    list_to_charlist(Data, Config);
convert_type_to_string(string, Data, _Config) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = string}, Data)]};
convert_type_to_string(nonempty_string, Data, Config) when is_list(Data), Data =/= [] ->
    list_to_charlist(Data, Config);
convert_type_to_string(nonempty_string, Data, _Config) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = nonempty_string}, Data)]};
convert_type_to_string(binary, Data, _Config) when is_binary(Data) ->
    {ok, binary_to_list(Data)};
convert_type_to_string(binary, Data, _Config) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = binary}, Data)]};
convert_type_to_string(nonempty_binary, Data, _Config) when is_binary(Data), Data =/= <<>> ->
    {ok, binary_to_list(Data)};
convert_type_to_string(nonempty_binary, Data, _Config) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = nonempty_binary}, Data)]};
convert_type_to_string(non_neg_integer, Data, _Config) when is_integer(Data), Data >= 0 ->
    {ok, integer_to_list(Data)};
convert_type_to_string(non_neg_integer, Data, _Config) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = non_neg_integer}, Data)]};
convert_type_to_string(pos_integer, Data, _Config) when is_integer(Data), Data > 0 ->
    {ok, integer_to_list(Data)};
convert_type_to_string(pos_integer, Data, _Config) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = pos_integer}, Data)]};
convert_type_to_string(neg_integer, Data, _Config) when is_integer(Data), Data < 0 ->
    {ok, integer_to_list(Data)};
convert_type_to_string(neg_integer, Data, _Config) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = neg_integer}, Data)]};
convert_type_to_string(Type, Data, _Config) ->
    {error, [sp_error:type_mismatch(#sp_simple_type{type = Type}, Data)]}.

list_to_charlist(Data, #sp_config{check_unicode = true}) ->
    case unicode:characters_to_list(Data) of
        DataList when is_list(DataList) ->
            {ok, DataList};
        _Other ->
            {error, [sp_error:type_mismatch(#sp_simple_type{type = string}, Data)]}
    end;
list_to_charlist(Data, _Config) ->
    {ok, Data}.

-spec try_convert_literal_to_string(Literal :: spectra:literal_value(), Data :: dynamic()) ->
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
    {error, [
        sp_error:type_mismatch(
            #sp_literal{value = Literal, binary_value = <<>>},
            Data
        )
    ]}.

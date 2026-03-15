-module(spectra_type_info).

-include("../include/spectra_internal.hrl").

-ignore_xref([find_function/3, new/2]).

-export([new/2, get_module/1]).
-export([add_type/4, find_type/3, get_type/3]).
-export([add_record/3, find_record/2, get_record/2]).
-export([add_function/4, find_function/3]).
-export([find_codec/2]).

-export_type([type_info/0, type_key/0, function_key/0]).

-type type_info() :: #type_info{}.
-type type_key() :: {Name :: atom(), Arity :: arity()}.
-type function_key() :: {Name :: atom(), Arity :: arity()}.

-spec new(module(), ImplementsCodec :: boolean()) -> type_info().
new(Module, ImplementsCodec) ->
    #type_info{module = Module, implements_codec = ImplementsCodec}.

-spec get_module(type_info()) -> module().
get_module(#type_info{module = M}) -> M.

-spec add_type(type_info(), atom(), arity(), spectra:sp_type()) -> type_info().
add_type(#type_info{types = Types} = TypeInfo, Name, Arity, Type) ->
    TypeInfo#type_info{types = Types#{{Name, Arity} => Type}}.

-spec find_type(type_info(), atom(), arity()) -> {ok, spectra:sp_type()} | error.
find_type(#type_info{types = Types}, Name, Arity) ->
    maps:find({Name, Arity}, Types).

-spec get_type(type_info(), atom(), arity()) -> spectra:sp_type().
get_type(#type_info{types = Types}, Name, Arity) ->
    case Types of
        #{{Name, Arity} := Type} ->
            Type;
        #{} ->
            erlang:error({type_not_found, Name, Arity})
    end.

-spec add_record(type_info(), atom(), #sp_rec{}) -> type_info().
add_record(#type_info{records = Records} = TypeInfo, Name, Record) ->
    TypeInfo#type_info{records = Records#{Name => Record}}.

-spec find_record(type_info(), atom()) -> {ok, #sp_rec{}} | error.
find_record(#type_info{records = Records}, Name) ->
    maps:find(Name, Records).

-spec get_record(type_info(), atom()) -> #sp_rec{}.
get_record(#type_info{records = Records}, Name) ->
    case Records of
        #{Name := Record} ->
            Record;
        #{} ->
            erlang:error({record_not_found, Name})
    end.

-spec add_function(type_info(), atom(), arity(), [spectra:sp_function_spec()]) ->
    type_info().
add_function(#type_info{functions = Functions} = TypeInfo, Name, Arity, FuncSpec) ->
    TypeInfo#type_info{functions = Functions#{{Name, Arity} => FuncSpec}}.

-spec find_function(type_info(), atom(), arity()) ->
    {ok, [spectra:sp_function_spec()]} | error.
find_function(#type_info{functions = Functions}, Name, Arity) ->
    maps:find({Name, Arity}, Functions).

-spec find_local_codec(type_info()) -> {ok, module()} | error.
find_local_codec(#type_info{implements_codec = true, module = M}) -> {ok, M};
find_local_codec(#type_info{implements_codec = false}) -> error.

-spec find_codec(module(), spectra:sp_type_reference()) -> {ok, module()} | error.
find_codec(Mod, TypeRef) ->
    GlobalCodecs = application:get_env(spectra, codecs, #{}),
    case maps:find({Mod, TypeRef}, GlobalCodecs) of
        {ok, _} = R ->
            R;
        error ->
            find_local_codec(spectra_module_types:get(Mod))
    end.

-module(spectra_type_info).

-moduledoc """
Stores and queries the type, record, and function metadata extracted from a
module's BEAM debug info.

A `type_info()` value is the primary context object passed through
serialisation and schema-generation traversals. It maps
`{TypeName, Arity}` keys to their `sp_type()` representations and also
tracks whether the owning module implements the `spectra_codec` behaviour.
""".

-include("../include/spectra_internal.hrl").

-ignore_xref([find_function/3, new/2]).

-export([new/2, get_module/1]).
-export([add_type/4, find_type/3, get_type/3]).
-export([add_record/3, find_record/2, get_record/2]).
-export([add_function/4, find_function/3]).
-export([find_codec/4]).

-export_type([type_info/0, type_key/0, function_key/0]).

-type type_info() :: #type_info{}.
-type type_key() :: {Name :: atom(), Arity :: arity()}.
-type function_key() :: {Name :: atom(), Arity :: arity()}.

-doc "Creates a fresh `type_info()` for `Module`. `ImplementsCodec` is `true` when the module exports `encode/5`, `decode/5` and is therefore its own codec.".
-spec new(module(), ImplementsCodec :: boolean()) -> type_info().
new(Module, ImplementsCodec) ->
    #type_info{module = Module, implements_codec = ImplementsCodec}.

-doc "Returns the module atom that this `type_info()` was built for.".
-spec get_module(type_info()) -> module().
get_module(#type_info{module = M}) -> M.

-doc "Registers a named type in `TypeInfo`. Called during module analysis.".
-spec add_type(type_info(), atom(), arity(), spectra:sp_type()) -> type_info().
add_type(#type_info{types = Types} = TypeInfo, Name, Arity, Type) ->
    TypeInfo#type_info{types = Types#{{Name, Arity} => Type}}.

-doc "Looks up a type by name and arity. Returns `error` when not found.".
-spec find_type(type_info(), atom(), arity()) -> {ok, spectra:sp_type()} | error.
find_type(#type_info{types = Types}, Name, Arity) ->
    maps:find({Name, Arity}, Types).

-doc "Like `find_type/3` but raises `{type_not_found, Name, Arity}` when absent.".
-spec get_type(type_info(), atom(), arity()) -> spectra:sp_type().
get_type(#type_info{types = Types}, Name, Arity) ->
    case Types of
        #{{Name, Arity} := Type} ->
            Type;
        #{} ->
            erlang:error({type_not_found, Name, Arity})
    end.

-doc "Registers a record type in `TypeInfo`. Called during module analysis.".
-spec add_record(type_info(), atom(), #sp_rec{}) -> type_info().
add_record(#type_info{records = Records} = TypeInfo, Name, Record) ->
    TypeInfo#type_info{records = Records#{Name => Record}}.

-doc "Looks up a record by name. Returns `error` when not found.".
-spec find_record(type_info(), atom()) -> {ok, #sp_rec{}} | error.
find_record(#type_info{records = Records}, Name) ->
    maps:find(Name, Records).

-doc "Like `find_record/2` but raises `{record_not_found, Name}` when absent.".
-spec get_record(type_info(), atom()) -> #sp_rec{}.
get_record(#type_info{records = Records}, Name) ->
    case Records of
        #{Name := Record} ->
            Record;
        #{} ->
            erlang:error({record_not_found, Name})
    end.

-doc "Registers a function's spec list in `TypeInfo`. Called during module analysis.".
-spec add_function(type_info(), atom(), arity(), [spectra:sp_function_spec()]) ->
    type_info().
add_function(#type_info{functions = Functions} = TypeInfo, Name, Arity, FuncSpec) ->
    TypeInfo#type_info{functions = Functions#{{Name, Arity} => FuncSpec}}.

-doc "Looks up a function's spec list by name and arity. Returns `error` when not found.".
-spec find_function(type_info(), atom(), arity()) ->
    {ok, [spectra:sp_function_spec()]} | error.
find_function(#type_info{functions = Functions}, Name, Arity) ->
    maps:find({Name, Arity}, Functions).

-spec find_local_codec(type_info()) -> {ok, module()} | error.
find_local_codec(#type_info{implements_codec = true, module = M}) -> {ok, M};
find_local_codec(#type_info{implements_codec = false}) -> error.

-doc """
Resolves the codec module for `{Mod, TypeRef}`.

Checks the supplied `Codecs` map first, then falls back to the module's own
`spectra_codec` behaviour if it implements one. Calls `code:ensure_loaded/1`
on any codec found in the map so it is ready before its callbacks are dispatched.

The `UseCache` flag is forwarded to `spectra_module_types:get/2` when a local
codec check is needed.
""".
-spec find_codec(module(), spectra:sp_type_reference(), Codecs, UseCache) ->
    {ok, module()} | error
when
    Codecs :: #{spectra:codec_key() => module()},
    UseCache :: boolean().
find_codec(Mod, TypeRef, Codecs, UseCache) ->
    case maps:find({Mod, TypeRef}, Codecs) of
        {ok, CodecMod} ->
            code:ensure_loaded(CodecMod),
            {ok, CodecMod};
        error ->
            find_local_codec(spectra_module_types:get(Mod, UseCache))
    end.

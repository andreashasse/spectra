-module(spectra_type_info).

-include("../include/spectra_internal.hrl").

-ignore_xref([find_function/3, find_record_doc/2]).

-export([new/0]).
-export([add_type/4, find_type/3, get_type/3]).
-export([add_record/3, find_record/2]).
-export([add_function/4, find_function/3]).
-export([add_doc/4, find_doc/3]).
-export([add_record_doc/3, find_record_doc/2]).

-export_type([type_info/0, type_key/0, function_key/0]).

-type type_info() :: #type_info{}.
-type type_key() :: {Name :: atom(), Arity :: arity()}.
-type function_key() :: {Name :: atom(), Arity :: arity()}.

-spec new() -> type_info().
new() ->
    #type_info{}.

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

-spec add_function(type_info(), atom(), arity(), [spectra:sp_function_spec()]) ->
    type_info().
add_function(#type_info{functions = Functions} = TypeInfo, Name, Arity, FuncSpec) ->
    TypeInfo#type_info{functions = Functions#{{Name, Arity} => FuncSpec}}.

-spec find_function(type_info(), atom(), arity()) ->
    {ok, [spectra:sp_function_spec()]} | error.
find_function(#type_info{functions = Functions}, Name, Arity) ->
    maps:find({Name, Arity}, Functions).

-spec add_doc(type_info(), atom(), arity(), spectra:type_doc()) -> type_info().
add_doc(#type_info{docs = Docs} = TypeInfo, Name, Arity, Doc) ->
    TypeInfo#type_info{docs = Docs#{{Name, Arity} => Doc}}.

-spec find_doc(type_info(), atom(), arity()) -> {ok, spectra:type_doc()} | error.
find_doc(#type_info{docs = Docs}, Name, Arity) ->
    maps:find({Name, Arity}, Docs).

-spec add_record_doc(type_info(), atom(), spectra:type_doc()) -> type_info().
add_record_doc(#type_info{record_docs = RecordDocs} = TypeInfo, Name, Doc) ->
    TypeInfo#type_info{record_docs = RecordDocs#{Name => Doc}}.

-spec find_record_doc(type_info(), atom()) -> {ok, spectra:type_doc()} | error.
find_record_doc(#type_info{record_docs = RecordDocs}, Name) ->
    maps:find(Name, RecordDocs).

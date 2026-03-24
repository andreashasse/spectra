-module(spectra_type).

-moduledoc """
Utility functions for inspecting and manipulating `sp_type()` values.

Provides uniform meta-data access across all concrete type records
(`#sp_simple_type{}`, `#sp_union{}`, `#sp_rec{}`, etc.), optional-value
detection, and normalisation of `-spectra()` doc annotations.
""".

-include("../include/spectra_internal.hrl").

-export([
    can_be_missing/2,
    get_meta/1,
    set_meta/2,
    parameters/1,
    type_args/1,
    enrich_ref/2,
    add_doc_to_type/2,
    normalize_doc/1,
    normalize_function_doc/1
]).

%% Functions meant to be used by external libraries like Spectral
-ignore_xref([
    get_meta/1,
    set_meta/2,
    parameters/1,
    type_args/1,
    enrich_ref/2,
    add_doc_to_type/2,
    normalize_doc/1,
    normalize_function_doc/1
]).

-doc """
Checks whether `Type` can hold an absent value (`nil` or `undefined`).

Returns `{true, MissingValue}` when the type is, or expands to, a literal
`nil` or `undefined` (possibly inside a union). The returned `MissingValue`
is the atom to use when the field is omitted. Returns `false` otherwise.
Used during encode/decode to skip optional fields whose value matches the
missing sentinel.
""".
-spec can_be_missing(
    TypeInfo :: spectra:type_info(), Type :: spectra:sp_type()
) ->
    {true, spectra:missing_value()} | false.
can_be_missing(TypeInfo, Type) ->
    case Type of
        #sp_type_with_variables{type = Type2} ->
            can_be_missing(TypeInfo, Type2);
        #sp_union{types = Types} ->
            case lists:filtermap(fun(T) -> can_be_missing(TypeInfo, T) end, Types) of
                [] ->
                    false;
                MissingValues ->
                    {true, lists:last(MissingValues)}
            end;
        #sp_literal{value = LiteralValue} when
            LiteralValue =:= nil orelse LiteralValue =:= undefined
        ->
            {true, LiteralValue};
        #sp_user_type_ref{type_name = TypeName, variables = TypeArgs} ->
            TypeArity = length(TypeArgs),
            RefType = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
            can_be_missing(TypeInfo, RefType);
        _ ->
            false
    end.

-doc "Extracts the meta map from any `sp_type()` record.".
-spec get_meta(spectra:sp_type()) -> spectra:sp_type_meta().
get_meta(#sp_simple_type{meta = Meta}) -> Meta;
get_meta(#sp_tuple{meta = Meta}) -> Meta;
get_meta(#sp_map{meta = Meta}) -> Meta;
get_meta(#sp_rec{meta = Meta}) -> Meta;
get_meta(#sp_type_with_variables{meta = Meta}) -> Meta;
get_meta(#sp_function{meta = Meta}) -> Meta;
get_meta(#sp_union{meta = Meta}) -> Meta;
get_meta(#sp_literal{meta = Meta}) -> Meta;
get_meta(#sp_rec_ref{meta = Meta}) -> Meta;
get_meta(#sp_remote_type{meta = Meta}) -> Meta;
get_meta(#sp_maybe_improper_list{meta = Meta}) -> Meta;
get_meta(#sp_nonempty_improper_list{meta = Meta}) -> Meta;
get_meta(#sp_user_type_ref{meta = Meta}) -> Meta;
get_meta(#sp_var{meta = Meta}) -> Meta;
get_meta(#sp_range{meta = Meta}) -> Meta;
get_meta(#sp_list{meta = Meta}) -> Meta;
get_meta(#sp_nonempty_list{meta = Meta}) -> Meta.

-doc "Returns a copy of `Type` with its meta map replaced by `Meta`.".
-spec set_meta(spectra:sp_type(), spectra:sp_type_meta()) -> spectra:sp_type().
set_meta(#sp_simple_type{} = T, Meta) -> T#sp_simple_type{meta = Meta};
set_meta(#sp_tuple{} = T, Meta) -> T#sp_tuple{meta = Meta};
set_meta(#sp_map{} = T, Meta) -> T#sp_map{meta = Meta};
set_meta(#sp_rec{} = T, Meta) -> T#sp_rec{meta = Meta};
set_meta(#sp_type_with_variables{} = T, Meta) -> T#sp_type_with_variables{meta = Meta};
set_meta(#sp_function{} = T, Meta) -> T#sp_function{meta = Meta};
set_meta(#sp_union{} = T, Meta) -> T#sp_union{meta = Meta};
set_meta(#sp_literal{} = T, Meta) -> T#sp_literal{meta = Meta};
set_meta(#sp_rec_ref{} = T, Meta) -> T#sp_rec_ref{meta = Meta};
set_meta(#sp_remote_type{} = T, Meta) -> T#sp_remote_type{meta = Meta};
set_meta(#sp_maybe_improper_list{} = T, Meta) -> T#sp_maybe_improper_list{meta = Meta};
set_meta(#sp_nonempty_improper_list{} = T, Meta) -> T#sp_nonempty_improper_list{meta = Meta};
set_meta(#sp_user_type_ref{} = T, Meta) -> T#sp_user_type_ref{meta = Meta};
set_meta(#sp_var{} = T, Meta) -> T#sp_var{meta = Meta};
set_meta(#sp_range{} = T, Meta) -> T#sp_range{meta = Meta};
set_meta(#sp_list{} = T, Meta) -> T#sp_list{meta = Meta};
set_meta(#sp_nonempty_list{} = T, Meta) -> T#sp_nonempty_list{meta = Meta}.

-doc "Returns the `parameters` entry from the type's meta map, or `undefined` if absent. Used for string constraints such as `min_length`, `max_length`, and `pattern`.".
-spec parameters(spectra:sp_type()) -> term().
parameters(Type) ->
    maps:get(parameters, get_meta(Type), undefined).

-doc "Extracts the type-variable bindings from an `sp_type()` node. Returns the list of concrete type arguments for `#sp_user_type_ref{}` and `#sp_remote_type{}`, or `[]` for all other types.".
-spec type_args(spectra:sp_type()) -> [spectra:sp_type()].
type_args(#sp_user_type_ref{variables = Vars}) -> Vars;
type_args(#sp_remote_type{mfargs = {_, _, Args}}) -> Args;
type_args(_) -> [].

-doc "Returns a copy of `Ref` with the `parameters` value from `ResolvedType` merged into its meta. Call this before passing a type reference node as the `SpType` argument to a codec callback, so that `spectra_type:parameters/1` returns the same value on the reference as it does on the resolved type.".
-spec enrich_ref(Ref :: spectra:sp_type(), ResolvedType :: spectra:sp_type()) ->
    spectra:sp_type().
enrich_ref(Ref, ResolvedType) ->
    case parameters(ResolvedType) of
        undefined -> Ref;
        Params -> set_meta(Ref, (get_meta(Ref))#{parameters => Params})
    end.

-doc "Attaches a normalised doc map (from a `-spectra()` attribute) to `Type`.".
-spec add_doc_to_type(spectra:sp_type(), map()) -> spectra:sp_type().
add_doc_to_type(Type, DocMap) ->
    Doc = normalize_doc(DocMap),
    Meta = get_meta(Type),
    set_meta(Type, Meta#{doc => Doc}).

-doc "Validates and normalises a raw `-spectra()` annotation map into a `type_doc()`. Raises `{invalid_spectra_field, Key, Value}` on unknown or ill-typed fields.".
-spec normalize_doc(map()) -> spectra:type_doc().
normalize_doc(DocMap) ->
    maps:fold(fun add_doc_field/3, #{}, DocMap).

-spec add_doc_field(atom(), term(), spectra:type_doc()) -> spectra:type_doc().
add_doc_field(title, Value, Acc) when is_binary(Value) ->
    Acc#{title => Value};
add_doc_field(description, Value, Acc) when is_binary(Value) ->
    Acc#{description => Value};
add_doc_field(deprecated, Value, Acc) when is_boolean(Value) ->
    Acc#{deprecated => Value};
add_doc_field(examples, Value, Acc) when is_list(Value) ->
    Acc#{examples => Value};
add_doc_field(examples_function, {Module, Function, Args} = MFA, Acc) when
    is_atom(Module), is_atom(Function), is_list(Args)
->
    Acc#{examples_function => MFA};
add_doc_field(Key, Value, _Acc) ->
    erlang:error({invalid_spectra_field, Key, Value}).

-doc "Like `normalize_doc/1` but for function-level `-spectra()` annotations. Accepts `summary`, `description`, and `deprecated`.".
-spec normalize_function_doc(map()) -> spectra:function_doc().
normalize_function_doc(DocMap) ->
    maps:fold(fun add_function_doc_field/3, #{}, DocMap).

-spec add_function_doc_field(atom(), term(), spectra:function_doc()) -> spectra:function_doc().
add_function_doc_field(summary, Value, Acc) when is_binary(Value) ->
    Acc#{summary => Value};
add_function_doc_field(description, Value, Acc) when is_binary(Value) ->
    Acc#{description => Value};
add_function_doc_field(deprecated, Value, Acc) when is_boolean(Value) ->
    Acc#{deprecated => Value};
add_function_doc_field(Key, Value, _Acc) ->
    erlang:error({invalid_spectra_field, Key, Value}).

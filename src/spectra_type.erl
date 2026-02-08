-module(spectra_type).

-include("../include/spectra_internal.hrl").

-export([can_be_missing/2, type_reference/1, get_meta/1, set_meta/2]).

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

-spec type_reference(spectra:sp_type_or_ref()) -> {true, spectra:sp_type_reference()} | false.
type_reference({type, _, _} = Type) ->
    {true, Type};
type_reference({record, _} = Type) ->
    {true, Type};
type_reference(_) ->
    false.

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

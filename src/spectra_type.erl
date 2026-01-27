-module(spectra_type).

-include("../include/spectra_internal.hrl").

-export([can_be_missing/2, type_reference/1]).

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

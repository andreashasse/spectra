-module(spectra_type).

-include("../include/spectra_internal.hrl").

-export([can_be_missing/2, is_type_reference/1, to_string/1]).

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
            {ok, RefType} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
            can_be_missing(TypeInfo, RefType);
        _ ->
            false
    end.

-spec is_type_reference(spectra:sp_type_or_ref()) -> boolean().
is_type_reference({type, _, _}) ->
    true;
is_type_reference({record, _}) ->
    true;
is_type_reference(_) ->
    false.

-doc """
Converts a sp_type() record to a human-readable string representation.

### Examples:
```
1> spectra_type:to_string(#sp_simple_type{type = integer}).
<<\"integer\">>

2> spectra_type:to_string(#sp_range{lower_bound = 0, upper_bound = 100}).
<<\"integer(0..100)\">>

3> spectra_type:to_string(#sp_list{type = #sp_simple_type{type = binary}}).
<<\"list(binary)\">>
```
""".
-spec to_string(spectra:sp_type()) -> binary().
%% Simple types
to_string(#sp_simple_type{type = string}) ->
    <<"string">>;
to_string(#sp_simple_type{type = nonempty_string}) ->
    <<"nonempty_string">>;
to_string(#sp_simple_type{type = integer}) ->
    <<"integer">>;
to_string(#sp_simple_type{type = non_neg_integer}) ->
    <<"non_neg_integer">>;
to_string(#sp_simple_type{type = neg_integer}) ->
    <<"neg_integer">>;
to_string(#sp_simple_type{type = pos_integer}) ->
    <<"pos_integer">>;
to_string(#sp_simple_type{type = float}) ->
    <<"float">>;
to_string(#sp_simple_type{type = number}) ->
    <<"number">>;
to_string(#sp_simple_type{type = boolean}) ->
    <<"boolean">>;
to_string(#sp_simple_type{type = binary}) ->
    <<"binary">>;
to_string(#sp_simple_type{type = nonempty_binary}) ->
    <<"nonempty_binary">>;
to_string(#sp_simple_type{type = bitstring}) ->
    <<"bitstring">>;
to_string(#sp_simple_type{type = nonempty_bitstring}) ->
    <<"nonempty_bitstring">>;
to_string(#sp_simple_type{type = atom}) ->
    <<"atom">>;
to_string(#sp_simple_type{type = term}) ->
    <<"term">>;
to_string(#sp_simple_type{type = reference}) ->
    <<"reference">>;
to_string(#sp_simple_type{type = pid}) ->
    <<"pid">>;
to_string(#sp_simple_type{type = port}) ->
    <<"port">>;
to_string(#sp_simple_type{type = iolist}) ->
    <<"iolist">>;
to_string(#sp_simple_type{type = iodata}) ->
    <<"iodata">>;
to_string(#sp_simple_type{type = none}) ->
    <<"none">>;
to_string(#sp_simple_type{type = map}) ->
    <<"map">>;
%% Range types
to_string(#sp_range{lower_bound = Min, upper_bound = Max}) ->
    MinBin = integer_to_binary(Min),
    MaxBin = integer_to_binary(Max),
    <<"integer(", MinBin/binary, "..", MaxBin/binary, ")">>;
%% List types
to_string(#sp_list{type = Type}) ->
    TypeStr = to_string(Type),
    <<"list(", TypeStr/binary, ")">>;
to_string(#sp_nonempty_list{type = Type}) ->
    TypeStr = to_string(Type),
    <<"nonempty_list(", TypeStr/binary, ")">>;
to_string(#sp_maybe_improper_list{elements = Elem, tail = Tail}) ->
    ElemStr = to_string(Elem),
    TailStr = to_string(Tail),
    <<"maybe_improper_list(", ElemStr/binary, ", ", TailStr/binary, ")">>;
to_string(#sp_nonempty_improper_list{elements = Elem, tail = Tail}) ->
    ElemStr = to_string(Elem),
    TailStr = to_string(Tail),
    <<"nonempty_improper_list(", ElemStr/binary, ", ", TailStr/binary, ")">>;
%% Map types
to_string(#sp_map{struct_name = undefined, fields = []}) ->
    <<"map">>;
to_string(#sp_map{struct_name = undefined, fields = Fields}) when length(Fields) =< 3 ->
    %% Show field types for small maps
    FieldStrs = [format_map_field(F) || F <- Fields],
    FieldList = join(FieldStrs, <<", ">>),
    <<"map(", FieldList/binary, ")">>;
to_string(#sp_map{struct_name = undefined}) ->
    <<"map">>;
to_string(#sp_map{struct_name = Name}) ->
    NameBin = atom_to_binary(Name, utf8),
    <<"struct ", NameBin/binary>>;
%% Record types
to_string(#sp_rec{name = Name}) ->
    NameBin = atom_to_binary(Name, utf8),
    <<"record ", NameBin/binary>>;
to_string(#sp_rec_ref{record_name = Name, field_types = []}) ->
    NameBin = atom_to_binary(Name, utf8),
    <<"record ", NameBin/binary>>;
to_string(#sp_rec_ref{record_name = Name, field_types = _}) ->
    %% For parameterized records, just show the name
    NameBin = atom_to_binary(Name, utf8),
    <<"record ", NameBin/binary>>;
%% Tuple types
to_string(#sp_tuple{fields = any}) ->
    <<"tuple">>;
to_string(#sp_tuple{fields = Fields}) when is_list(Fields), length(Fields) =< 5 ->
    FieldStrs = [to_string(F) || F <- Fields],
    FieldList = join(FieldStrs, <<", ">>),
    <<"{", FieldList/binary, "}">>;
to_string(#sp_tuple{fields = _}) ->
    <<"tuple">>;
%% Union types
to_string(#sp_union{types = Types}) when length(Types) =< 3 ->
    TypeStrs = [to_string(T) || T <- Types],
    join(TypeStrs, <<" | ">>);
to_string(#sp_union{types = _}) ->
    <<"union">>;
%% Literal types
to_string(#sp_literal{value = Value}) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
to_string(#sp_literal{value = Value}) when is_integer(Value) ->
    integer_to_binary(Value);
to_string(#sp_literal{value = []}) ->
    <<"[]">>;
%% Function types
to_string(#sp_function{args = any, return = Ret}) ->
    RetStr = to_string(Ret),
    <<"fun(...) -> ", RetStr/binary>>;
to_string(#sp_function{args = Args, return = Ret}) when is_list(Args), length(Args) =< 3 ->
    ArgStrs = [to_string(A) || A <- Args],
    ArgList = join(ArgStrs, <<", ">>),
    RetStr = to_string(Ret),
    <<"fun((", ArgList/binary, ") -> ", RetStr/binary, ")">>;
to_string(#sp_function{args = _, return = Ret}) ->
    RetStr = to_string(Ret),
    <<"fun(...) -> ", RetStr/binary>>;
%% User type references
to_string(#sp_user_type_ref{type_name = Name, variables = []}) ->
    atom_to_binary(Name, utf8);
to_string(#sp_user_type_ref{type_name = Name, variables = Vars}) when length(Vars) =< 3 ->
    NameBin = atom_to_binary(Name, utf8),
    VarStrs = [to_string(V) || V <- Vars],
    VarList = join(VarStrs, <<", ">>),
    <<NameBin/binary, "(", VarList/binary, ")">>;
to_string(#sp_user_type_ref{type_name = Name, variables = _}) ->
    NameBin = atom_to_binary(Name, utf8),
    <<NameBin/binary, "(...)">>;
%% Type variables
to_string(#sp_var{name = Name}) ->
    atom_to_binary(Name, utf8);
%% Remote types
to_string(#sp_remote_type{mfargs = {Mod, Fun, Args}}) when length(Args) =< 2 ->
    ModBin = atom_to_binary(Mod, utf8),
    FunBin = atom_to_binary(Fun, utf8),
    case Args of
        [] ->
            <<ModBin/binary, ":", FunBin/binary>>;
        _ ->
            ArgStrs = [to_string(A) || A <- Args],
            ArgList = join(ArgStrs, <<", ">>),
            <<ModBin/binary, ":", FunBin/binary, "(", ArgList/binary, ")">>
    end;
to_string(#sp_remote_type{mfargs = {Mod, Fun, _}}) ->
    ModBin = atom_to_binary(Mod, utf8),
    FunBin = atom_to_binary(Fun, utf8),
    <<ModBin/binary, ":", FunBin/binary, "(...)">>;
%% Type with variables
to_string(#sp_type_with_variables{type = Type, vars = []}) ->
    to_string(Type);
to_string(#sp_type_with_variables{type = Type, vars = _}) ->
    %% For types with variables, just show the base type
    to_string(Type);
%% Fallback
to_string(_) ->
    <<"unknown">>.

%% Helper to format map fields
-spec format_map_field(spectra:map_field()) -> binary().
format_map_field(#literal_map_field{name = Name, val_type = ValType, kind = exact}) ->
    NameBin = format_key(Name),
    ValStr = to_string(ValType),
    <<NameBin/binary, " := ", ValStr/binary>>;
format_map_field(#literal_map_field{name = Name, val_type = ValType, kind = assoc}) ->
    NameBin = format_key(Name),
    ValStr = to_string(ValType),
    <<NameBin/binary, " => ", ValStr/binary>>;
format_map_field(#typed_map_field{key_type = KeyType, val_type = ValType, kind = exact}) ->
    KeyStr = to_string(KeyType),
    ValStr = to_string(ValType),
    <<KeyStr/binary, " := ", ValStr/binary>>;
format_map_field(#typed_map_field{key_type = KeyType, val_type = ValType, kind = assoc}) ->
    KeyStr = to_string(KeyType),
    ValStr = to_string(ValType),
    <<KeyStr/binary, " => ", ValStr/binary>>.

%% Helper to format map keys
-spec format_key(atom() | integer()) -> binary().
format_key(Key) when is_atom(Key) ->
    atom_to_binary(Key, utf8);
format_key(Key) when is_integer(Key) ->
    integer_to_binary(Key).

%% Helper to join binaries with a separator
-spec join([binary()], binary()) -> binary().
join([], _Sep) ->
    <<>>;
join([H], _Sep) ->
    H;
join([H | T], Sep) ->
    Rest = join(T, Sep),
    <<H/binary, Sep/binary, Rest/binary>>.

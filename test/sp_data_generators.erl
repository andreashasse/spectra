-module(sp_data_generators).

-include_lib("proper/include/proper.hrl").
-include("../include/spectra_internal.hrl").

-export([gen_data/2]).

%% @doc Generate valid Erlang data for a given type
gen_data(_TypeInfo, #sp_simple_type{type = integer}) ->
    integer();
gen_data(_TypeInfo, #sp_simple_type{type = non_neg_integer}) ->
    non_neg_integer();
gen_data(_TypeInfo, #sp_simple_type{type = neg_integer}) ->
    neg_integer();
gen_data(_TypeInfo, #sp_simple_type{type = pos_integer}) ->
    pos_integer();
gen_data(_TypeInfo, #sp_simple_type{type = float}) ->
    float();
gen_data(_TypeInfo, #sp_simple_type{type = number}) ->
    oneof([integer(), float()]);
gen_data(_TypeInfo, #sp_simple_type{type = boolean}) ->
    boolean();
gen_data(_TypeInfo, #sp_simple_type{type = atom}) ->
    atom();
gen_data(_TypeInfo, #sp_simple_type{type = string}) ->
    utf8_string();
gen_data(_TypeInfo, #sp_simple_type{type = nonempty_string}) ->
    non_empty(utf8_string());
gen_data(_TypeInfo, #sp_simple_type{type = binary}) ->
    utf8();
gen_data(_TypeInfo, #sp_simple_type{type = nonempty_binary}) ->
    ?SUCHTHAT(B, utf8(), byte_size(B) > 0);
gen_data(_TypeInfo, #sp_simple_type{type = bitstring}) ->
    bitstring();
gen_data(_TypeInfo, #sp_simple_type{type = nonempty_bitstring}) ->
    ?SUCHTHAT(B, bitstring(), bit_size(B) > 0);
gen_data(_TypeInfo, #sp_simple_type{type = term}) ->
    term();
gen_data(_TypeInfo, #sp_simple_type{type = reference}) ->
    ?LET(_, integer(), make_ref());
gen_data(_TypeInfo, #sp_simple_type{type = pid}) ->
    ?LET(_, integer(), self());
gen_data(_TypeInfo, #sp_simple_type{type = port}) ->
    ?LET(_, integer(), hd(erlang:ports()));
gen_data(_TypeInfo, #sp_simple_type{type = iolist}) ->
    oneof([list(oneof([byte(), binary(), list(byte())])), binary()]);
gen_data(_TypeInfo, #sp_simple_type{type = iodata}) ->
    oneof([list(oneof([byte(), binary()])), binary()]);
gen_data(_TypeInfo, #sp_simple_type{type = none}) ->
    ?LET(_, integer(), throw(none_type_cannot_generate_data));
gen_data(_TypeInfo, #sp_simple_type{type = map}) ->
    map(atom(), term());
gen_data(_TypeInfo, #sp_literal{value = Value}) ->
    Value;
gen_data(_TypeInfo, #sp_range{type = integer, lower_bound = Lower, upper_bound = Upper}) ->
    choose(Lower, Upper);
gen_data(TypeInfo, #sp_list{type = ElemType}) ->
    list(gen_data(TypeInfo, ElemType));
gen_data(TypeInfo, #sp_nonempty_list{type = ElemType}) ->
    non_empty(list(gen_data(TypeInfo, ElemType)));
gen_data(_TypeInfo, #sp_tuple{fields = any}) ->
    ?SIZED(Size, ?LET(Len, choose(0, Size), vector(Len, term())));
gen_data(TypeInfo, #sp_tuple{fields = Fields}) ->
    ?LET(Values, [gen_data(TypeInfo, Field) || Field <- Fields], list_to_tuple(Values));
gen_data(TypeInfo, #sp_map{fields = Fields}) ->
    gen_map_data(TypeInfo, Fields);
gen_data(TypeInfo, #sp_union{types = Types}) ->
    oneof([gen_data(TypeInfo, T) || T <- Types]);
gen_data(TypeInfo, #sp_rec_ref{record_name = RecordName, field_types = FieldTypes}) ->
    FieldValues = [gen_data(TypeInfo, FieldType) || {_FieldName, FieldType} <- FieldTypes],
    list_to_tuple([RecordName | FieldValues]);
gen_data(_TypeInfo, #sp_var{name = _Name}) ->
    term();
gen_data(TypeInfo, #sp_user_type_ref{type_name = TypeName, variables = Variables}) ->
    {ok, TypeDef} = spectra_type_info:get_type(TypeInfo, TypeName, length(Variables)),
    gen_data(TypeInfo, TypeDef);
gen_data(TypeInfo, #sp_maybe_improper_list{elements = ElemType, tail = TailType}) ->
    oneof([
        list(gen_data(TypeInfo, ElemType)),
        ?LET(
            {Elems, Tail},
            {non_empty(list(gen_data(TypeInfo, ElemType))), gen_data(TypeInfo, TailType)},
            lists:foldr(fun(E, Acc) -> [E | Acc] end, Tail, Elems)
        )
    ]);
gen_data(TypeInfo, #sp_nonempty_improper_list{elements = ElemType, tail = TailType}) ->
    ?LET(
        {Elems, Tail},
        {non_empty(list(gen_data(TypeInfo, ElemType))), gen_data(TypeInfo, TailType)},
        lists:foldr(fun(E, Acc) -> [E | Acc] end, Tail, Elems)
    );
gen_data(TypeInfo, #sp_type_with_variables{type = Type, vars = _Vars}) ->
    gen_data(TypeInfo, Type);
gen_data(_TypeInfo, #sp_function{args = _Args, return = _Return}) ->
    ?LET(_, integer(), fun() -> ok end);
gen_data(_TypeInfo, #sp_remote_type{mfargs = {_Module, _Function, _Args}}) ->
    %% For remote types, we'd need to resolve them - for now, generate term()
    term();
gen_data(TypeInfo, #sp_rec{name = Name, fields = Fields, arity = _Arity}) ->
    FieldValues = [gen_data(TypeInfo, FieldType) || {_FieldName, FieldType} <- Fields],
    list_to_tuple([Name | FieldValues]).

%% Helper function to generate map data from map fields
gen_map_data(TypeInfo, Fields) ->
    gen_map_data(TypeInfo, Fields, #{}).

gen_map_data(_TypeInfo, [], Acc) ->
    Acc;
gen_map_data(TypeInfo, [#literal_map_field{name = Name, val_type = ValType} | Rest], Acc) ->
    ?LET(
        Value,
        gen_data(TypeInfo, ValType),
        gen_map_data(TypeInfo, Rest, Acc#{Name => Value})
    );
gen_map_data(TypeInfo, [#typed_map_field{key_type = KeyType, val_type = ValType} | Rest], Acc) ->
    ?LET(
        {Key, Value},
        {gen_data(TypeInfo, KeyType), gen_data(TypeInfo, ValType)},
        gen_map_data(TypeInfo, Rest, Acc#{Key => Value})
    ).

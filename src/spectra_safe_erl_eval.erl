-module(spectra_safe_erl_eval).

%% Safe literal evaluation of Erlang abstract-form expressions.
%% Only handles constant forms (atoms, numbers, binaries, strings,
%% tuples, lists, maps). Function calls and other non-literal
%% expressions return `undefined` rather than executing code.

-export([eval_record_default/1, integer_value/1]).

-spec eval_record_default(dynamic()) -> undefined | {value, term()}.
eval_record_default({atom, _, Value}) when is_atom(Value) ->
    {value, Value};
eval_record_default({float, _, Value}) when is_float(Value) ->
    {value, Value};
eval_record_default({nil, _}) ->
    {value, []};
eval_record_default({string, _, Value}) when is_list(Value) ->
    {value, Value};
eval_record_default({bin, _, Segments}) ->
    try
        Parts = [bin_segment_value(S) || S <- Segments],
        {value, iolist_to_binary(Parts)}
    catch
        _:_ -> undefined
    end;
eval_record_default({tuple, _, Elements}) ->
    eval_literal_list(Elements, fun erlang:list_to_tuple/1);
eval_record_default({cons, _, _Head, _Tail} = Expr) ->
    eval_literal_cons(Expr);
eval_record_default({map, _, Assocs}) ->
    eval_literal_map(Assocs);
eval_record_default(Expr) ->
    try
        {value, integer_value(Expr)}
    catch
        _:_ -> undefined
    end.

-spec integer_value(dynamic()) -> integer().
integer_value({char, _, Value}) when is_integer(Value) ->
    Value;
integer_value({integer, _, Value}) when is_integer(Value) ->
    Value;
integer_value({op, _, Operator, Left, Right}) ->
    case Operator of
        '-' ->
            integer_value(Left) - integer_value(Right);
        '+' ->
            integer_value(Left) + integer_value(Right);
        '*' ->
            integer_value(Left) * integer_value(Right);
        'div' ->
            integer_value(Left) div integer_value(Right);
        'rem' ->
            integer_value(Left) rem integer_value(Right);
        'band' ->
            integer_value(Left) band integer_value(Right);
        'bor' ->
            integer_value(Left) bor integer_value(Right);
        'bxor' ->
            integer_value(Left) bxor integer_value(Right);
        'bsl' ->
            integer_value(Left) bsl integer_value(Right);
        'bsr' ->
            integer_value(Left) bsr integer_value(Right)
    end;
integer_value({op, _, Operator, Unary}) ->
    case Operator of
        '-' ->
            -integer_value(Unary);
        '+' ->
            integer_value(Unary);
        'bnot' ->
            bnot integer_value(Unary)
    end.

-spec eval_literal_list(list(), fun((list()) -> term())) -> undefined | {value, term()}.
eval_literal_list(Exprs, Wrap) ->
    Results = [eval_record_default(E) || E <- Exprs],
    case lists:all(fun(R) -> R =/= undefined end, Results) of
        true -> {value, Wrap([V || {value, V} <- Results])};
        false -> undefined
    end.

-spec eval_literal_cons(dynamic()) -> undefined | {value, dynamic()}.
eval_literal_cons({nil, _}) ->
    {value, []};
eval_literal_cons({cons, _, Head, Tail}) ->
    case {eval_record_default(Head), eval_literal_cons(Tail)} of
        {{value, H}, {value, T}} -> {value, [H | T]};
        _ -> undefined
    end;
eval_literal_cons(_) ->
    undefined.

-spec eval_literal_map(list()) -> undefined | {value, term()}.
eval_literal_map(Assocs) ->
    Pairs = [eval_map_assoc(A) || A <- Assocs],
    case lists:all(fun(R) -> R =/= undefined end, Pairs) of
        true -> {value, maps:from_list([KV || {value, KV} <- Pairs])};
        false -> undefined
    end.

-spec eval_map_assoc(dynamic()) -> undefined | {value, {term(), term()}}.
eval_map_assoc({map_field_assoc, _, K, V}) ->
    case {eval_record_default(K), eval_record_default(V)} of
        {{value, KV}, {value, VV}} -> {value, {KV, VV}};
        _ -> undefined
    end;
eval_map_assoc(_) ->
    undefined.

-spec bin_segment_value(dynamic()) -> iodata().
bin_segment_value({bin_element, _, {string, _, S}, default, default}) -> S;
bin_segment_value({bin_element, _, {char, _, V}, default, default}) -> V;
bin_segment_value({bin_element, _, Expr, default, default}) -> integer_value(Expr).

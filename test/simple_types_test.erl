-module(simple_types_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-compile(nowarn_unused_type).

-type my_arity() :: arity().
-type my_byte() :: byte().
-type my_char() :: char().
-type my_mfa() :: mfa().
-type my_any() :: any().
-type my_timeout() :: timeout().
-type my_pid() :: pid().
-type my_iodata() :: iodata().
-type my_iolist() :: iolist().
-type my_port() :: port().
-type my_reference() :: reference().
-type my_node() :: node().
-type my_identifier() :: identifier().
-type my_literal() :: 1.
-type my_list() :: list().
-type my_term() :: term().
-type my_nonempty_list() :: nonempty_list().
%% My code formater re-writes nil to [].
-type my_nil() :: [].
-type my_dynamic() :: dynamic().
-type my_nonempty_binary() :: nonempty_binary().
-type my_bitstring() :: bitstring().
-type my_nonempty_bitstring() :: nonempty_bitstring().
-type my_no_return() :: no_return().
-type my_none() :: none().
-type my_range() :: -2..2.

missing_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% range
    %% FIXME: more specific matching
    ?assertMatch(
        {error, _}, spectra:encode(json, ?MODULE, {type, my_range, 0}, -0.0, [pre_encoded])
    ),

    %% arity
    MyArityType = spectra_type_info:get_type(TypeInfo, my_arity, 0),
    ?assertEqual(
        #sp_range{
            type = integer,
            lower_bound = 0,
            upper_bound = 255,
            meta = #{name => {type, my_arity, 0}}
        },
        MyArityType
    ),
    ?assertEqual({ok, 42}, spectra:encode(json, ?MODULE, {type, my_arity, 0}, 42, [pre_encoded])),
    ?assertEqual({ok, 42}, spectra:decode(json, ?MODULE, {type, my_arity, 0}, 42, [pre_decoded])),

    %% byte
    MyByteType = spectra_type_info:get_type(TypeInfo, my_byte, 0),
    ?assertEqual(
        #sp_range{
            type = integer,
            lower_bound = 0,
            upper_bound = 255,
            meta = #{name => {type, my_byte, 0}}
        },
        MyByteType
    ),
    ?assertEqual({ok, 42}, spectra:encode(json, ?MODULE, {type, my_byte, 0}, 42, [pre_encoded])),
    ?assertEqual({ok, 42}, spectra:decode(json, ?MODULE, {type, my_byte, 0}, 42, [pre_decoded])),

    %% char
    MyCharType = spectra_type_info:get_type(TypeInfo, my_char, 0),
    ?assertEqual(
        #sp_range{
            type = integer,
            lower_bound = 0,
            upper_bound = 1114111,
            meta = #{name => {type, my_char, 0}}
        },
        MyCharType
    ),
    ?assertEqual({ok, 42}, spectra:encode(json, ?MODULE, {type, my_char, 0}, 42, [pre_encoded])),
    ?assertEqual({ok, 42}, spectra:decode(json, ?MODULE, {type, my_char, 0}, 42, [pre_decoded])),

    %% mfa
    MyMfaType = spectra_type_info:get_type(TypeInfo, my_mfa, 0),
    ?assertEqual(
        #sp_tuple{
            fields =
                [
                    #sp_simple_type{type = atom},
                    #sp_simple_type{type = atom},
                    #sp_range{
                        type = integer,
                        lower_bound = 0,
                        upper_bound = 255
                    }
                ],
            meta = #{name => {type, my_mfa, 0}}
        },
        MyMfaType
    ),
    ?assertError(
        {type_not_supported, _},
        spectra:encode(json, ?MODULE, {type, my_mfa, 0}, {module, function, 42}, [pre_encoded])
    ),
    ?assertError(
        {type_not_supported, _},
        spectra:decode(json, ?MODULE, {type, my_mfa, 0}, {module, function, 42}, [pre_decoded])
    ),

    %% any
    MyTermType = spectra_type_info:get_type(TypeInfo, my_term, 0),
    ?assertEqual(#sp_simple_type{type = term, meta = #{name => {type, my_term, 0}}}, MyTermType),
    ?assertEqual({ok, 42}, spectra:encode(json, ?MODULE, {type, my_term, 0}, 42, [pre_encoded])),
    ?assertEqual({ok, 42}, spectra:decode(json, ?MODULE, {type, my_term, 0}, 42, [pre_decoded])),

    %% timeout
    MyTimeoutType = spectra_type_info:get_type(TypeInfo, my_timeout, 0),
    ?assertEqual(
        #sp_union{
            types =
                [
                    #sp_simple_type{type = non_neg_integer},
                    #sp_literal{value = infinity, binary_value = <<"infinity">>}
                ],
            meta = #{name => {type, my_timeout, 0}}
        },
        MyTimeoutType
    ),
    ?assertMatch(
        {error, [#sp_error{type = no_match}]},
        spectra:encode(json, ?MODULE, {type, my_timeout, 0}, <<"infinity">>, [pre_encoded])
    ),
    ?assertEqual(
        {ok, <<"infinity">>},
        spectra:encode(json, ?MODULE, {type, my_timeout, 0}, infinity, [pre_encoded])
    ),
    ?assertEqual({ok, 42}, spectra:encode(json, ?MODULE, {type, my_timeout, 0}, 42, [pre_encoded])),
    ?assertEqual(
        {ok, infinity},
        spectra:decode(json, ?MODULE, {type, my_timeout, 0}, infinity, [pre_decoded])
    ),
    ?assertEqual(
        {ok, 1000}, spectra:decode(json, ?MODULE, {type, my_timeout, 0}, 1000, [pre_decoded])
    ),

    %% pid
    MyPidType = spectra_type_info:get_type(TypeInfo, my_pid, 0),
    ?assertEqual(#sp_simple_type{type = pid, meta = #{name => {type, my_pid, 0}}}, MyPidType),
    ?assertError(
        {type_not_supported, _},
        spectra:encode(json, ?MODULE, {type, my_pid, 0}, self(), [pre_encoded])
    ),
    ?assertError(
        {type_not_supported, _},
        spectra:decode(json, ?MODULE, {type, my_pid, 0}, <<"not_a_pid">>, [pre_decoded])
    ),

    %% iodata
    IoList1 = [<<"hello">>, <<"world">>],
    IoList2 = [<<"hello">> | <<"world">>],
    IoList3 = [104, <<"ello">>, [<<"wo">>, 114 | <<"l">>] | <<"d">>],

    MyIodataType = spectra_type_info:get_type(TypeInfo, my_iodata, 0),
    ?assertEqual(
        #sp_simple_type{type = iodata, meta = #{name => {type, my_iodata, 0}}}, MyIodataType
    ),
    ?assertEqual(
        {ok, <<"helloworld">>},
        spectra:encode(json, ?MODULE, {type, my_iodata, 0}, IoList1, [pre_encoded])
    ),
    ?assertEqual(
        {ok, <<"helloworld">>},
        spectra:encode(json, ?MODULE, {type, my_iodata, 0}, IoList2, [pre_encoded])
    ),
    ?assertEqual(
        {ok, <<"helloworld">>},
        spectra:encode(json, ?MODULE, {type, my_iodata, 0}, IoList3, [pre_encoded])
    ),
    ?assertEqual(
        {ok, <<"helloworld">>},
        spectra:encode(json, ?MODULE, {type, my_iodata, 0}, <<"helloworld">>, [pre_encoded])
    ),
    ?assertEqual(
        {ok, <<"helloworld">>},
        spectra:decode(json, ?MODULE, {type, my_iodata, 0}, <<"helloworld">>, [pre_decoded])
    ),

    MyIolistType = spectra_type_info:get_type(TypeInfo, my_iolist, 0),
    ?assertEqual(
        #sp_simple_type{type = iolist, meta = #{name => {type, my_iolist, 0}}}, MyIolistType
    ),
    ?assertEqual(
        {ok, <<"helloworld">>},
        spectra:encode(json, ?MODULE, {type, my_iolist, 0}, IoList1, [pre_encoded])
    ),
    ?assertEqual(
        {ok, <<"helloworld">>},
        spectra:encode(json, ?MODULE, {type, my_iolist, 0}, IoList2, [pre_encoded])
    ),
    ?assertEqual(
        {ok, <<"helloworld">>},
        spectra:encode(json, ?MODULE, {type, my_iolist, 0}, IoList3, [pre_encoded])
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra:encode(json, ?MODULE, {type, my_iolist, 0}, <<"helloworld">>, [pre_encoded])
    ),
    ?assertEqual(
        {ok, [<<"helloworld">>]},
        spectra:decode(json, ?MODULE, {type, my_iolist, 0}, <<"helloworld">>, [pre_decoded])
    ),

    %% port
    MyPortType = spectra_type_info:get_type(TypeInfo, my_port, 0),
    ?assertEqual(#sp_simple_type{type = port, meta = #{name => {type, my_port, 0}}}, MyPortType),
    ?assertError(
        {type_not_supported, _},
        spectra:encode(json, ?MODULE, {type, my_port, 0}, not_a_port, [pre_encoded])
    ),
    ?assertError(
        {type_not_supported, _},
        spectra:decode(json, ?MODULE, {type, my_port, 0}, <<"not_a_port">>, [pre_decoded])
    ),

    %% reference
    MyReferenceType = spectra_type_info:get_type(TypeInfo, my_reference, 0),
    ?assertEqual(
        #sp_simple_type{type = reference, meta = #{name => {type, my_reference, 0}}},
        MyReferenceType
    ),
    ?assertError(
        {type_not_supported, _},
        spectra:encode(json, ?MODULE, {type, my_reference, 0}, make_ref(), [pre_encoded])
    ),
    ?assertError(
        {type_not_supported, _},
        spectra:decode(json, ?MODULE, {type, my_reference, 0}, <<"not_a_reference">>, [pre_decoded])
    ),

    %% node
    MyNodeType = spectra_type_info:get_type(TypeInfo, my_node, 0),
    ?assertEqual(#sp_simple_type{type = atom, meta = #{name => {type, my_node, 0}}}, MyNodeType),
    ?assertEqual(
        {ok, nonode@nohost},
        spectra:encode(json, ?MODULE, {type, my_node, 0}, nonode@nohost, [pre_encoded])
    ),
    ?assertEqual(
        {ok, nonode@nohost},
        spectra:decode(json, ?MODULE, {type, my_node, 0}, <<"nonode@nohost">>, [pre_decoded])
    ),

    %% identifier
    MyIdentifierType = spectra_type_info:get_type(TypeInfo, my_identifier, 0),
    ?assertEqual(
        #sp_union{
            types =
                [
                    #sp_simple_type{type = pid},
                    #sp_simple_type{type = port},
                    #sp_simple_type{type = reference}
                ],
            meta = #{name => {type, my_identifier, 0}}
        },
        MyIdentifierType
    ),
    ?assertError(
        {type_not_supported, _},
        spectra:encode(json, ?MODULE, {type, my_identifier, 0}, my_identifier, [pre_encoded])
    ),
    ?assertError(
        {type_not_supported, _},
        spectra:encode(json, ?MODULE, {type, my_identifier, 0}, my_identifier, [pre_encoded])
    ),

    %% literal
    MyLiteralType = spectra_type_info:get_type(TypeInfo, my_literal, 0),
    ?assertEqual(
        #sp_literal{value = 1, binary_value = <<"1">>, meta = #{name => {type, my_literal, 0}}},
        MyLiteralType
    ),
    ?assertEqual({ok, 1}, spectra:encode(json, ?MODULE, {type, my_literal, 0}, 1, [pre_encoded])),
    ?assertEqual({ok, 1}, spectra:decode(json, ?MODULE, {type, my_literal, 0}, 1, [pre_decoded])),

    %% list
    MyListType = spectra_type_info:get_type(TypeInfo, my_list, 0),
    ?assertEqual(
        #sp_list{type = #sp_simple_type{type = term}, meta = #{name => {type, my_list, 0}}},
        MyListType
    ),
    ?assertEqual(
        {ok, [1, 2, 3]},
        spectra:encode(json, ?MODULE, {type, my_list, 0}, [1, 2, 3], [pre_encoded])
    ),
    ?assertEqual(
        {ok, [1, 2, 3]},
        spectra:decode(json, ?MODULE, {type, my_list, 0}, [1, 2, 3], [pre_decoded])
    ),

    %% term
    MyTermType2 = spectra_type_info:get_type(TypeInfo, my_term, 0),
    ?assertEqual(#sp_simple_type{type = term, meta = #{name => {type, my_term, 0}}}, MyTermType2),
    ?assertEqual({ok, 42}, spectra:encode(json, ?MODULE, {type, my_term, 0}, 42, [pre_encoded])),
    ?assertEqual({ok, 42}, spectra:decode(json, ?MODULE, {type, my_term, 0}, 42, [pre_decoded])),

    %% nonempty_list
    MyNonemptyListType = spectra_type_info:get_type(TypeInfo, my_nonempty_list, 0),
    ?assertEqual(
        #sp_nonempty_list{
            type = #sp_simple_type{type = term}, meta = #{name => {type, my_nonempty_list, 0}}
        },
        MyNonemptyListType
    ),
    ?assertEqual(
        {ok, [1, 2, 3]},
        spectra:encode(json, ?MODULE, {type, my_nonempty_list, 0}, [1, 2, 3], [pre_encoded])
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra:encode(json, ?MODULE, {type, my_nonempty_list, 0}, [], [pre_encoded])
    ),
    ?assertEqual(
        {ok, [1, 2, 3]},
        spectra:decode(json, ?MODULE, {type, my_nonempty_list, 0}, [1, 2, 3], [pre_decoded])
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra:decode(json, ?MODULE, {type, my_nonempty_list, 0}, [], [pre_decoded])
    ),

    %% nil
    MyNilType = spectra_type_info:get_type(TypeInfo, my_nil, 0),
    ?assertEqual(
        #sp_literal{value = [], binary_value = <<"[]">>, meta = #{name => {type, my_nil, 0}}},
        MyNilType
    ),
    ?assertEqual({ok, []}, spectra:encode(json, ?MODULE, {type, my_nil, 0}, [], [pre_encoded])),
    ?assertEqual({ok, []}, spectra:decode(json, ?MODULE, {type, my_nil, 0}, [], [pre_decoded])),

    %% dynamic
    MyDynamicType = spectra_type_info:get_type(TypeInfo, my_dynamic, 0),
    ?assertEqual(
        #sp_simple_type{type = term, meta = #{name => {type, my_dynamic, 0}}}, MyDynamicType
    ),
    ?assertEqual({ok, 42}, spectra:encode(json, ?MODULE, {type, my_dynamic, 0}, 42, [pre_encoded])),
    ?assertEqual({ok, 42}, spectra:decode(json, ?MODULE, {type, my_dynamic, 0}, 42, [pre_decoded])),

    %% nonempty_binary
    MyNonemptyBinaryType =
        spectra_type_info:get_type(TypeInfo, my_nonempty_binary, 0),
    ?assertEqual(
        #sp_simple_type{type = nonempty_binary, meta = #{name => {type, my_nonempty_binary, 0}}},
        MyNonemptyBinaryType
    ),
    ?assertEqual(
        {ok, <<"hello">>},
        spectra:encode(json, ?MODULE, {type, my_nonempty_binary, 0}, <<"hello">>, [pre_encoded])
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra:encode(json, ?MODULE, {type, my_nonempty_binary, 0}, <<>>, [pre_encoded])
    ),
    ?assertEqual(
        {ok, <<"hello">>},
        spectra:decode(json, ?MODULE, {type, my_nonempty_binary, 0}, <<"hello">>, [pre_decoded])
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra:decode(json, ?MODULE, {type, my_nonempty_binary, 0}, <<>>, [pre_decoded])
    ),

    %% bitstring
    MyBitstringType = spectra_type_info:get_type(TypeInfo, my_bitstring, 0),
    ?assertEqual(
        #sp_simple_type{type = bitstring, meta = #{name => {type, my_bitstring, 0}}},
        MyBitstringType
    ),
    ?assertError(
        {type_not_supported, _},
        spectra:encode(json, ?MODULE, {type, my_bitstring, 0}, <<1, 2, 3>>, [pre_encoded])
    ),
    ?assertError(
        {type_not_supported, _},
        spectra:decode(json, ?MODULE, {type, my_bitstring, 0}, <<1, 2, 3>>, [pre_decoded])
    ),

    %% nonempty_bitstring
    MyNonemptyBitstringType =
        spectra_type_info:get_type(TypeInfo, my_nonempty_bitstring, 0),
    ?assertEqual(
        #sp_simple_type{
            type = nonempty_bitstring, meta = #{name => {type, my_nonempty_bitstring, 0}}
        },
        MyNonemptyBitstringType
    ),
    ?assertError(
        {type_not_supported, _},
        spectra:encode(json, ?MODULE, {type, my_nonempty_bitstring, 0}, <<1, 2, 3>>, [pre_encoded])
    ),
    ?assertError(
        {type_not_supported, _},
        spectra:decode(json, ?MODULE, {type, my_nonempty_bitstring, 0}, <<1, 2, 3>>, [pre_decoded])
    ),

    %% no_return
    MyNoReturnType = spectra_type_info:get_type(TypeInfo, my_no_return, 0),
    ?assertEqual(
        #sp_simple_type{type = none, meta = #{name => {type, my_no_return, 0}}},
        MyNoReturnType
    ),
    ?assertError(
        {type_not_supported, _},
        spectra:encode(json, ?MODULE, {type, my_no_return, 0}, a, [pre_encoded])
    ),
    ?assertError(
        {type_not_supported, _},
        spectra:decode(json, ?MODULE, {type, my_no_return, 0}, <<"not_a_no_return">>, [pre_decoded])
    ),

    %% none
    MyNoneType = spectra_type_info:get_type(TypeInfo, my_none, 0),
    ?assertEqual(
        #sp_simple_type{type = none, meta = #{name => {type, my_none, 0}}},
        MyNoneType
    ),
    ?assertError(
        {type_not_supported, _},
        spectra:encode(json, ?MODULE, {type, my_none, 0}, a, [pre_encoded])
    ),
    ?assertError(
        {type_not_supported, _},
        spectra:decode(json, ?MODULE, {type, my_none, 0}, <<"not_a_none">>, [pre_decoded])
    ).

-module(prop_schema_consistency).

-include_lib("proper/include/proper.hrl").

%% Property-based test to verify that spectra:schema/3 returns the same result
%% regardless of how the type is referenced (atom, 3-tuple, or sp_type()).
%%
%% This test checks that when a type has a -spectra() attribute with metadata
%% (title, description, examples), all three ways of calling schema/3 return
%% exactly the same schema data.

%% Test module with types that have -spectra attributes
-compile([nowarn_unused_type, nowarn_unused_record]).

-spectra(#{
    title => <<"Simple Integer">>,
    description => <<"A simple integer type">>,
    examples => [1, 2, 3]
}).
-type simple_int() :: integer().

-spectra(#{
    title => <<"Positive Number">>,
    description => <<"A positive integer">>,
    examples => [1, 42, 100]
}).
-type pos_int() :: pos_integer().

-spectra(#{
    title => <<"Status">>,
    description => <<"User status enum">>,
    examples => [active, pending]
}).
-type status() :: active | pending | inactive.

-spectra(#{
    title => <<"Age Range">>,
    description => <<"Valid age range">>,
    examples => [0, 18, 65, 120]
}).
-type age() :: 0..120.

-spectra(#{
    title => <<"Username">>,
    description => <<"Non-empty username string">>
}).
-type username() :: binary().

-spectra(#{
    title => <<"User ID List">>,
    description => <<"List of user IDs">>,
    examples => [[1, 2, 3], [42]]
}).
-type user_ids() :: [pos_integer()].

-spectra(#{
    title => <<"Simple Record">>,
    description => <<"A simple record with metadata">>,
    examples => [
        {simple_rec, 1, <<"Alice">>},
        {simple_rec, 42, <<"Bob">>}
    ]
}).
-record(simple_rec, {
    id :: pos_integer(),
    name :: binary()
}).

%% Types to test
types_with_spectra() ->
    oneof([
        {simple_int, 0},
        {pos_int, 0},
        {status, 0},
        {age, 0},
        {username, 0},
        {user_ids, 0}
    ]).

records_with_spectra() ->
    oneof([
        simple_rec
    ]).

prop_schema_consistency_for_types() ->
    TypeInfo = spectra_module_types:get(?MODULE),
    ?FORALL(
        %% FIXME: instead of these silly types we should generate random types
        %% with -spectra attributes in sp_type_generator
        {TypeName, TypeArity},
        types_with_spectra(),
        begin
            Type = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
            SchemaFromType = call_schema_safe(?MODULE, Type),
            SchemaFromAtom = call_schema_safe(?MODULE, TypeName),
            SchemaFromTuple = call_schema_safe(?MODULE, {type, TypeName, TypeArity}),

            % All should return the same result with full documentation
            ?WHENFAIL(
                begin
                    io:format("~nInconsistency detected for type: ~p/~p~n", [TypeName, TypeArity]),
                    io:format("Schema from sp_type: ~p~n", [SchemaFromType]),
                    io:format("Schema from atom:  ~p~n", [SchemaFromAtom]),
                    io:format("Schema from tuple: ~p~n", [SchemaFromTuple])
                end,
                SchemaFromAtom =:= SchemaFromTuple andalso SchemaFromAtom =:= SchemaFromType
            )
        end
    ).

prop_schema_consistency_for_records() ->
    ?FORALL(
        RecordName,
        records_with_spectra(),
        begin
            % Call schema/3 in two ways that preserve record name information
            SchemaFromAtom = call_schema_safe(?MODULE, RecordName),
            SchemaFromTuple = call_schema_safe(?MODULE, {record, RecordName}),

            % Both should return the same result with full documentation
            ?WHENFAIL(
                begin
                    io:format("~nInconsistency detected for record: ~p~n", [RecordName]),
                    io:format("Schema from atom:  ~p~n", [SchemaFromAtom]),
                    io:format("Schema from tuple: ~p~n", [SchemaFromTuple])
                end,
                SchemaFromAtom =:= SchemaFromTuple
            )
        end
    ).

%% Helper function to safely call schema and decode the result
call_schema_safe(Module, TypeOrRef) ->
    try
        SchemaJson = spectra:schema(json_schema, Module, TypeOrRef),
        json:decode(iolist_to_binary(SchemaJson))
    catch
        Class:Reason:Stack ->
            {error, {Class, Reason, Stack}}
    end.

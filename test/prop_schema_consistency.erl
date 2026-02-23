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

%% Type that references the record - should get the record's metadata
-type simple_rec_ref() :: #simple_rec{}.

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
    TypeInfo = spectra_module_types:get(?MODULE),
    ?FORALL(
        RecordName,
        records_with_spectra(),
        begin
            %% Get the actual #sp_rec{} type
            {ok, SpRec} = spectra_type_info:find_record(TypeInfo, RecordName),

            %% Call schema/3 in five different ways:
            %% 1. Via record atom
            SchemaFromAtom = call_schema_safe(?MODULE, RecordName),
            %% 2. Via {record, Name} tuple
            SchemaFromTuple = call_schema_safe(?MODULE, {record, RecordName}),
            %% 3. Via direct sp_type (the #sp_rec{} record)
            SchemaFromSpRec = call_schema_safe(?MODULE, SpRec),
            %% 4. Via a type that is a record reference ({type, TypeName, 0})
            %%    For simple_rec, we have simple_rec_ref type
            SchemaFromRecRefType =
                case RecordName of
                    simple_rec ->
                        call_schema_safe(?MODULE, {type, simple_rec_ref, 0});
                    _ ->
                        %% For other records, skip this check
                        SchemaFromAtom
                end,
            %% 5. Via direct #sp_rec_ref{} sp_type
            SchemaFromSpRecRef =
                case RecordName of
                    simple_rec ->
                        SpRecRefType = spectra_type_info:get_type(TypeInfo, simple_rec_ref, 0),
                        call_schema_safe(?MODULE, SpRecRefType);
                    _ ->
                        %% For other records, skip this check
                        SchemaFromAtom
                end,

            %% All should return the same result with full documentation
            ?WHENFAIL(
                begin
                    io:format("~nInconsistency detected for record: ~p~n", [RecordName]),
                    io:format("Schema from atom:          ~p~n", [SchemaFromAtom]),
                    io:format("Schema from tuple:         ~p~n", [SchemaFromTuple]),
                    io:format("Schema from sp_rec:        ~p~n", [SchemaFromSpRec]),
                    io:format("Schema from rec_ref type:  ~p~n", [SchemaFromRecRefType]),
                    io:format("Schema from sp_rec_ref:    ~p~n", [SchemaFromSpRecRef])
                end,
                SchemaFromAtom =:= SchemaFromTuple andalso
                    SchemaFromAtom =:= SchemaFromSpRec andalso
                    SchemaFromAtom =:= SchemaFromRecRefType andalso
                    SchemaFromAtom =:= SchemaFromSpRecRef
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

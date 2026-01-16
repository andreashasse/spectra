-module(nonempty_list_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-type item() :: integer().
-type nonempty_items() :: [item(), ...].
-type user_with_items() :: #{name := string(), items := nonempty_items()}.

%% Test function to validate nonempty_list
validate_nonempty_list_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    {ok, NonemptyItemsType} = spectra_type_info:find_type(TypeInfo, nonempty_items, 0),

    % Test JSON conversion using to_json
    ValidUser = #{name => "John", items => [1, 2, 3]},
    InvalidUser = #{name => "John", items => []},

    ?assertEqual({ok, #{<<"name">> => <<"John">>, <<"items">> => [1, 2, 3]}}, to_json(ValidUser)),

    {error, Errors} = to_json(InvalidUser),
    ?assertEqual(
        [
            sp_error:append_location(
                sp_error:type_mismatch(NonemptyItemsType, []),
                items
            )
        ],
        Errors
    ),

    % Test JSON conversion using from_json
    ValidJson = #{<<"name">> => <<"Jane">>, <<"items">> => [4, 5, 6]},
    InvalidJson = #{<<"name">> => <<"Jane">>, <<"items">> => []},

    {ok, User} = from_json(ValidJson),
    ?assertEqual(#{name => "Jane", items => [4, 5, 6]}, User),

    {error, FromErrors} = from_json(InvalidJson),
    ?assertEqual(
        [
            sp_error:append_location(
                sp_error:type_mismatch(NonemptyItemsType, []),
                items
            )
        ],
        FromErrors
    ).

-spec to_json(user_with_items()) ->
    {ok, json:encode_value()} | {error, [spectra:error()]}.
to_json(User) ->
    spectra_json:to_json(?MODULE, {type, user_with_items, 0}, User).

-spec from_json(json:encode_value()) ->
    {ok, user_with_items()} | {error, [spectra:error()]}.
from_json(Json) ->
    spectra_json:from_json(?MODULE, {type, user_with_items, 0}, Json).

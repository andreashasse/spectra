-module(sp_error).

-export([
    type_mismatch/2,
    type_mismatch/3,
    missing_data/3,
    not_matched_fields/2,
    no_match/2,
    no_match/3,
    append_location/2
]).

-include("../include/spectra.hrl").

-spec type_mismatch(
    spectra:sp_type_or_ref() | spectra:map_field() | spectra:record_field(),
    term()
) -> #sp_error{}.
type_mismatch(Type, Value) ->
    type_mismatch(Type, Value, #{}).

-spec type_mismatch(
    spectra:sp_type_or_ref() | spectra:map_field() | spectra:record_field(),
    term(),
    map()
) -> #sp_error{}.
type_mismatch(Type, Value, Ctx) ->
    #sp_error{
        type = type_mismatch,
        location = [],
        ctx = Ctx#{type => Type, value => Value}
    }.

-spec missing_data(
    spectra:sp_type_or_ref() | spectra:map_field() | spectra:record_field(),
    term(),
    [string() | atom()]
) -> #sp_error{}.
missing_data(Type, Value, Location) ->
    #sp_error{
        type = missing_data,
        location = Location,
        ctx = #{type => Type, value => Value}
    }.

-spec not_matched_fields(
    spectra:sp_type_or_ref() | spectra:map_field() | spectra:record_field(),
    term()
) -> #sp_error{}.
not_matched_fields(Type, Value) ->
    #sp_error{
        type = not_matched_fields,
        location = [],
        ctx = #{type => Type, value => Value}
    }.

-spec no_match(
    spectra:sp_type_or_ref() | spectra:map_field() | spectra:record_field(),
    term()
) -> #sp_error{}.
no_match(Type, Value) ->
    no_match(Type, Value, []).

-spec no_match(
    spectra:sp_type_or_ref() | spectra:map_field() | spectra:record_field(),
    term(),
    [{spectra:sp_type(), #sp_error{}}]
) -> #sp_error{}.
no_match(Type, Value, Errors) ->
    #sp_error{
        type = no_match,
        location = [],
        ctx = #{type => Type, value => Value, errors => Errors}
    }.

-spec append_location(#sp_error{}, string() | atom()) -> #sp_error{}.
append_location(Err, FieldName) ->
    Err#sp_error{location = [FieldName | Err#sp_error.location]}.

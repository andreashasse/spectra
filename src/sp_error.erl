-module(sp_error).

-moduledoc """
Constructor functions for structured validation errors.

Each function builds an `#sp_error{}` record describing why a value failed
to match an expected type. Errors carry a `location` path (built up by
`append_location/2` as the traversal unwinds) and a `ctx` map with the
offending type and value.
""".

-export([
    type_mismatch/2,
    type_mismatch/3,
    missing_data/3,
    not_matched_fields/2,
    no_match/3,
    append_location/2
]).

-include("../include/spectra.hrl").

-doc "Creates a type-mismatch error with no extra context.".
-spec type_mismatch(
    spectra:sp_type_or_ref() | spectra:map_field() | spectra:record_field(),
    dynamic()
) -> #sp_error{}.
type_mismatch(Type, Value) ->
    type_mismatch(Type, Value, #{}).

-doc "Creates a type-mismatch error, merging `Ctx` into the context map.".
-spec type_mismatch(
    spectra:sp_type_or_ref() | spectra:map_field() | spectra:record_field(),
    dynamic(),
    map()
) -> #sp_error{}.
type_mismatch(Type, Value, Ctx) ->
    #sp_error{
        type = type_mismatch,
        location = [],
        ctx = Ctx#{type => Type, value => Value}
    }.

-doc "Creates an error for a required field that was absent in the input.".
-spec missing_data(
    spectra:sp_type_or_ref() | spectra:map_field() | spectra:record_field(),
    dynamic(),
    [string() | atom()]
) -> #sp_error{}.
missing_data(Type, Value, Location) ->
    #sp_error{
        type = missing_data,
        location = Location,
        ctx = #{type => Type, value => Value}
    }.

-doc "Creates an error when an exact typed map field had no matching keys in the data.".
-spec not_matched_fields(
    spectra:sp_type_or_ref() | spectra:map_field() | spectra:record_field(),
    dynamic()
) -> #sp_error{}.
not_matched_fields(Type, Value) ->
    #sp_error{
        type = not_matched_fields,
        location = [],
        ctx = #{type => Type, value => Value}
    }.

-doc "Creates an error when no branch of a union type matched, bundling the per-branch errors.".
-spec no_match(
    spectra:sp_type_or_ref() | spectra:map_field() | spectra:record_field(),
    dynamic(),
    [{spectra:sp_type(), [#sp_error{}]}]
) -> #sp_error{}.
no_match(Type, Value, Errors) ->
    #sp_error{
        type = no_match,
        location = [],
        ctx = #{type => Type, value => Value, errors => Errors}
    }.

-doc """
Prepends `FieldName` to the error's location path.

Called as the traversal unwinds so the final location reads
outermost-to-innermost (e.g. `[user, address, street]`).
""".
-spec append_location(#sp_error{}, string() | atom()) -> #sp_error{}.
append_location(Err, FieldName) ->
    Err#sp_error{location = [FieldName | Err#sp_error.location]}.

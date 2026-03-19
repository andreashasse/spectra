# Feature: Type Parameters for Codec Types

## Background

Spectra supports "codec types" — Erlang types backed by a module implementing the
`spectra_codec` behaviour. Codec modules receive `encode/4`, `decode/4`, and
optionally `schema/3` callbacks, letting them fully control serialisation,
deserialisation, and schema generation for a named type.

Currently the fourth argument to every codec callback is always an empty map `#{}`.
This makes it impossible to reuse a single codec implementation across types that
differ only in a configuration value. A concrete example is a regex-validated string
type: the codec logic is identical, but each usage site wants a different pattern.

The fourth argument will be repurposed to carry the `type_parameters` value directly
(not wrapped in a map), so codecs can pattern-match on it cleanly.

The `-spectra(...)` module attribute already lets developers attach documentation
metadata to types. This feature extends that mechanism with a new key,
`type_parameters`, whose value is passed through to codec callbacks so that each
type definition can carry its own codec configuration.

### Example

```erlang
%% Codec module: regex_string_codec
-behaviour(spectra_codec).

decode(json, _TypeRef, Data, Pattern) when is_binary(Data), is_binary(Pattern) ->
    case re:run(Data, Pattern) of
        {match, _} -> {ok, Data};
        nomatch     -> {error, [#sp_error{...}]}
    end;
decode(json, _TypeRef, Data, undefined) when is_binary(Data) ->
    {ok, Data}.
```

```erlang
%% Consuming module
-spectra(#{type_parameters => <<"^[a-z]+$">>}).
-type lowercase() :: binary().

-spectra(#{type_parameters => <<"^[0-9]+$">>}).
-type digits() :: binary().
```

When `spectra:decode(json, my_module, lowercase, <<"hello">>)` is called, the codec
receives `<<"^[a-z]+$">>` as the fourth argument. When the same type is referenced
from another module (e.g. `my_module:lowercase()`), the parameters still come from
the defining module `my_module`.

---

## Design Decisions

- **Parameters belong to the type definition, not the usage site.** If `A:phone_number()`
  is referenced from module `B`, the parameters are read from module `A`'s type info.

- **`type_parameters` value is `term()`** — the codec interprets it however it wants.
  No shape is enforced by the library.

- **Stored in `sp_type_meta()`** as the key `parameters`. This is the canonical
  location; it travels with the resolved `sp_type()` at every dispatch point.

- **Passed directly as the fourth argument** to codec callbacks. When `type_parameters`
  is set, the value is passed as-is. When not set, `undefined` is passed. Codecs
  pattern-match on the value directly.

---

## Implementation Plan

### 1. `src/spectra.erl` — extend `sp_type_meta()` type

Add `parameters => term()` to the `sp_type_meta()` type definition.

### 2. `src/spectra_type.erl` — handle `type_parameters` in attribute pipeline

The `-spectra(...)` map is currently passed wholesale to `normalize_doc/1` via
`add_doc_to_type/2`, and `add_doc_field/3` crashes on any unknown key. Two changes:

- In `add_doc_to_type/2` (or its caller `attach_doc/2` in `spectra_abstract_code`):
  extract `type_parameters` from the raw DocMap *before* passing the remainder to
  `normalize_doc/1`.
- Set `meta#{parameters => Value}` directly on the type after `add_doc_to_type/2`
  returns.

This keeps the `type_doc()` path clean and `add_doc_field/3` unchanged.

### 3. `src/spectra_abstract_code.erl` — `attach_doc/2`

Modify `attach_doc/2` to strip `type_parameters` from the DocMap before calling
`spectra_type:add_doc_to_type/2`, and apply it to the type's meta separately.
(Alternatively this logic can live in `add_doc_to_type/2` itself — decide at
implementation time based on where it reads most cleanly.)

### 4. `src/spectra.erl` — `maybe_codec_schema/2`

`SpType` is currently resolved *after* the codec call (in the `continue` branch only).
Move `resolve_type_ref/2` to before the codec dispatch so parameters can be read from
meta and included in opts.

### 5. `src/spectra.erl` — `maybe_codec_decode/6` and `maybe_codec_encode/6`

`SpType` is already a parameter. Read `meta.parameters` and pass it directly as the
fourth argument to the codec:

```erlang
Params = maps:get(parameters, spectra_type:get_meta(SpType), undefined),
M:decode(Format, TypeRef, Data, Params)
```

### 6. `src/spectra_json.erl` — 6 call sites

For each of the three ref-type branches in `to_json/3` and `do_from_json/3`:

- **`#sp_user_type_ref{}`**: resolve the type via `get_type(TypeInfo, N, Arity)` before
  the codec call; read `meta.parameters` (defaulting to `undefined`) for the fourth arg.
- **`#sp_remote_type{}`**: `RemoteTypeInfo` is already fetched eagerly at the top of
  the clause; call `get_type(RemoteTypeInfo, TypeName, TypeArity)` before the codec call.
- **`#sp_rec_ref{}`**: call `get_record(TypeInfo, RecordName)` before the codec call.

### 7. `src/spectra_json_schema.erl` — 3 call sites

Same pattern as `spectra_json.erl` for all three ref types in `do_to_schema/2`.

### 8. `src/spectra_string.erl` — 6 call sites

Same pattern. For `#sp_remote_type{}` branches, `RemoteTypeInfo` is currently
fetched lazily (only inside `continue`/`error` sub-branches). Move the fetch to
before the codec dispatch to make parameters available.

### 9. `src/spectra_binary_string.erl` — 6 call sites

Same pattern. The `resolve_remote_type/3` helper already returns the resolved type;
read parameters from the returned `SpType` directly before the codec call.

---

## Files Changed

| File | Nature of change |
|---|---|
| `src/spectra.erl` | Extend `sp_type_meta()` type; update 3 `maybe_codec_*` functions |
| `src/spectra_type.erl` | Strip `type_parameters` before `normalize_doc` in `add_doc_to_type/2` |
| `src/spectra_abstract_code.erl` | Possibly split DocMap handling in `attach_doc/2` |
| `src/spectra_json.erl` | Resolve type before codec call at 6 sites |
| `src/spectra_json_schema.erl` | Resolve type before codec call at 3 sites |
| `src/spectra_string.erl` | Resolve type before codec call at 6 sites; make remote fetch eager |
| `src/spectra_binary_string.erl` | Resolve type before codec call at 6 sites |
| `test/` | New test module (see acceptance criteria) |

---

## Acceptance Criteria

### AC1 — Parameters flow into decode
Given a type with `-spectra(#{type_parameters => Value})` and a codec module, calling
`spectra:decode/4` invokes `M:decode(Format, TypeRef, Data, Value)`.

### AC2 — Parameters flow into encode
Same as AC1 but for `spectra:encode/4` → `M:encode(Format, TypeRef, Data, Value)`.

### AC3 — Parameters flow into schema
Same as AC1 but for `spectra:schema/3` → `M:schema(Format, TypeRef, Value)`.

### AC4 — No parameters → `undefined`
A type without `type_parameters` passes `undefined` as the fourth argument to codec
callbacks.

### AC5 — Remote type uses defining module's parameters
When module `B` has a field of type `A:phone_number()`, and `A` defines `phone_number`
with `type_parameters`, the codec is called with `A`'s parameters regardless of
whether the call originates in module `B` or `A`.

### AC6 — Parameters work across all formats
AC1–AC5 hold for all supported formats: `json`, `string`, `binary_string`.

### AC7 — Unknown keys in `-spectra(...)` still crash
A `-spectra(#{unknown_key => value})` on a type still raises
`{invalid_spectra_field, unknown_key, value}`.

### AC8 — `type_parameters` on a function spec crashes
`type_parameters` is only valid on type/record definitions. Using it on a `-spec`
raises `{invalid_spectra_field, type_parameters, Value}` (the function doc path is
unchanged).

### AC9 — `make build-test` passes
All existing tests continue to pass and the new tests pass.

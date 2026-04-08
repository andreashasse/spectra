# Feature: Field Filtering with `only`

## Background

Spectra encodes and decodes all fields declared in a map type by default. Sometimes you need to control which fields are exposed externally — for example, to avoid serialising sensitive fields like `password_hash`, or to produce a slimmer JSON representation without changing the internal type.

Elixir's JSON library Jason supports this via `@derive {Jason.Encoder, only: [:field1, :field2]}`. This feature adds an equivalent `only` key to the `-spectra()` attribute.

---

## Design Decisions

### Filtering happens at type extraction time

The `only` filter is applied inside `attach_doc/2` in `spectra_abstract_code.erl` when the BEAM abstract code is read. The resulting `#sp_map{fields}` list only contains the allowed fields. All downstream code — encoding, decoding, schema generation — then operates on the already-filtered field list, so no changes are required in `spectra_json.erl`, `spectra_json_schema.erl`, or any other format module.

### Union members are filtered; type refs are not

`apply_only/2` propagates the filter through `#sp_union{}` members, so `MyStruct | nil` works correctly: the map member is filtered and the `nil` literal is left untouched. However, `#sp_user_type_ref{}` and `#sp_remote_type{}` nodes are passed through unchanged. These resolve at encode/decode time by looking up types from a different module's `TypeInfo`, and filtering them at extraction time would require cross-module type resolution with ordering constraints. In practice, Elixir struct types are always defined as direct map literals, not via remote or user type references.

### `only` is removed from the doc metadata

Like `type_parameters`, the `only` key is stripped from the doc map before it is forwarded to `spectra_type:add_doc_to_type/2`. It is a structural directive, not documentation metadata — it must not appear in generated schemas or OpenAPI specs.

### `typed_map_field` records are excluded naturally

The list comprehension `[F || #literal_map_field{name = N} = F <- Fields, lists:member(N, Only)]` pattern-matches on `#literal_map_field{}`, so `#typed_map_field{}` records (dynamic key types like `atom() => integer()`) are dropped automatically when `only` is specified.

### Decoded maps may not conform to the Erlang type

For Elixir structs, struct defaults fill excluded fields (via `StructName:'__struct__'()`), so the returned map is a valid struct. For plain map types, excluded fields are simply absent from the decoded result. In both cases, the result may not satisfy the declared type — this is intentional and documented.


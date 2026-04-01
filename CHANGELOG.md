# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.9.3] - 2026-04-01

### Added
- **`spectra_type:update_meta/2`**: New helper that updates a type's metadata map in one call, reducing the boilerplate of paired `get_meta`/`set_meta` calls.
- **`make lint`**: New lint target using `elp lint --rebar --read-config` (also added to CI).

### Changed
- `#sp_user_type_ref{}` and `#sp_remote_type{}` internal records now cache `arity` directly, eliminating repeated `length/1` calls across all serialization and deserialization modules.

## [0.9.2] - 2026-03-27

### Added
- **`spectra_calendar_codec`**: Built-in codec for `calendar:datetime()` and `calendar:date()`. Serialises to/from ISO 8601 strings (`"YYYY-MM-DDTHH:MM:SS"` and `"YYYY-MM-DD"`). Opt-in via the application environment, same pattern as `spectra_dict_codec`.

## [0.9.1] - 2026-03-26

### Fixed
- String/binary constraints (`min_length`, `max_length`, `pattern`) are now correctly enforced when the type body is a remote type alias that resolves to a string/binary type (e.g. Elixir's `String.t()`). Previously the constraints were silently ignored.

## [0.9.0] - 2026-03-24

### Added
- **`spectra_dict_codec`**: Built-in codec for encoding and decoding `dict:dict()` values. Register it via the app env or `-behaviour(spectra_codec)` like any other codec. Mostly to show that codecs can be implemented for types with arity > 0.

### Changed
- **Breaking**: `spectra_codec` callbacks now receive an additional `SpType :: spectra:sp_type()` argument. `encode` and `decode` are now arity 6; `schema` is now arity 5. Existing codec modules must add this argument to all callback clauses. Use `spectra_type:type_args/1` on `SpType` to access concrete type-variable bindings at the call site.

## [0.8.2] - 2026-03-21

### Changed
- README: added reference tables documenting all valid `-spectra()` attribute keys for types/records and function specs, including the `title` vs `summary` distinction.

## [0.8.1] - 2026-03-21

### Added
- **Exported OpenAPI types**: `spectra_openapi` now exports `endpoint_spec/0`, `endpoint_doc/0`, `response_spec/0`, `parameter_spec/0`, `parameter_input_spec/0`, `http_method/0`, `http_status_code/0`, and `openapi_metadata/0`.
- **`parameter_input_spec/0`**: New type for the map passed to `with_parameter/3`. Distinct from the internal `parameter_spec/0` (which includes `module`) — the function merges `module` in automatically.

### Changed
- README overhauled: simpler introductory example, cleaner API reference, restructured Custom Codecs section, added `binary_string`/`string` format example.

## [0.8.0] - 2026-03-19

### Added
- **Custom codecs**: New `spectra_codec` behaviour with `encode/4`, `decode/4`, and optional `schema/4` callbacks. Register codecs via the application environment (`{spectra, [{codecs, #{...}}]}`) or by declaring `-behaviour(spectra_codec)` on the type's own module.
- **Type parameters**: Types can now carry a `type_parameters` field in their `-spectra()` attribute. The value is passed as the 4th argument to codec callbacks, allowing a single codec module to handle multiple parameterised variants.
- **Auto-populate `description` and `deprecated` from type annotations**: Parameters, request bodies, and response headers that reference a type with a `-spectra()` doc annotation now automatically inherit `description` and `deprecated`. Explicit values on the spec still take precedence.
- **Plain atom type refs in `spectra_openapi`**: `spectra_openapi` functions now accept plain atoms as type references (e.g. `user` instead of `{type, user, 0}`), matching the behaviour of `spectra.erl`.

### Changed
- **Breaking**: `with_request_body/4` fourth argument changed from an opts map (`#{content_type => ..., description => ...}`) to a plain `content_type` binary. Pass `description` via the type's `-spectra()` annotation instead.

### Fixed
- **`type_doc/2` now follows type references**: The internal `type_doc/2` function in `spectra_openapi` previously returned an empty description whenever the resolved type was an `#sp_user_type_ref{}` or `#sp_remote_type{}`. It now follows the reference to the underlying type to retrieve its description. Local annotations on the alias take precedence — the reference is only followed when the alias itself carries no `-spectra` doc.

## [0.7.0] - 2026-03-04

### Added
- **`description` and `deprecated` fields for OpenAPI parameters**: `parameter_spec()` now accepts `description => binary()` and `deprecated => boolean()` fields, propagated into the generated OpenAPI output.
- **`description` for request body specs**: `request_body_spec()` now accepts a `description` field.
- **`deprecated` for response header specs**: `header_spec()` now accepts a `deprecated => boolean()` field.
- **Extended `openapi_metadata()`**: Supports additional `info` fields and a top-level `servers` list for multi-server OpenAPI documents.

### Changed
- **Breaking**: `with_request_body/3,4` now takes the schema as the third positional argument. Optional metadata (`content_type`, `description`) is passed as a fourth `Opts` map. Update calls from `with_request_body(E, Method, #{schema => S})` to `with_request_body(E, Method, S)`.

## [0.6.0] - 2026-03-04

### Added
- **`-spectra` attribute for function specs**: You can now annotate `-spec` declarations with a `-spectra()` attribute to attach metadata to functions. The `function_doc()` type supports `summary`, `description`, and `deprecated` fields (distinct from the `type_doc()` fields used for types and records).

### Fixed
- `#sp_union{}` `types` field was declared with a default value instead of a type annotation, which could cause subtle runtime issues.

## [0.5.1] - 2026-03-02

### Added
- **`pre_decoded` / `pre_encoded` options** for `decode/5` and `encode/5`: pass `[pre_decoded]` to skip JSON parsing when your input is already a decoded term, or `[pre_encoded]` to get back a `json:encode_value()` term instead of `iodata()` from `encode/5`.
- **`spectra_openapi:endpoints_to_openapi/3`**: new overload that accepts encode options (e.g. `[pre_encoded]`) for the generated OpenAPI document.

### Changed
- `__spectra_type_info__/0` calls are now cached in `persistent_term` alongside abstract-code lookups, reducing repeated reflection overhead.

## [0.5.0] - 2026-02-26

### Added
- **JSON schema documentation**: Types can now carry `title` and `description` metadata via the `spectra` attribute, which is propagated into generated JSON Schema and OpenAPI output
- **`__spectra_type_info__/0` protocol**: Modules can now expose its `type_info` by exporting the `__spectra_type_info__/0` function. This is an implementation detail in the library. It will be used to pair documentation with types in Elixir, and can later be used for performance and to handle hot code reloading better.

## [0.4.0] - 2026-01-27

### Changed
- **Breaking**: `spectra:schema/3` now returns schema values directly instead of `{ok, Schema}` or `{error, Reason}` tuples.

## [0.3.2] - 2026-01-25

### Changed
- Improved documentation and clarified OTP 27 requirement

## [0.3.0] - 2026-01-20

### Changed
- Upgraded JSON Schema from draft-07 to 2020-12 specification with proper `$schema` field support
- Upgraded OpenAPI spec generation from 3.0 to 3.1 (which natively uses JSON Schema 2020-12)
- Improved remote type handling in enums and parameterized types
- Simplified error handling implementation in preparation for better error messages
- Enhanced null/optional handling with clearer documentation and dedicated tests for undefined/nil behavior in mandatory vs optional map fields

### Internal
- Refactored type utilities into `spectra_util.erl` with renamed functions for consistency
- New tests for typed map fields, parameterized remote types, and enum remote types
- Property-based testing for JSON encoding/schema/decoding consistency (`test/prop_json_encode_schema_consistency.erl`)
- Python validators for JSON Schema 2020-12 and OpenAPI 3.1 standards compliance
- Development tooling improvements including updated `.tool-versions` and enhanced `Makefile` with release safeguards
- Major code simplification in `spectra_binary_string.erl`, `spectra_json.erl`, and `spectra_string.erl`
- Created `sp_error.erl` module for consolidated error handling

## [0.2.0] - 2025-12-14

### Changed
- **Breaking**: Extra fields in JSON objects are now ignored during deserialization instead of causing a `not_matched_fields` error. This affects map, struct (elixir) and record deserialization.

### Notes
- The old strict validation behavior has been commented out with a TODO to potentially add it back as a configuration option in the future

# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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

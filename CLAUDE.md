# Claude Code Instructions

## Project Overview
spectra - A data validation library for Erlang inspired by Pydantic

## Commands to run after each change
```bash
make format
make build-test
```
When `make build-test` succeeds, see if you can cleanup the code.
Once that is done, run `make proper`.

## Git Workflow
  - Never commit directly to the `main` branch — it is protected and pushes will be rejected. Always work on a feature or release branch.

## Common Tasks
  - Add test: Create new test file in test/ directory
  - Run specific test: rebar3 eunit --module=test_module_name

## Lint Rules
  - When a lint diagnostic includes a code (e.g. W0032) and a link, follow the link to understand the rule before attempting a fix. Do not guess at the meaning of the code.

## Development Guidelines
  - All changes should come with unit tests (positive and negative if applicable) that cover the change.
  - Don't define types in .hrl files. Types that the user of this library should use should be defined in spectra.erl
  - When using a type that is defined in the same file, you don't have to prefix it with the module name
  - When the property based tests uncover an error. Make a unit test that reproduces the error and fix the error.
  - Unit tests that tests the decoders, encoders and schemas should call spectra.erl, not eg spectra_json.erl
  - In tests, avoid hardcoding internal `#sp_map{}` / `#sp_rec{}` / `#sp_simple_type{}` etc. records directly. Instead, define the type in a dedicated Erlang helper module in `test/` and use `spectra_abstract_code:types_in_module(helper_module)` + `spectra_type_info:get_type(TypeInfo, type_name, 0)` to get the type. This keeps tests readable and decoupled from internal record formats.

## Library Architecture
  - The library uses Erlang types during runtime to support type-safe serialization and deserialization
  - Types are extracted in `spectra_abstract_code` into an internal format (`spectra:sp_type()`)
  - Different modules can use the extracted type data to:
    * Serialize data
    * Deserialize data
    * Generate schemas
  - Supports multiple formats including:
    * JSON
    * Open API Spec
    * DynamoDB's JSON flavor

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

## Common Tasks
  - Add test: Create new test file in test/ directory
  - Run specific test: rebar3 eunit --module=test_module_name

## Development Guidelines
  - Don't define types in .hrl files. Types that the user of this library should use should be defined in spectra.erl
  - When using a type that is defined in the same file, you don't have to prefix it with the module name
  - When the property based tests uncover an error. Make a unit test that reproduces the error and fix the error.

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

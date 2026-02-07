# Agents Guide for Spectra

This document provides guidance for AI coding agents working on the Spectra codebase.

## Project Overview

Spectra is a type-safe data validation library for Erlang inspired by Pydantic. It provides serialization/deserialization for Erlang records and types with a focus on JSON and OpenAPI documentation generation.

**Requirements**: Erlang/OTP 27+ (uses native `json` module)

### Library Architecture
- The library uses Erlang types during runtime to support type-safe serialization and deserialization
- Types are extracted in `spectra_abstract_code` into an internal format (`spectra:sp_type()`)
- Different modules can use the extracted type data to:
  * Serialize data
  * Deserialize data  
  * Generate schemas
- Supports multiple formats including:
  * JSON
  * OpenAPI Spec
  * DynamoDB's JSON flavor

## Build & Test Commands

### Standard Workflow
After each change, run:
```bash
make format        # Format code
make build-test    # Run all checks
```

When `make build-test` succeeds, review the code for cleanup opportunities.  
Once cleanup is done, run `make proper` to verify property-based tests.

### Building
```bash
rebar3 compile           # Compile the project
make compile             # Same via Makefile
```

### Testing
```bash
rebar3 eunit                         # Run all tests
rebar3 eunit --module=MODULE_NAME    # Run single test module (e.g., spectra_abstract_code_test)
make test                            # Run tests (includes Elixir compilation if available)
rebar3 proper                        # Run property-based tests
make proper                          # Same via Makefile
```

### Code Quality
```bash
rebar3 fmt                # Format code (auto-write)
rebar3 fmt --check        # Check formatting without writing
make format               # Format via Makefile
make format_verify        # Verify formatting

rebar3 dialyzer           # Run Dialyzer type checker
make dialyzer             # Same via Makefile

rebar3 xref               # Check cross-references
make xref                 # Same via Makefile

elp eqwalize-all          # Run eqWAlizer type checker
make type_check           # Same via Makefile

rebar3 hank               # Check for unused code
make hank                 # Same via Makefile

rebar3 check_app_calls    # Check application calls
make check_app_calls      # Same via Makefile
```

### Coverage & Documentation
```bash
rebar3 cover              # Generate coverage report
make cover                # Same via Makefile

rebar3 ex_doc             # Generate documentation
make doc                  # Same via Makefile
```

### Full Build
```bash
make build-test           # Run all checks: compile, xref, type_check, test, dialyzer, hank, check_app_calls, format_verify, cover
make all                  # Run format, build-test, doc
```

## Code Style Guidelines

### Module Structure
```erlang
-module(module_name).

-export([public_function/2]).
-ignore_xref([rarely_used_function/1]).  % For functions not called within codebase

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-type my_type() :: term().
-export_type([my_type/0]).

-spec public_function(Arg1, Arg2) -> Result when
    Arg1 :: type1(),
    Arg2 :: type2(),
    Result :: result_type().
```

### Imports and Includes
- Use `-include("../include/spectra.hrl")` for public API types
- Use `-include("../include/spectra_internal.hrl")` for internal record definitions
- Use `-include_lib("eunit/include/eunit.hrl")` for tests
- **Always use relative paths** for includes (e.g., `"../include/..."`)

### Type Specifications
- **All exported functions MUST have `-spec` declarations** (enforced by `warn_missing_spec`)
- Test modules use `nowarn_missing_spec` to relax this requirement
- Use specific types over general ones (e.g., `pos_integer()` vs `integer()`)
- Export types that are part of the public API using `-export_type/1`
- **Don't define types in .hrl files** - Types for library users should be defined in `spectra.erl`
- When using a type defined in the same file, don't prefix it with the module name

### Naming Conventions
- Module names: lowercase with underscores (e.g., `spectra_abstract_code`)
- Function names: lowercase with underscores (e.g., `types_in_module`)
- Type names: lowercase with underscores (e.g., `type_info`, `sp_type`)
- Record names: lowercase with underscores (e.g., `sp_simple_type`, `type_info`)
- Variables: CamelCase starting with uppercase (e.g., `TypeInfo`, `Module`)
- Private functions: lowercase with underscores, typically have helper suffix

### Record Definitions
Records are defined in `include/spectra_internal.hrl`:
```erlang
-record(sp_simple_type, {
    type :: spectra:simple_types()
}).
```

Access records using pattern matching or field access:
```erlang
#type_info{types = Types} = TypeInfo
TypeInfo#type_info.types
```

### Error Handling
- Use `erlang:error/1` for programmer errors (e.g., invalid arguments)
- Return `{ok, Result} | error` tuples for expected failures
- Use `#sp_error{}` record for validation errors with location information
- Helper functions in `sp_error.erl`: `type_mismatch/3`, `missing_data/2`, `not_matched_fields/2`

### Pattern Matching & Guards
```erlang
% Prefer pattern matching in function heads
function_name(#sp_simple_type{type = string} = Type) -> ...;
function_name(#sp_simple_type{type = integer} = Type) -> ...

% Use guards for type checks
when is_atom(Name) andalso is_integer(Arity) -> ...
when Error =:= non_existing orelse Error =:= preloaded -> ...
```

### Formatting
- Use `erlfmt` for automatic formatting
- 4-space indentation (handled by erlfmt)
- **Line length**: Let erlfmt decide (no manual line breaking needed)
- **Commas**: Trailing commas in lists/maps encouraged for better diffs

### Comments
- Avoid unnecessary comments - code should be self-documenting
- Use `%%` for line comments
- Remove TODO/FIXME comments after addressing them
- Minimal comments in test files - let test names describe behavior

### Test Guidelines
- Test module naming: `<module>_test.erl`
- Test function naming: `descriptive_name_test/0`
- Use EUnit macros: `?assert`, `?assertEqual`, `?assertMatch`, `?assertError`
- **Consolidate assertions**: Use single `?assertMatch` with nested patterns instead of multiple separate assertions
- Include type definitions in test modules for testing type extraction
- `code:ensure_loaded/1` is called automatically by `types_in_module/1` - no need in tests
- **Unit tests should call public API**: Test decoders, encoders and schemas via `spectra.erl`, not internal modules like `spectra_json.erl`
- **When property-based tests find bugs**: Create a unit test that reproduces the error, then fix it

### Type System Internals
- `sp_type()`: Internal representation of Erlang types
- `type_info` record: Container for module's types, records, functions, docs
- `__spectra__/0`: Modules can export this function returning `#type_info{}` to provide precomputed type info
- All modules must be compiled with `debug_info` for type extraction from abstract code

### Common Patterns
```erlang
% Getting type info from a module
TypeInfo = spectra_abstract_code:types_in_module(Module)

% Finding a type
{ok, Type} = spectra_type_info:find_type(TypeInfo, type_name, Arity)
Type = spectra_type_info:get_type(TypeInfo, type_name, Arity)  % Errors if not found

% Pattern matching map fields with := vs =>
#{required_field := Value} = Map      % Required field (must exist)
#{optional_field => Value} = Map      % Optional field (may exist)
```

### Warnings as Errors
The project treats warnings as errors:
- `warnings_as_errors` is enabled
- `warn_unused_import` catches unused imports
- `warn_missing_spec` requires specs on all exported functions (except tests)

### Dependencies
- Only use standard library in main code
- Test dependencies: `proper`, `jesse`, `eqwalizer_support`
- Avoid adding new dependencies without discussion

## Key Files
- `src/spectra.erl` - Main API module
- `src/spectra_abstract_code.erl` - Type extraction from abstract code
- `src/spectra_type_info.erl` - Type info data structure management
- `src/spectra_json.erl` - JSON encoding/decoding
- `src/spectra_json_schema.erl` - JSON Schema generation
- `src/spectra_openapi.erl` - OpenAPI specification generation
- `include/spectra_internal.hrl` - Internal record definitions

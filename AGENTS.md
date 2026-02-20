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
make compile             # Same via Makefile
```

### Testing
```bash
rebar3 eunit --module=MODULE_NAME    # Run single test module (e.g., spectra_abstract_code_test)
make test                            # Run tests (includes Elixir compilation if available)
make proper                          # Same via Makefile
```

### Full Build
```bash
make build-test           # Run all checks: compile, xref, type_check, test, dialyzer, hank, check_app_calls, format_verify, cover
make all                  # Run format, build-test, doc
```

## Code Style Guidelines

### Type Specifications
- **All exported functions MUST have `-spec` declarations** (enforced by `warn_missing_spec`)
- Test modules use `nowarn_missing_spec` to relax this requirement
- Use specific types over general ones (e.g., `pos_integer()` vs `integer()`)
- Export types that are part of the public API using `-export_type/1`
- **Don't define types in .hrl files** - Types for library users should be defined in `spectra.erl`
- When using a type defined in the same file, don't prefix it with the module name


### Error Handling
- **Crash on bad code, error on bad user input.** Never silently swallow unexpected inputs with defensive catch-all clauses that return defaults or empty results. If a programmer passes invalid data, let it crash with a clear match error. Only return `{error, Reason}` tuples for expected failures from user input (e.g., validation errors).
- Use `erlang:error/1` for programmer errors (e.g., invalid arguments)
- Return `{ok, Result} | {error, Reason}` for functions that can have expected failures (user input, validation, etc.)
- Use `#sp_error{}` record for validation errors with location information, and wrap it in an `{error, Reason}` tuple (e.g., `{error, #sp_error{...}}` or `{error, [#sp_error{}, ...]}`)
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

### Function Arity Pattern
When defining functions with multiple arities, the lower-arity function should call the higher-arity function with a default value. This applies to both the implementation and the documentation:
```erlang
% Good - arity-2 calls arity-3 with default
-doc """
Creates a basic endpoint specification.

Equivalent to calling endpoint/3 with an empty documentation map.
...
""".
-doc #{
    equiv => endpoint(Method, Path, #{}),
    params => #{...}
}.
-spec endpoint(Method :: http_method(), Path :: binary()) -> endpoint_spec().
endpoint(Method, Path) ->
    endpoint(Method, Path, #{}).

-doc """
Creates an endpoint specification with documentation.

This function creates the foundation for an endpoint with the specified HTTP method, path, and documentation.
...
""".
-spec endpoint(Method :: http_method(), Path :: binary(), Doc :: endpoint_doc()) ->
    endpoint_spec().
endpoint(Method, Path, Doc) ->
    #{method => Method, path => Path, doc => Doc}.

% Bad - duplicating implementation in both functions
-spec endpoint(Method :: http_method(), Path :: binary()) -> endpoint_spec().
endpoint(Method, Path) ->
    #{method => Method, path => Path, doc => #{}}.

-spec endpoint(Method :: http_method(), Path :: binary(), Doc :: endpoint_doc()) ->
    endpoint_spec().
endpoint(Method, Path, Doc) ->
    #{method => Method, path => Path, doc => Doc}.
```

### Comments
- Avoid unnecessary comments - code should be self-documenting
- Minimal comments in test files - let test names describe behavior

### Test Guidelines
- Use EUnit macros: `?assert`, `?assertEqual`, `?assertMatch`, `?assertError`
- **Consolidate assertions**: Use single `?assertMatch` with nested patterns instead of multiple separate assertions
- Include type definitions in test modules for testing type extraction
- **Unit tests should call public API**: Test decoders, encoders and schemas via `spectra.erl`, not internal modules like `spectra_json.erl`
- **When property-based tests find bugs**: Create a unit test that reproduces the error, then fix it

### Type System Internals
- `sp_type()`: Internal representation of Erlang types
- `type_info` record: Container for module's types, records, functions, docs
- `__spectra_type_info__/0`: Modules can export this function returning `#type_info{}` to provide precomputed type info
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

### Elixir Integration via Spectral

When working in Elixir, use the `Spectral.TypeInfo` module instead of directly pattern-matching on the `#type_info{}` record tuple or calling `:spectra_type_info` functions. This provides an idiomatic Elixir API and insulates Elixir code from internal record changes.

```elixir
# Good - use Spectral.TypeInfo
type_info = MyModule.__spectra_type_info__()
{:ok, type} = Spectral.TypeInfo.find_type(type_info, :t, 0)
{:ok, func_specs} = Spectral.TypeInfo.find_function(type_info, :my_func, 2)

# Bad - destructuring the Erlang record tuple directly
{:type_info, types, records, functions} = MyModule.__spectra_type_info__()
```

### Dependencies
- Only use standard library in main code
- Test dependencies: `proper`, `jesse`, `eqwalizer_support`
- Avoid adding new dependencies without discussion

## Key Files
- `src/spectra.erl` - Main API module
- `src/spectra_abstract_code.erl` - Type extraction from abstract code
- `src/spectra_module_types.erl` - Get type info for a module
- `src/spectra_type_info.erl` - Type info data structure management
- `src/spectra_json.erl` - JSON encoding/decoding
- `src/spectra_json_schema.erl` - JSON Schema generation
- `src/spectra_openapi.erl` - OpenAPI specification generation
- `include/spectra_internal.hrl` - Internal record definitions

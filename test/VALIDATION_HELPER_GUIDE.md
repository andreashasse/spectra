# JSON Schema Validation Helper Guide

This guide explains how to integrate the Python JSON Schema validator into your Erlang tests.

## Overview

The `json_schema_validator_helper` module allows you to validate generated schemas against the JSON Schema 2020-12 specification directly from your EUnit tests.

## Benefits

- **Automatic validation**: Every generated schema is checked for 2020-12 compliance
- **Early error detection**: Catch invalid schemas during test runs
- **CI/CD integration**: Fails tests if schemas don't conform to spec
- **Optional dependency**: Tests skip validation gracefully if `uv` is not installed

## Usage

### 1. Add the helper to your test module

```erlang
-module(my_schema_test).
-include_lib("eunit/include/eunit.hrl").

%% Add helper function at the top of your test module
validate_with_python(Schema) ->
    case json_schema_validator_helper:validate_schema_2020_12(Schema) of
        ok ->
            ok;
        {skip, Reason} ->
            io:format("Skipping Python validation: ~s~n", [Reason]),
            ok;
        {error, {validation_failed, Output}} ->
            io:format("Python validation failed:~n~s~n", [Output]),
            erlang:error({python_validation_failed, Output})
    end.
```

### 2. Validate schemas in your tests

```erlang
my_schema_test() ->
    %% Generate schema
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {type, user, 0}),

    %% Your existing assertions
    ?assertEqual(<<"object">>, maps:get(type, Schema)),

    %% Add validation
    validate_with_python(Schema).
```

### 3. Pattern: Extract, Assert, Validate

The recommended pattern is:

```erlang
my_test() ->
    %% 1. Extract schema
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {type, my_type, 0}),

    %% 2. Assert expected structure
    ?assertEqual(
        #{<<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
          type => <<"string">>},
        Schema
    ),

    %% 3. Validate with Python
    validate_with_python(Schema).
```

## Requirements

- **Optional**: `uv` installed on the system
- If `uv` is not found, validation is skipped with a warning
- No changes needed to existing test infrastructure

## Installation

To enable validation, install `uv`:

```bash
# macOS/Linux
curl -LsSf https://astral.sh/uv/install.sh | sh

# Or with pip
pip install uv

# Or with Homebrew
brew install uv
```

## Example: Full Test Module

```erlang
-module(product_schema_test).
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_unused_type).

-record(product, {
    id :: integer(),
    name :: string(),
    price :: float()
}).

-type product() :: #product{}.

%% Helper function
validate_with_python(Schema) ->
    case json_schema_validator_helper:validate_schema_2020_12(Schema) of
        ok -> ok;
        {skip, Reason} ->
            io:format("Skipping Python validation: ~s~n", [Reason]),
            ok;
        {error, {validation_failed, Output}} ->
            io:format("Python validation failed:~n~s~n", [Output]),
            erlang:error({python_validation_failed, Output})
    end.

%% Test
product_schema_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {record, product}),

    %% Verify structure
    ?assertMatch(
        #{<<"$schema">> := _,
          type := <<"object">>,
          properties := _},
        Schema
    ),

    %% Validate with Python
    validate_with_python(Schema).
```

## Applying to Existing Tests

### Before

```erlang
simple_types_test() ->
    ?assertEqual(
        {ok, #{<<"$schema">> => <<"...">>, type => <<"integer">>}},
        spectra_json_schema:to_schema(?MODULE, {type, my_integer, 0})
    ).
```

### After

```erlang
simple_types_test() ->
    {ok, IntSchema} = spectra_json_schema:to_schema(?MODULE, {type, my_integer, 0}),
    ?assertEqual(
        #{<<"$schema">> => <<"...">>, type => <<"integer">>},
        IntSchema
    ),
    validate_with_python(IntSchema).
```

## Test Files to Update

Consider adding validation to these test modules:

- `test/spectra_json_schema_test.erl` - ✅ Already updated
- `test/pr45_comment_issues_test.erl` - JSON Schema generation tests
- `test/spectra_json_schema_remote_type_test.erl` - Remote type tests
- `test/integer_key_map_test.erl` - Integer key map schema tests

**Note**: Don't add to `test/openapi_json_test.erl` - OpenAPI schemas have `$schema` stripped for embedding.

## Troubleshooting

### Validation skipped

```
Skipping Python validation: uv not installed, skipping JSON Schema 2020-12 validation
```

**Solution**: Install `uv` (see Installation section above)

### Validation failed

```
Python validation failed:
❌ Schema does not declare JSON Schema 2020-12
```

**Solution**: Check that your schema has the correct `$schema` field

### Tests pass without uv

This is expected behavior. If `uv` is not installed, tests skip validation and continue normally. This ensures tests work in environments without Python/uv.

## CI/CD Integration

To enforce validation in CI/CD:

1. Install `uv` in your CI environment
2. Run tests normally - validation will happen automatically
3. Tests will fail if schemas are invalid

Example GitHub Actions:

```yaml
- name: Install uv
  run: curl -LsSf https://astral.sh/uv/install.sh | sh

- name: Run tests with validation
  run: rebar3 eunit
```

## Performance

- Validation adds ~50-100ms per schema
- Uses temporary files with automatic cleanup
- Schemas are cached by uv for faster subsequent runs
- Minimal impact on test suite performance

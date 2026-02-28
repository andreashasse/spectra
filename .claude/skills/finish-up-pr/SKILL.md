---
name: finish-up-pr
description: Finalize a PR by cleaning up comments, verifying test coverage, checking README accuracy, and tightening type specs.
---

# Finish Up PR Skill

You are performing a final quality pass on the current PR before it is merged. Work through each step below in order, making fixes directly in the code.

## Step 1: Understand the Changes

Get the full diff of this branch against main:

```bash
git diff main...HEAD
git diff main...HEAD --stat
git log main..HEAD --oneline
```

Read every changed file in full before making any edits. Build a clear picture of:
- Which modules/functions were added, modified, or deleted
- What types and type specs were affected
- What the intended behavior change is

## Step 2: Remove Unnecessary Comments

In each changed file, look for and remove:
- **Commented-out code**: Lines or blocks of code that are commented out (e.g., `%% old_function(X) -> ...`)
- **Resolved TODO/FIXME comments**: Comments like `%% TODO: fix this` that refer to work already done in this PR
- **Redundant/obvious comments**: Comments that merely restate what the code does without adding insight (e.g., `%% Increment counter` above `Counter + 1`)

Do NOT remove:
- Comments that explain *why* something is done a non-obvious way
- Module/function doc comments that add real value
- Comments that reference issues, specs, or external context

After removing, run `make format` to re-format.

## Step 3: Check Test Coverage

Identify every function and type that was added or meaningfully changed in this PR.

For each changed public function or behaviour, verify there is:
- At least one **positive test** (valid input produces correct output)
- At least one **negative test** (invalid input produces the expected error or rejection)

Check the test files in `test/` for coverage. Prefer calling through `spectra.erl` for public API tests; use internal modules like `spectra_json.erl` only when you are explicitly testing internal behaviour.

If a function is missing a positive or negative test, write one. Place new tests in the appropriate existing test file, or create a new `test/<feature>_test.erl` file if none exists.

After adding tests, run:
```bash
make build-test
```

Report which tests were added and which functions they cover.

## Step 4: Check README.md Accuracy

Read `README.md` in full and compare it against the changes made in this PR.

Check for:
- **New public API functions** — are they documented?
- **Changed function signatures or behaviour** — is the documentation updated?
- **New supported types or formats** — are they listed?
- **Removed or deprecated features** — are they reflected?
- **Code examples** — do they still compile and match actual behaviour?

If the README is out of date, update it. Keep additions concise and consistent with the existing style.

## Step 5: Tighten Type Specs

Review all type definitions (`-type`, `-opaque`) and function specs (`-spec`) in changed files.

Look for types that are less specific than they should be:

- `tuple()` where a record type like `#some_record{}` could be used
- `any()` or `term()` where a concrete type is known
- `list()` where `[specific_type()]` could be used
- `integer()` where a range like `1..5` or a union of literals would be more precise
- `atom()` where a union of specific atoms (e.g., `ok | error | pending`) applies
- Return types like `{ok, term()} | {error, term()}` that could name their payload types

For each loosely-typed spec you find:
1. Check whether a tighter type is actually enforced by the code and tests
2. If yes, tighten the type
3. If not, leave a comment explaining why (e.g., truly polymorphic)

After changes, run:
```bash
make format
make build-test
```

## Step 6: Final Check

Run the full test suite including property-based tests:

```bash
make proper
```

If `make proper` fails, investigate and fix before finishing.

## Step 7: Summary

Report back to the user with a concise summary:
- Comments removed (with file:line references)
- Tests added (with function names they cover)
- README changes made
- Type specs tightened (with before/after)
- Any issues found that could not be automatically fixed (with a recommendation)

---
name: address-pr-comments
description: Review and address open (unresolved) GitHub PR review comments. Assesses relevance, correctness, and whether to implement in this PR or as a follow-up. For bugs, writes a failing test first. For design issues, considers updating CLAUDE.md.
---

# Address PR Comments Skill

You are performing a focused pass to address all open, unresolved review comments on the current pull request. Work through each comment systematically.

## Step 1: Identify the Pull Request

Detect the current PR from the branch:

```bash
gh pr view --json number,title,baseRefName,headRefName
```

## Step 2: Fetch All Unresolved Review Threads

Use the GitHub GraphQL API to retrieve only **unresolved** review threads (this correctly excludes resolved conversations):

```bash
gh api graphql -f query='
{
  repository(owner: "OWNER", name: "REPO") {
    pullRequest(number: PR_NUMBER) {
      reviewThreads(first: 100) {
        nodes {
          isResolved
          isOutdated
          comments(first: 20) {
            nodes {
              id
              body
              path
              line
              originalLine
              author { login }
              createdAt
              url
            }
          }
        }
      }
    }
  }
}'
```

To get OWNER and REPO:
```bash
gh repo view --json owner,name
```

**Filter the results**: Only process threads where `isResolved` is `false`. Skip any thread where `isResolved` is `true` — do not touch those at all.

Also fetch general (non-inline) PR comments:
```bash
gh pr view PR_NUMBER --json comments --jq '.comments[] | {author: .author.login, body: .body, createdAt: .createdAt}'
```

## Step 3: Read the Relevant Code

Before assessing any comment, read the file(s) it refers to. Do not rely on memory of what the code looks like — read each file fresh.

## Step 4: Assess Each Unresolved Comment

For each unresolved comment, make a structured assessment:

### Relevance
- Has the code the comment refers to already been changed or deleted in a later commit?
- If the concern no longer exists, note it as "no longer applicable" and skip.

### Correctness
- Is the reviewer right? Read the referenced code carefully and apply your own judgment.
- The reviewer may be wrong, partially right, or may be applying a convention that doesn't fit this codebase. Be honest.

### Value
Rate the comment as one of:
- **Must fix**: Correctness bug, type error, broken edge case, or clear violation of project guidelines
- **Should fix**: Legitimate style or design concern that would improve maintainability
- **Nice to have**: Minor preference or style nit — low ROI for the effort
- **Disagree**: The comment is incorrect or misunderstands the code — leave as-is with a note

### Timing
- **This PR**: Fix anything that is a bug, that touches code already modified in this PR, or that is quick and low risk.
- **Follow-up**: Refactors, larger design changes, or changes that would require touching unrelated code. Create a GitHub issue for these.

## Step 4b: Present Assessment and Ask for Confirmation

Before implementing anything, show the full assessment as a table:

| # | File / Location | Reviewer | Rating | Timing | Proposed Action |
|---|----------------|----------|--------|--------|-----------------|
| 1 | `src/foo.erl:42` | alice | Must fix | This PR | Write failing test, then fix |
| 2 | `src/bar.erl:10` | bob | Should fix | This PR | Fix code; consider CLAUDE.md addition |
| 3 | `src/baz.erl:55` | alice | Should fix | Follow-up | Open GitHub issue |
| 4 | `src/qux.erl:7`  | bob | No longer applicable | — | Skip |
| 5 | `src/quux.erl:3` | carol | Disagree | — | Leave as-is |

For each non-trivial row, include one sentence of reasoning below the table explaining your rating.

Then ask:

> "Does this assessment look right? Let me know if you'd like to change any category or action before I proceed."

Wait for the user to confirm or correct before writing any code.

## Step 5: Implement Fixes

Once the user confirms (or corrects the plan), work through all approved "Must fix" and "Should fix" comments in order.

Once the user confirms (or corrects the plan), work through all approved "Must fix" and "Should fix" comments in order.

### If the fix is a bug:

1. **Write a failing test first.** Add a test to the appropriate file in `test/` that reproduces the exact problem described. The test must fail before the fix.
2. Run `make build-test` to confirm the test fails.
3. Implement the fix in the source code.
4. Run `make build-test` to confirm the test now passes.
5. Run `make proper` to verify no property tests regress.

### If the fix is a style or design problem:

1. Apply the fix to the code.
2. Consider: **Is this a pattern that will recur?** If so, add a concise guideline to `CLAUDE.md` under the appropriate section to prevent the same issue from appearing in future code. Keep CLAUDE.md additions short and actionable — one bullet or sentence.
3. Run `make format && make build-test` after any code changes.

### If the fix is "nice to have" or disagreed with:

Do not modify any code. Record your reasoning in the summary.

## Step 6: Handle Follow-ups

For comments you have categorized as "Follow-up" and that the user confirmed in Step 5, create a GitHub issue:

```bash
gh issue create --title "TITLE" --body "BODY" --label "enhancement"
```

Link the issue in the PR as a comment so reviewers know it is tracked:

```bash
gh pr comment PR_NUMBER --body "Opened #ISSUE_NUMBER to track: BRIEF_DESCRIPTION"
```

## Step 7: Final Verification

After all fixes are applied:

```bash
make format
make build-test
make proper
```

All tests must pass before finishing.

## Step 8: Summary

Report back with a table covering every unresolved comment:

| # | File / Location | Reviewer | Assessment | Action Taken |
|---|----------------|----------|------------|--------------|
| 1 | `src/foo.erl:42` | alice | Must fix — off-by-one error | Test added, fixed |
| 2 | `src/bar.erl:10` | bob | Should fix — design concern | Fixed; added CLAUDE.md guideline |
| 3 | `src/baz.erl:55` | alice | Follow-up — larger refactor | Opened #42 |
| 4 | `src/qux.erl:7`  | bob | No longer applicable | Skipped |
| 5 | `src/quux.erl:3` | carol | Disagree — code is correct | Left as-is; explanation: ... |

Then list:
- **Tests added**: function names and files
- **CLAUDE.md additions**: what was added and why
- **Issues created**: issue numbers and titles
- **Build status**: pass/fail

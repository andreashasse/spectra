---
name: release
description: Prepare and publish a new release. Updates CHANGELOG.md and README.md with version information, analyzes git commits to determine semantic version, and guides through the release process.
---

# Release Skill

You are helping to prepare a new release of the spectra library.

## Step 1: Detect Current Version

Read the CHANGELOG.md to determine the current version (the most recent version listed at the top).

## Step 2: Analyze Changes

Check recent git commits since the last release:
```bash
git log --oneline --since="$(git log -1 --format=%ai $(git describe --tags --abbrev=0 2>/dev/null || echo HEAD))" 2>/dev/null || git log --oneline -20
```

Also check git diff to see what files have changed:
```bash
git diff $(git describe --tags --abbrev=0 2>/dev/null || echo HEAD~20)..HEAD --stat
```

Analyze the commits and changes to understand:
- Are there breaking changes?
- Are there new features?
- Are there bug fixes?
- Are there documentation updates?

## Step 3: Suggest Version Bump

Based on semantic versioning:
- **Major** (X.0.0): Breaking changes, incompatible API changes
- **Minor** (0.X.0): New features, backwards-compatible functionality
- **Patch** (0.0.X): Bug fixes, backwards-compatible fixes

Use the AskUserQuestion tool to present three options showing what the new version would be for each choice (major, minor, or patch). Indicate which one you recommend based on the changes.

## Step 4: Generate Changelog Entries

Based on the git commits and changes, draft changelog entries organized into sections:
- **Added**: New features
- **Changed**: Changes in existing functionality
- **Fixed**: Bug fixes
- **Deprecated**: Soon-to-be removed features
- **Removed**: Removed features
- **Security**: Security fixes

Only include sections that have entries. Keep entries concise and user-focused.

## Step 5: Update Files

After the user selects a version bump, update these files:

### README.md
- Find and update the installation example: `{spectra, "~> X.Y.Z"}`

### CHANGELOG.md
- Add a new version section at the top (after the header, before the previous version)
- Use today's date in ISO format (YYYY-MM-DD)
- Include the changelog entries you generated
- Format:
  ```markdown
  ## [X.Y.Z] - YYYY-MM-DD

  ### Added
  - New feature 1
  - New feature 2

  ### Changed
  - Change 1
  - Change 2

  ### Fixed
  - Bug fix 1

  ```

## Step 6: Inform User

After updating the files, inform the user that the files have been updated and they should:
1. Review and edit the CHANGELOG.md entries if needed
2. Run `make release` when ready to publish

## Important Notes

- Do NOT modify `src/spectra.app.src` - it uses `{vsn, "git"}` which gets version from git tags
- Do NOT modify `rebar.config` - it doesn't contain version information
- Do NOT run `make release` - the user will do that
- Try to write clear, user-focused changelog entries
- Group related changes together
- Omit internal refactorings unless they significantly impact users

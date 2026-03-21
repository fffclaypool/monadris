---
name: branch-add-commit
description: Use when the user wants to create a branch, commit changes, or both. Triggers on requests like "ブランチ切って", "コミットして", "ブランチ切ってコミット", "commit this", "create a branch and commit".
argument-hint: [description]
---

Handle branch creation and/or committing based on the user's request and `$ARGUMENTS`.

## Branch Naming Rules (from shared config)

Valid prefixes (loaded from `.github/config/branch-prefixes.conf`):

!`cat .github/config/branch-prefixes.conf`

### Format

`<prefix>/<short-kebab-case-description>`

## Commit Message Conventions

- Style: concise, descriptive (e.g., `feat: implement game loop`, `fix: wall kick near boundary`)
- **No AI attribution**: Do NOT include "Co-authored-by: Claude" or any AI attribution

## Behavior

Determine what the user is asking for and execute accordingly:

### If creating a branch:
1. Determine the appropriate prefix from the change type
2. Generate a concise kebab-case slug from the description or staged changes
3. Run `git checkout -b <branch-name>`

### If committing:
1. Run `git status` and `git diff --staged` to understand changes
2. Stage relevant files (prefer specific files over `git add -A`)
3. Write a commit message following the conventions above
4. Commit (if pre-commit hook reformats files, re-stage and create a NEW commit)

### If both:
1. Create the branch first
2. Then stage and commit

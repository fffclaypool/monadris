# Contributing to Monadris

Thank you for your interest in contributing to Monadris! This document outlines the guidelines and workflows for contributing to this project.

## Branch Naming Convention

This repository enforces a strict branch naming convention. **Pull requests from branches that do not follow this convention will fail CI and cannot be merged.**

### Required Format

```
<type>/<description>
```

### Valid Prefixes

| Prefix | Purpose | Auto-assigned Label |
|--------|---------|---------------------|
| `feature/` | New features or enhancements | `enhancement` |
| `fix/` | Bug fixes | `bug` |
| `hotfix/` | Urgent production fixes | `bug` |
| `refactor/` | Code refactoring (no functional changes) | `refactor` |
| `docs/` | Documentation updates | `documentation` |
| `chore/` | Maintenance tasks | `chore` |
| `update/` | Dependency updates (used by Scala Steward) | `dependencies` |

### Examples

**Valid branch names:**
- `feature/add-tetromino-rotation`
- `fix/score-calculation-bug`
- `hotfix/critical-crash`
- `refactor/clean-up-game-logic`
- `docs/update-readme`
- `chore/update-dependencies`

**Invalid branch names:**
- `add-tetromino` (missing prefix)
- `testing` (missing prefix and description)
- `update` (missing prefix and description)
- `feature-add-tetromino` (wrong separator, use `/` not `-`)

### Why This Matters

- **Automated Release Notes**: Proper branch names enable automatic categorization in release notes
- **Clear Intent**: The prefix immediately communicates the purpose of the change
- **Consistent History**: Makes the project history easier to navigate and understand

## Development Workflow

1. Create a new branch from `main` with the appropriate prefix
2. Make your changes
3. Open a Pull Request
4. Wait for CI checks to pass (including branch name validation)
5. Request review and address feedback
6. Merge after approval

## Code Style

Please refer to `CLAUDE.md` for detailed coding standards, including:
- Scala 3 with significant indentation
- Pure Functional Programming (no `var`, immutable data structures)
- ZIO 2.x for effects (in `app` layer only)
- WartRemover enforcement in `core` layer

## Questions?

If you have any questions about contributing, please open an issue for discussion.

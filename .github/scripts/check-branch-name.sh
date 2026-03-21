#!/usr/bin/env bash
set -euo pipefail

readonly BRANCH_NAME="${BRANCH_NAME:?BRANCH_NAME is required}"
readonly SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
readonly CONFIG_FILE="$SCRIPT_DIR/../config/branch-prefixes.conf"

# Read prefixes from shared config (strip comments and blank lines)
load_prefixes() {
    sed 's/#.*//; s/[[:space:]]//g' "$CONFIG_FILE" | grep -v '^$' | paste -sd '|' -
}

readonly PREFIXES="$(load_prefixes)"
readonly PATTERN="^($PREFIXES)/.+"

main() {
    echo "Checking branch name: $BRANCH_NAME"

    if [[ "$BRANCH_NAME" =~ $PATTERN ]]; then
        echo "Branch name '$BRANCH_NAME' follows the naming convention."
        exit 0
    fi

    echo "::error::Branch name '$BRANCH_NAME' does not follow the naming convention."
    echo ""
    echo "Expected format: <type>/<description>"
    echo "Valid prefixes: $(echo "$PREFIXES" | tr '|' ', ' | sed 's/\([^,]*\)/\1\//g')"
    echo ""
    echo "Examples:"
    echo "  - feature/add-tetromino"
    echo "  - fix/score-bug"
    echo "  - update/zio-version (for dependencies)"
    echo ""
    echo "Please rename your branch and create a new PR."
    exit 1
}

main "$@"

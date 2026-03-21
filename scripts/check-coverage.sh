#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BASELINE_FILE="$PROJECT_ROOT/.coverage-baseline.json"
SCALA_VERSION="3.3.5"

if [[ ! -f "$BASELINE_FILE" ]]; then
  echo "ERROR: Baseline file not found: $BASELINE_FILE"
  echo "Run 'make coverage-baseline' to generate it."
  exit 1
fi

echo "Running coverage analysis (unit tests only)..."
(cd "$PROJECT_ROOT" && sbt "coverage; test; coverageReport")

extract_rate() {
  local project="$1"
  local xml_path="$PROJECT_ROOT/$project/target/scala-$SCALA_VERSION/scoverage-report/scoverage.xml"
  if [[ ! -f "$xml_path" ]]; then
    echo "ERROR: Coverage report not found: $xml_path" >&2
    return 1
  fi
  grep -oP 'statement-rate="\K[0-9.]+' "$xml_path" | head -1
}

read_baseline() {
  local project="$1"
  grep -oP "\"$project\"\\s*:\\s*\\K[0-9.]+" "$BASELINE_FILE"
}

compare() {
  local project="$1"
  local baseline actual

  baseline=$(read_baseline "$project")
  actual=$(extract_rate "$project")

  if (( $(echo "$actual < $baseline" | bc -l) )); then
    printf "  %-6s: %.2f%% -> %.2f%% FAIL (baseline: %.2f%%)\n" "$project" "$baseline" "$actual" "$baseline"
    return 1
  else
    printf "  %-6s: %.2f%% -> %.2f%% OK\n" "$project" "$baseline" "$actual"
    return 0
  fi
}

echo ""
echo "Coverage comparison:"
failed=0
compare "core" || failed=1
compare "app"  || failed=1

echo ""
if [[ "$failed" -eq 1 ]]; then
  echo "FAILED: Coverage regression detected."
  echo "If intentional, update the baseline with 'make coverage-baseline'."
  exit 1
else
  echo "PASSED: No coverage regression."
fi

.PHONY: all build build-core build-app test test-all test-core test-app stress-test integration-test run clean help
.PHONY: fmt fmt-check lint lint-check coverage coverage-check coverage-baseline check fix

all: build

build:
	sbt compile

build-core:
	sbt core/compile

build-app:
	sbt app/compile

test:
	sbt test

test-core:
	sbt core/test

test-app:
	sbt app/test

stress-test:
	sbt stressTest

integration-test:
	sbt integrationTest

test-all: test stress-test integration-test

run:
	sbt app/run

clean:
	sbt clean

fmt:
	sbt scalafmtAll

fmt-check:
	sbt scalafmtCheckAll

lint:
	sbt "scalafix; Test/scalafix"

lint-check:
	sbt "scalafix --check; Test/scalafix --check"

coverage:
	sbt "coverage; test; integrationTest; coverageReport"

coverage-check:
	scripts/check-coverage.sh

coverage-baseline: coverage-check
	@SCALA_VERSION=3.3.5; \
	CORE_RATE=$$(grep -oP 'statement-rate="\K[0-9.]+' core/target/scala-$$SCALA_VERSION/scoverage-report/scoverage.xml | head -1); \
	APP_RATE=$$(grep -oP 'statement-rate="\K[0-9.]+' app/target/scala-$$SCALA_VERSION/scoverage-report/scoverage.xml | head -1); \
	printf '{\n  "core": %s,\n  "app": %s\n}\n' "$$CORE_RATE" "$$APP_RATE" > .coverage-baseline.json; \
	echo "Baseline updated: core=$$CORE_RATE%, app=$$APP_RATE%"

check: fmt-check lint-check
	@echo "All checks passed"

fix: fmt lint
	@echo "All fixes applied"

help:
	@echo "Available targets:"
	@echo ""
	@echo "Build:"
	@echo "  build        - Build all projects"
	@echo "  build-core   - Build core project only"
	@echo "  build-app    - Build app project only"
	@echo ""
	@echo "Test:"
	@echo "  test         - Run unit tests"
	@echo "  test-all     - Run all tests (unit + stress + integration)"
	@echo "  test-core    - Run core tests only"
	@echo "  test-app     - Run app tests only"
	@echo "  stress-test  - Run stress tests only"
	@echo "  integration-test - Run integration tests (requires Docker)"
	@echo "  coverage     - Run all tests with coverage report (requires Docker)"
	@echo "  coverage-check    - Check coverage against baseline (unit tests only)"
	@echo "  coverage-baseline - Update baseline with current coverage"
	@echo ""
	@echo "Lint/Format:"
	@echo "  fmt          - Format code with scalafmt"
	@echo "  fmt-check    - Check code formatting"
	@echo "  lint         - Fix lint issues with scalafix"
	@echo "  lint-check   - Check for lint issues"
	@echo "  fix          - Run fmt + lint"
	@echo "  check        - Run fmt-check + lint-check"
	@echo ""
	@echo "Other:"
	@echo "  run          - Run the application"
	@echo "  clean        - Clean build artifacts"
	@echo "  help         - Show this help"

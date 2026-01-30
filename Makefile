.PHONY: all build build-core build-app test test-core test-app stress-test run clean help
.PHONY: fmt fmt-check lint lint-check coverage check fix

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
	sbt "coverage; test; coverageReport"

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
	@echo "  test         - Run all tests"
	@echo "  test-core    - Run core tests only"
	@echo "  test-app     - Run app tests only"
	@echo "  stress-test  - Run stress tests only"
	@echo "  coverage     - Run tests with coverage report"
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

# Monadris Project Guidelines

## Build & Test Commands
- Build: `sbt compile`
- Test: `sbt test`
- Run: `sbt run`
- Single Test: `sbt "testOnly monadris.package.ClassName"`
- Fix Lint/Format: `sbt scalafix` (if configured)

## Git Workflow
- **Commit Messages**: Write concise and descriptive messages (e.g., "feat: implement game loop", "refactor: separate view logic").
- **No AI Attribution**: Do **NOT** include "Co-authored-by: Claude" or any AI attribution in commit messages.

## Coding Standards
- **Language**: Scala 3 (latest syntax). Use significant indentation (no braces for blocks).
- **Paradigm**: Pure Functional Programming.
  - **NO `var`**: Use `val`, recursion, `foldLeft`, or `map`/`flatMap`.
  - **Immutability**: All data structures must be immutable (`case class`, `enum`, `Vector`).
- **No Magic Numbers**:
  - Avoid using raw numbers or string literals directly in logic AND tests.
  - Define them as named constants in a `private object` (e.g., `private object Layout`, `object Constants`) or `AppConfig`.
- **Effect System**: ZIO 2.x
  - Use `ZIO` for all side effects.
  - Pure logic (domain/view) should NOT depend on ZIO.

## Architecture (Strict Layering)
1. **domain/** (Core Model)
   - Pure data structures (`case class`, `enum`).
   - No dependencies on other layers.
   - Example: `GameState`, `Grid`, `Tetromino`, `Input`.
2. **logic/** (Business Logic)
   - Pure functions only: `(State, Input) => State`.
   - Depends on `domain`.
   - Example: `GameLogic`, `Collision`, `LineClearing`.
3. **view/** (Presentation Logic)
   - Pure data transformation: `State => ViewModel`.
   - No ANSI codes here. Use abstract `UiColor`.
   - Depends on `domain`, `config`.
   - Example: `GameView`, `ScreenBuffer`, `ViewModel`.
4. **infrastructure/** (Infrastructure & Effects)
   - Impure layer handling ZIO effects (Console, TTY).
   - Depends on `view`, `domain`, `config`.
   - Example: `ConsoleRenderer`, `TerminalInput`, `GameRunner`.
5. **config/**
   - Application configuration (`AppConfig`).

## Naming Conventions
- Use `CamelCase` for classes and traits.
- Use `camelCase` for functions and values.
- Enums should be used for fixed sets of values (e.g., `TetrominoShape`, `UiColor`).

## Error Handling
- Use ZIO's error channel (`ZIO[R, E, A]`) for effectful errors.
- Use `Option` or `Either` for pure logic errors. Never throw exceptions.

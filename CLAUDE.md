# Monadris Project Guidelines

## Build & Test Commands
- Build: `sbt compile` (builds all projects)
- Build core only: `sbt core/compile`
- Build app only: `sbt app/compile`
- Test: `sbt test` (runs all tests)
- Test core only: `sbt core/test`
- Test app only: `sbt app/test`
- Run: `sbt app/run`
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
- **Effect System**: ZIO 2.x (app layer only)
  - Use `ZIO` for all side effects.
  - Pure logic (core) should NOT depend on ZIO.

## Architecture (SBT Multi-Project)

### Project Structure
```
monadris/
├── core/                 # Pure logic (ZIO-independent)
│   └── src/
│       ├── main/scala/monadris/
│       │   ├── domain/       # Pure data structures
│       │   │   └── config/   # Pure config case classes
│       │   ├── logic/        # Pure game logic
│       │   └── view/         # Pure view transformation
│       └── test/scala/monadris/
├── app/                  # Impure layer (ZIO-dependent)
│   └── src/
│       ├── main/scala/monadris/
│       │   ├── config/       # ZIO Config loading (ConfigLayer)
│       │   └── infrastructure/
│       └── test/scala/monadris/
│       └── resources/        # application.conf, logback.xml
└── build.sbt
```

### Layer Separation

#### `core` Project (Pure Layer)
- **No ZIO dependency** - pure Scala only
- **WartRemover enforced** - `var`, `null`, `throw`, `return` are compile errors
- Contains:
  - `domain/` - Pure data structures (`GameState`, `Grid`, `Tetromino`, `Input`)
  - `domain/config/` - Pure `case class` definitions (`AppConfig`, etc.)
  - `logic/` - Pure functions: `(State, Input) => State`
  - `view/` - Pure transformation: `State => ScreenBuffer`

#### `app` Project (Impure Layer)
- Depends on `core`
- ZIO 2.x for effects
- WartRemover disabled (allows ZIO-style patterns)
- Contains:
  - `config/ConfigLayer` - ZIO Config loading from `application.conf`
  - `infrastructure/` - `ConsoleRenderer`, `TerminalInput`, `GameRunner`
  - `Main.scala` - Application entry point

### WartRemover (Purity Enforcement)
The `core` project uses WartRemover to enforce pure functional programming:
- **Forbidden**: `Var`, `Null`, `Return`, `Throw`, `AsInstanceOf`, `IsInstanceOf`
- **Allowed**: `DefaultArguments` only
- Tests are exempt from WartRemover checks

## Naming Conventions
- Use `CamelCase` for classes and traits.
- Use `camelCase` for functions and values.
- Enums should be used for fixed sets of values (e.g., `TetrominoShape`, `UiColor`).

## Error Handling
- Use ZIO's error channel (`ZIO[R, E, A]`) for effectful errors in `app`.
- Use `Option` or `Either` for pure logic errors in `core`. Never throw exceptions.

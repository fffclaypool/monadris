# Monadris

A functional Tetris implementation in Scala 3 + ZIO.

## Features

- **Immutability**: All data structures are immutable (`case class` / `enum`)
- **Pure Functions**: Game logic follows `(State, Input) => State` signature
- **Effect Separation**: Rendering, input, and randomness are wrapped in ZIO, completely separated from core logic

## Architecture

```
┌─────────────────────────────────────────────┐
│           Effect Layer (ZIO)                │
│   Renderer │ InputHandler │ RandomPiece     │
└──────────────────┬──────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────┐
│           Pure Core Logic                   │
│   (GameState, Input) => GameState           │
│   Collision │ LineClearing │ GameLogic      │
└──────────────────┬──────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────┐
│           Domain Models (Immutable)         │
│   Position │ Tetromino │ Grid │ GameState   │
└─────────────────────────────────────────────┘
```

## Requirements

- Java 21+
- sbt 1.9+

## Build & Run

```bash
# Compile
sbt compile

# Run tests
sbt test

# Run game
sbt run
```

## Controls

| Key | Action |
|-----|--------|
| `←` `→` / `H` `L` | Move left/right |
| `↓` / `J` | Soft drop |
| `↑` / `K` | Rotate clockwise |
| `Z` | Rotate counter-clockwise |
| `Space` | Hard drop |
| `P` | Pause |
| `Q` | Quit |

## Project Structure

```
src/main/scala/monadris/
├── domain/           # Immutable data models
│   ├── Position.scala
│   ├── Tetromino.scala
│   ├── Grid.scala
│   └── GameState.scala
├── logic/            # Pure functions
│   ├── Collision.scala
│   ├── LineClearing.scala
│   └── GameLogic.scala
├── effect/           # ZIO effect layer
│   └── GameRunner.scala
└── Main.scala
```

## Testing

```bash
sbt test
```

67 tests covering domain models, collision detection, line clearing, and game logic.

## License

MIT

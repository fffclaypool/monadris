# Monadris

[![CI](https://github.com/fffclaypool/monadris/actions/workflows/ci.yml/badge.svg)](https://github.com/fffclaypool/monadris/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/fffclaypool/monadris/graph/badge.svg)](https://codecov.io/gh/fffclaypool/monadris)
![Scala](https://img.shields.io/badge/scala-3.3.5-dc322f.svg?logo=scala&logoColor=white)
![ZIO](https://img.shields.io/badge/ZIO-2.0-1a237e.svg?logo=scala&logoColor=white)
![License](https://img.shields.io/badge/license-MIT-green.svg)

[![Scalafmt](https://img.shields.io/badge/code_style-scalafmt-c22d40.svg?logo=scala&logoColor=white)](.scalafmt.conf)
[![Scalafix](https://img.shields.io/badge/linter-scalafix-brightgreen.svg?logo=scala&logoColor=white)](.scalafix.conf)
[![WartRemover](https://img.shields.io/badge/purity-WartRemover-blueviolet.svg?logo=scala&logoColor=white)](build.sbt)

A strictly functional Tetris implementation in Scala 3 + ZIO.
**Zero variables (`var`), zero exceptions, and zero side effects in the core domain.**

## Demo

https://github.com/user-attachments/assets/4d8b7920-68e7-45d8-9a86-fbe476922b3c

## Features

- **Zero Mutation**: `var`, `null`, `throw`, and `return` are forbidden at compile time via **WartRemover** (in `core`).
- **Physical Separation**: Multi-project architecture strictly isolates pure `core` logic from impure `app` infrastructure.
- **Pure Functions**: Game logic is modeled strictly as `(State, Input) => State`.
- **Event-Driven**: Utilizing **ZIO Queue** for non-blocking, thread-safe event handling.
- **Effect Isolation**: Rendering, input, and time are wrapped in ZIO effects.
- **Configurable**: Game settings are loaded from HOCON configuration files.

## Architecture

### 1. Data Flow
The internal state management follows a strict unidirectional data flow pattern (The Elm Architecture)

```mermaid
graph LR
    %% Styles
    classDef infra fill:#e3f2fd,stroke:#1e88e5,stroke-width:2px,color:#0d47a1,rx:5,ry:5;
    classDef core fill:#e8f5e9,stroke:#43a047,stroke-width:2px,color:#1b5e20,rx:5,ry:5;
    classDef view fill:#fff3e0,stroke:#fb8c00,stroke-width:2px,color:#e65100,rx:5,ry:5;
    classDef terminal fill:#263238,stroke:#000000,stroke-width:2px,color:#ffffff,rx:10,ry:10;

    Input["Input / Ticker"]:::infra
    Queue["ZIO Queue"]:::infra
    Loop["Game Loop"]:::infra
    Renderer["Console Renderer"]:::infra

    Logic["Game Logic"]:::core
    State["Domain State"]:::core

    View["Game View"]:::view

    Terminal(("Terminal Output")):::terminal

    Input -->|Command| Queue
    Queue -->|Event| Loop
    Loop -->|Action| Logic
    Logic -->|Update| State
    State -->|Data| View
    View -->|ViewModel| Renderer
    Renderer -->|ANSI| Terminal
```

### 2. Module Separation
The project is physically split into two SBT modules to prevent architectural erosion.

```mermaid
graph TD
    %% Styles
    classDef pure fill:#e8f5e9,stroke:#2e7d32,stroke-width:2px,color:#1b5e20;
    classDef impure fill:#e3f2fd,stroke:#1565c0,stroke-width:2px,color:#0d47a1;
    classDef boundary fill:none,stroke:#90a4ae,stroke-width:2px,stroke-dasharray: 5 5;

    subgraph AppProject ["App Project (Impure / ZIO)"]
        direction TB
        Main["Main / ZIO App"]:::impure
        Infra["Infrastructure"]:::impure
        ConfigLoader["Config Loader"]:::impure
    end

    subgraph Boundary ["🚧 Physical Barrier"]
        direction TB
        subgraph CoreProject ["Core Project (Pure Scala)"]
            Logic["Game Logic"]:::pure
            Domain["Domain Models"]:::pure
            View["View Logic"]:::pure
        end
    end

    Main --> Infra
    Infra --> Logic
    Infra --> View
    Logic --> Domain
    View --> Domain
    ConfigLoader --> Domain
```

| Layer | Project | ZIO Dependency | Description |
|-------|---------|----------------|-------------|
| **Domain** | `core` | **No** | Immutable data structures (`GameState`, `Grid`, `Tetromino`) |
| **Config** | `core` | **No** | Pure configuration definitions (`AppConfig`) |
| **Game** | `core` | **No** | Pure state transitions (`GameLogic`, `GameLoop`, `Collision`) |
| **Input** | `core` | **No** | Input processing (`KeyMapping`, `GameCommand`) |
| **Replay** | `core` | **No** | Replay recording/playback (`ReplayBuilder`, `ReplayPlayer`) |
| **View** | `core` | **No** | Pure transformation (`State => ScreenBuffer`) |
| **Infrastructure** | `app` | **Yes** | ZIO effects, Console I/O, Queues, Persistence |

### 3. Runtime Event Loop
How ZIO handles concurrent inputs and serializes them into the game loop.

```mermaid
sequenceDiagram
    participant User as 👤 Player
    participant Input as ⚡ Input Fiber
    participant Timer as ⏰ Timer Fiber
    participant Queue as 📥 ZIO Queue
    participant GameLoop as 🔄 Game Loop
    participant Core as 🧠 Pure Core
    participant Screen as 🖥️ Terminal

    Note over Input, Timer: Running in Parallel (ZIO Fibers)

    par Parallel Inputs
        User->>Input: Press Key
        Input->>Queue: Offer(UserAction)
    and
        Timer->>Timer: Sleep(100ms)
        Timer->>Queue: Offer(TimeTick)
    end

    Note over Queue, GameLoop: Serialize Concurrent Events

    loop Every Event
        Queue->>GameLoop: Take(Command)
        GameLoop->>Core: update(State, Command)
        Core-->>GameLoop: New State
        GameLoop->>Core: view(New State)
        Core-->>GameLoop: ScreenBuffer
        GameLoop->>Screen: Render(ANSI Codes)
    end
```

## Development Environment

### Option 1: Dev Containers (Recommended)

The easiest way to get started is using [Dev Containers](https://containers.dev/). All dependencies are pre-configured.

**VS Code:**
1. Install [Docker](https://www.docker.com/) and [VS Code](https://code.visualstudio.com/)
2. Install the [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)
3. Open this repository in VS Code
4. Click "Reopen in Container" when prompted (or run `Dev Containers: Reopen in Container` from the command palette)

The dev container includes:
- Java 21
- sbt, Scala CLI, scalafmt (via Coursier)
- Metals extension for VS Code

### Option 2: Local Setup

**Requirements:**
- Java 21+
- sbt 1.9+
- Bash (for execution script)

## Build & Run

**Note:** Please use the provided shell script. Running directly with `sbt run` may cause display glitches due to terminal mode handling.

```bash
# Compile
sbt compile

# Run game
./run.sh
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

```text
monadris/
├── core/                       # Pure logic (ZIO-independent / WartRemover enforced)
│   └── src/main/scala/monadris/
│       ├── config/             # Pure config definition (AppConfig)
│       ├── domain/             # Immutable data models (GameState, Grid, Tetromino)
│       ├── game/               # Pure game rules (GameLogic, GameLoop, Collision)
│       ├── input/              # Input processing (KeyMapping, GameCommand)
│       ├── replay/             # Replay system (ReplayBuilder, ReplayPlayer)
│       └── view/               # Presentation logic (GameView, AnsiRenderer)
├── app/                        # Impure layer (ZIO-dependent)
│   └── src/main/scala/monadris/
│       ├── config/             # ZIO Config loading (ConfigLayer)
│       ├── infrastructure/     # ZIO effect implementation
│       │   ├── game/           # Game runtime (GameRunner, GameSession, ReplayRunner)
│       │   ├── persistence/    # Replay storage (FileReplayRepository, JsonReplayCodec)
│       │   └── terminal/       # Console I/O (ConsoleRenderer, TerminalInput)
│       └── Main.scala
└── build.sbt
```

## Testing

This project uses **ZIO Test**.
Heavy tests (memory leak checks) are tagged with `heavy` and excluded by default.

```bash
# Run standard unit tests (Fast) - stress tests are excluded by default
sbt test

# Run stress tests only (Slow: 100,000 iterations)
sbt stressTest
```

### Test Coverage
- **Domain**: Immutable data structures, grid operations, tetromino transformations.
- **Game**: State transitions, collision detection, line clearing, scoring.
- **Input**: Key mapping, command parsing.
- **Replay**: Recording, playback, serialization.
- **View**: Layout generation and ViewModel construction.
- **Stress Testing**: Validates memory safety and stack safety by running 100,000 game frames in a simulated environment (`StressTest.scala`).

### Architecture Testing

This project uses **ArchUnit** to automatically verify architectural rules. Any violation will cause `sbt test` to fail.

**Enforced Rules:**
- **Domain Isolation**: Domain layer (`monadris.domain`) must not depend on upper layers (Game, View).
- **Game/View Separation**: Game layer must not depend on View layer, and vice versa.
- **Purity**: Core module must not depend on impure infrastructure APIs (`java.io`, `java.sql`, `java.net`, `java.util.concurrent`) or effect systems (ZIO).
- **No Cycles**: Package dependencies must be free of cycles.

## GitHub Actions

| Workflow | Trigger | Description |
|----------|---------|-------------|
| **CI** | Push/PR to `main` | Lint, format check, compile, test with coverage, SonarCloud scan, Codecov upload |
| **Stress Test** | Weekly (Sun 03:00 UTC) / Manual | Run stress tests (`sbt stressTest`) |
| **Scala Steward** | Daily (15:00 UTC) / Manual | Automated dependency updates |
| **Manual Release** | Manual | Build and create GitHub release with tag |
| **Auto Labeler** | PR events | Add labels to PRs based on branch name |
| **Branch Name Linter** | PR events | Validate branch naming convention |
| **Sync Labels** | Push to `main` (labels.yml) / Manual | Sync GitHub labels from `.github/labels.yml` |

## License

MIT

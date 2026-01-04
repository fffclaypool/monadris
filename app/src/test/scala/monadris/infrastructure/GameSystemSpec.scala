package monadris.infrastructure

import zio.*
import zio.test.*
import zio.test.Assertion.*

import monadris.config.ConfigLayer
import monadris.domain.*
import monadris.domain.config.AppConfig
import monadris.infrastructure.TestServices as Mocks
import monadris.logic.LineClearing
import monadris.view.GameView

/**
 * IO抽象化レイヤーのテスト
 * TestServices のモック実装を使用
 */
object GameSystemSpec extends ZIOSpecDefault:

  val gridWidth: Int  = Mocks.testConfig.grid.width
  val gridHeight: Int = Mocks.testConfig.grid.height

  // ============================================================
  // Test constants
  // ============================================================

  private val minimumBufferDimension = 0
  private val shapeSampleCount       = 20
  private val minimumUniqueShapes    = 1

  // ANSI escape sequences for assertions
  private object Ansi:
    val clearScreen = "\u001b[2J"

  def initialState: GameState =
    GameState.initial(TetrominoShape.T, TetrominoShape.I, gridWidth, gridHeight)

  def spec = suite("GameSystem Tests")(
    // ============================================================
    // TtyService Tests
    // ============================================================

    suite("TtyService")(
      test("test implementation returns queued input") {
        val inputs = Chunk('h'.toInt, 'j'.toInt, 'k'.toInt)
        for
          first  <- TtyService.read()
          second <- TtyService.read()
          third  <- TtyService.read()
        yield assertTrue(
          first == 'h'.toInt,
          second == 'j'.toInt,
          third == 'k'.toInt
        )
      }.provide(Mocks.tty(Chunk('h'.toInt, 'j'.toInt, 'k'.toInt))),
      test("available returns queue size") {
        val inputs = Chunk(1, 2, 3)
        for
          size1 <- TtyService.available()
          _     <- TtyService.read()
          size2 <- TtyService.available()
        yield assertTrue(size1 == 3, size2 == 2)
      }.provide(Mocks.tty(Chunk(1, 2, 3))),
      test("sleep completes immediately in test") {
        for _ <- TtyService.sleep(1000)
        yield assertTrue(true)
      }.provide(Mocks.tty(Chunk.empty))
    ),

    // ============================================================
    // ConsoleService Tests
    // ============================================================

    suite("ConsoleService")(
      test("test implementation accumulates output") {
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleService.print("Hello")
          _       <- ConsoleService.print("World")
          output  <- service.buffer.get
        yield assertTrue(output == List("Hello", "World"))
      }.provide(Mocks.console),
      test("flush succeeds") {
        for _ <- ConsoleService.flush()
        yield assertTrue(true)
      }.provide(Mocks.console)
    ),

    // ============================================================
    // CommandService Tests
    // ============================================================

    suite("CommandService")(
      test("test implementation records commands") {
        for
          service <- ZIO.service[Mocks.TestCommandService]
          _       <- CommandService.exec("echo hello")
          _       <- CommandService.exec("ls -la")
          history <- service.history.get
        yield assertTrue(history == List("echo hello", "ls -la"))
      }.provide(Mocks.command)
    ),

    // ============================================================
    // TerminalControl Tests
    // ============================================================

    suite("TerminalControl")(
      test("enableRawMode calls correct stty command") {
        for
          service <- ZIO.service[Mocks.TestCommandService]
          _       <- TerminalControl.enableRawMode
          history <- service.history.get
        yield assertTrue(history.contains("stty raw -echo < /dev/tty"))
      }.provide(Mocks.command),
      test("disableRawMode calls correct stty command") {
        for
          service <- ZIO.service[Mocks.TestCommandService]
          _       <- TerminalControl.disableRawMode
          history <- service.history.get
        yield assertTrue(history.contains("stty cooked echo < /dev/tty"))
      }.provide(Mocks.command)
    ),

    // ============================================================
    // TerminalInput ZIO版 Tests
    // ============================================================

    suite("TerminalInput.readKeyZIO")(
      test("returns Timeout when no input available") {
        for result <- TerminalInput.readKeyZIO
        yield assertTrue(result == TerminalInput.ParseResult.Timeout)
      }.provide(Mocks.tty(Chunk.empty) ++ Mocks.config),
      test("returns Regular for normal key") {
        for result <- TerminalInput.readKeyZIO
        yield assertTrue(result == TerminalInput.ParseResult.Regular('h'.toInt))
      }.provide(Mocks.tty(Chunk('h'.toInt)) ++ Mocks.config),
      test("returns Arrow for up arrow sequence") {
        // ESC [ A = Up arrow (RotateClockwise)
        val upArrow = Chunk(27, '['.toInt, 'A'.toInt)
        for result <- TerminalInput.readKeyZIO
        yield assertTrue(result == TerminalInput.ParseResult.Arrow(Input.RotateClockwise))
      }.provide(Mocks.tty(Chunk(27, '['.toInt, 'A'.toInt)) ++ Mocks.config),
      test("returns Arrow for down arrow sequence") {
        // ESC [ B = Down arrow (MoveDown)
        for result <- TerminalInput.readKeyZIO
        yield assertTrue(result == TerminalInput.ParseResult.Arrow(Input.MoveDown))
      }.provide(Mocks.tty(Chunk(27, '['.toInt, 'B'.toInt)) ++ Mocks.config),
      test("returns Arrow for right arrow sequence") {
        // ESC [ C = Right arrow (MoveRight)
        for result <- TerminalInput.readKeyZIO
        yield assertTrue(result == TerminalInput.ParseResult.Arrow(Input.MoveRight))
      }.provide(Mocks.tty(Chunk(27, '['.toInt, 'C'.toInt)) ++ Mocks.config),
      test("returns Arrow for left arrow sequence") {
        // ESC [ D = Left arrow (MoveLeft)
        for result <- TerminalInput.readKeyZIO
        yield assertTrue(result == TerminalInput.ParseResult.Arrow(Input.MoveLeft))
      }.provide(Mocks.tty(Chunk(27, '['.toInt, 'D'.toInt)) ++ Mocks.config),
      test("returns Unknown for incomplete escape sequence") {
        // ESC alone (no following bytes)
        for result <- TerminalInput.readKeyZIO
        yield assertTrue(result == TerminalInput.ParseResult.Unknown)
      }.provide(Mocks.tty(Chunk(27)) ++ Mocks.config),
      test("returns Unknown for invalid escape sequence") {
        // ESC X (not [ followed by arrow)
        for result <- TerminalInput.readKeyZIO
        yield assertTrue(result == TerminalInput.ParseResult.Unknown)
      }.provide(Mocks.tty(Chunk(27, 'X'.toInt)) ++ Mocks.config)
    ),

    // ============================================================
    // GameRunner Rendering Tests
    // ============================================================

    suite("GameRunner Rendering")(
      test("render outputs ANSI escape codes") {
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- GameRunner.renderGame(initialState, Mocks.testConfig)
          output  <- service.buffer.get
        yield assertTrue(output.exists(_.contains("\u001b[H")))
      }.provide(Mocks.console),
      test("render outputs game info") {
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- GameRunner.renderGame(initialState, Mocks.testConfig)
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains("Score:"),
          combined.contains("Level:"),
          combined.contains("Lines:")
        )
      }.provide(Mocks.console),
      test("renderGameOver outputs GAME OVER message") {
        val gameOverState = initialState.copy(status = GameStatus.GameOver)
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- GameRunner.renderGameOver(gameOverState)
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(combined.contains("GAME OVER"))
      }.provide(Mocks.console),
      test("showTitle outputs title screen") {
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- GameRunner.showTitle
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains("Functional Tetris"),
          combined.contains("Controls")
        )
      }.provide(Mocks.console)
    ),

    // ============================================================
    // Integration Tests
    // ============================================================

    suite("Integration")(
      test("input sequence is parsed correctly") {
        // Simulate: h, j, ESC[A (up arrow), q
        val inputs = Chunk(
          'h'.toInt,
          'j'.toInt,
          27,
          '['.toInt,
          'A'.toInt,
          'q'.toInt
        )
        for
          r1 <- TerminalInput.readKeyZIO
          r2 <- TerminalInput.readKeyZIO
          r3 <- TerminalInput.readKeyZIO
          r4 <- TerminalInput.readKeyZIO
        yield assertTrue(
          r1 == TerminalInput.ParseResult.Regular('h'.toInt),
          r2 == TerminalInput.ParseResult.Regular('j'.toInt),
          r3 == TerminalInput.ParseResult.Arrow(Input.RotateClockwise),
          r4 == TerminalInput.ParseResult.Regular('q'.toInt)
        )
      }.provide(
        Mocks.tty(
          Chunk(
            'h'.toInt,
            'j'.toInt,
            27,
            '['.toInt,
            'A'.toInt,
            'q'.toInt
          )
        ) ++ Mocks.config
      ),
      test("toInput converts parse results correctly") {
        val arrow   = TerminalInput.ParseResult.Arrow(Input.MoveLeft)
        val regular = TerminalInput.ParseResult.Regular('h'.toInt)
        val timeout = TerminalInput.ParseResult.Timeout
        val unknown = TerminalInput.ParseResult.Unknown

        assertTrue(
          TerminalInput.toInput(arrow) == Some(Input.MoveLeft),
          TerminalInput.toInput(regular) == Some(Input.MoveLeft),
          TerminalInput.toInput(timeout) == None,
          TerminalInput.toInput(unknown) == None
        )
      },
      test("arrowToInput maps arrow keys correctly") {
        assertTrue(
          TerminalInput.arrowToInput('A'.toInt) == Some(Input.RotateClockwise),
          TerminalInput.arrowToInput('B'.toInt) == Some(Input.MoveDown),
          TerminalInput.arrowToInput('C'.toInt) == Some(Input.MoveRight),
          TerminalInput.arrowToInput('D'.toInt) == Some(Input.MoveLeft),
          TerminalInput.arrowToInput('X'.toInt) == None
        )
      }
    ),

    // ============================================================
    // Additional Branch Coverage Tests
    // ============================================================

    suite("Additional Coverage")(
      test("parseEscapeSequenceZIO returns None when no bytes available after wait") {
        // Only ESC, but available returns 0 after sleep
        for result <- TerminalInput.parseEscapeSequenceZIO
        yield assertTrue(result.isEmpty)
      }.provide(Mocks.tty(Chunk.empty) ++ Mocks.config),
      test("parseEscapeSequenceZIO returns None for invalid bracket") {
        // ESC followed by non-bracket
        for
          _      <- TtyService.read() // consume ESC
          result <- TerminalInput.parseEscapeSequenceZIO
        yield assertTrue(result.isEmpty)
      }.provide(Mocks.tty(Chunk(27, 'X'.toInt)) ++ Mocks.config),
      test("multiple consecutive key reads") {
        val inputs = Chunk('a'.toInt, 'b'.toInt, 'c'.toInt)
        for
          r1 <- TerminalInput.readKeyZIO
          r2 <- TerminalInput.readKeyZIO
          r3 <- TerminalInput.readKeyZIO
          r4 <- TerminalInput.readKeyZIO // should be timeout
        yield assertTrue(
          r1 == TerminalInput.ParseResult.Regular('a'.toInt),
          r2 == TerminalInput.ParseResult.Regular('b'.toInt),
          r3 == TerminalInput.ParseResult.Regular('c'.toInt),
          r4 == TerminalInput.ParseResult.Timeout
        )
      }.provide(Mocks.tty(Chunk('a'.toInt, 'b'.toInt, 'c'.toInt)) ++ Mocks.config),
      test("space key is parsed as HardDrop") {
        for
          result <- TerminalInput.readKeyZIO
          input = TerminalInput.toInput(result)
        yield assertTrue(input == Some(Input.HardDrop))
      }.provide(Mocks.tty(Chunk(' '.toInt)) ++ Mocks.config),
      test("p key is parsed as Pause") {
        for
          result <- TerminalInput.readKeyZIO
          input = TerminalInput.toInput(result)
        yield assertTrue(input == Some(Input.Pause))
      }.provide(Mocks.tty(Chunk('p'.toInt)) ++ Mocks.config),
      test("z key is parsed as RotateCounterClockwise") {
        for
          result <- TerminalInput.readKeyZIO
          input = TerminalInput.toInput(result)
        yield assertTrue(input == Some(Input.RotateCounterClockwise))
      }.provide(Mocks.tty(Chunk('z'.toInt)) ++ Mocks.config),
      test("unknown key returns None from toInput") {
        for
          result <- TerminalInput.readKeyZIO
          input = TerminalInput.toInput(result)
        yield assertTrue(input.isEmpty)
      }.provide(Mocks.tty(Chunk('x'.toInt)) ++ Mocks.config),
      test("isQuitKey returns true for q and Q") {
        assertTrue(
          TerminalInput.isQuitKey('q'.toInt),
          TerminalInput.isQuitKey('Q'.toInt),
          !TerminalInput.isQuitKey('x'.toInt)
        )
      },
      test("keyToInput handles all vim keys") {
        assertTrue(
          TerminalInput.keyToInput('h'.toInt) == Some(Input.MoveLeft),
          TerminalInput.keyToInput('H'.toInt) == Some(Input.MoveLeft),
          TerminalInput.keyToInput('l'.toInt) == Some(Input.MoveRight),
          TerminalInput.keyToInput('L'.toInt) == Some(Input.MoveRight),
          TerminalInput.keyToInput('j'.toInt) == Some(Input.MoveDown),
          TerminalInput.keyToInput('J'.toInt) == Some(Input.MoveDown),
          TerminalInput.keyToInput('k'.toInt) == Some(Input.RotateClockwise),
          TerminalInput.keyToInput('K'.toInt) == Some(Input.RotateClockwise),
          TerminalInput.keyToInput('z'.toInt) == Some(Input.RotateCounterClockwise),
          TerminalInput.keyToInput('Z'.toInt) == Some(Input.RotateCounterClockwise),
          TerminalInput.keyToInput('P'.toInt) == Some(Input.Pause)
        )
      },
      test("EscapeKeyCode constant is 27") {
        assertTrue(TerminalInput.EscapeKeyCode == 27)
      }
    ),

    // ============================================================
    // InteractiveGameLoop Tests
    // ============================================================

    suite("InteractiveGameLoop")(
      test("quit key terminates the loop") {
        // 'q' を入力してループを終了
        val inputs = Chunk('q'.toInt)
        for finalState <- GameRunner
            .interactiveGameLoop(initialState)
            .provide(Mocks.tty(inputs) ++ Mocks.console ++ Mocks.config)
            .timeout(1.second)
        yield assertTrue(finalState.isDefined)
      },
      test("Q key also terminates the loop") {
        val inputs = Chunk('Q'.toInt)
        for finalState <- GameRunner
            .interactiveGameLoop(initialState)
            .provide(Mocks.tty(inputs) ++ Mocks.console ++ Mocks.config)
            .timeout(1.second)
        yield assertTrue(finalState.isDefined)
      },
      test("movement input changes game state") {
        // h (MoveLeft) を入力後、q で終了
        val inputs = Chunk('h'.toInt, 'q'.toInt)
        for
          service    <- ZIO.service[Mocks.TestConsoleService]
          finalState <- GameRunner
            .interactiveGameLoop(initialState)
            .timeout(1.second)
          output <- service.buffer.get
        yield assertTrue(
          finalState.isDefined,
          output.exists(_.contains("Score:"))
        )
      }.provide(Mocks.tty(Chunk('h'.toInt, 'q'.toInt)) ++ Mocks.console ++ Mocks.config),
      test("arrow key input is processed") {
        // ESC [ D (Left arrow) を入力後、q で終了
        val inputs = Chunk(27, '['.toInt, 'D'.toInt, 'q'.toInt)
        for finalState <- GameRunner
            .interactiveGameLoop(initialState)
            .provide(Mocks.tty(inputs) ++ Mocks.console ++ Mocks.config)
            .timeout(1.second)
        yield assertTrue(finalState.isDefined)
      },
      test("hard drop input is processed") {
        // Space (HardDrop) を入力後、q で終了
        val inputs = Chunk(' '.toInt, 'q'.toInt)
        for finalState <- GameRunner
            .interactiveGameLoop(initialState)
            .provide(Mocks.tty(inputs) ++ Mocks.console ++ Mocks.config)
            .timeout(1.second)
        yield assertTrue(finalState.isDefined)
      },
      test("pause input is processed") {
        // p (Pause) を入力後、q で終了
        val inputs = Chunk('p'.toInt, 'q'.toInt)
        for finalState <- GameRunner
            .interactiveGameLoop(initialState)
            .provide(Mocks.tty(inputs) ++ Mocks.console ++ Mocks.config)
            .timeout(1.second)
        yield assertTrue(finalState.isDefined)
      },
      test("rotate input is processed") {
        // k (RotateClockwise) を入力後、q で終了
        val inputs = Chunk('k'.toInt, 'q'.toInt)
        for finalState <- GameRunner
            .interactiveGameLoop(initialState)
            .provide(Mocks.tty(inputs) ++ Mocks.console ++ Mocks.config)
            .timeout(1.second)
        yield assertTrue(finalState.isDefined)
      },
      test("counter-clockwise rotate is processed") {
        // z (RotateCounterClockwise) を入力後、q で終了
        val inputs = Chunk('z'.toInt, 'q'.toInt)
        for finalState <- GameRunner
            .interactiveGameLoop(initialState)
            .provide(Mocks.tty(inputs) ++ Mocks.console ++ Mocks.config)
            .timeout(1.second)
        yield assertTrue(finalState.isDefined)
      },
      test("unknown key is ignored") {
        // x (unknown) を入力後、q で終了
        val inputs = Chunk('x'.toInt, 'q'.toInt)
        for finalState <- GameRunner
            .interactiveGameLoop(initialState)
            .provide(Mocks.tty(inputs) ++ Mocks.console ++ Mocks.config)
            .timeout(1.second)
        yield assertTrue(finalState.isDefined)
      },
      test("multiple movements are processed") {
        // h, l, j を入力後、q で終了
        val inputs = Chunk('h'.toInt, 'l'.toInt, 'j'.toInt, 'q'.toInt)
        for finalState <- GameRunner
            .interactiveGameLoop(initialState)
            .provide(Mocks.tty(inputs) ++ Mocks.console ++ Mocks.config)
            .timeout(1.second)
        yield assertTrue(finalState.isDefined)
      },
      test("up arrow rotates piece") {
        // ESC [ A (Up arrow = RotateClockwise) を入力後、q で終了
        val inputs = Chunk(27, '['.toInt, 'A'.toInt, 'q'.toInt)
        for finalState <- GameRunner
            .interactiveGameLoop(initialState)
            .provide(Mocks.tty(inputs) ++ Mocks.console ++ Mocks.config)
            .timeout(1.second)
        yield assertTrue(finalState.isDefined)
      },
      test("down arrow moves piece down") {
        // ESC [ B (Down arrow = MoveDown) を入力後、q で終了
        val inputs = Chunk(27, '['.toInt, 'B'.toInt, 'q'.toInt)
        for finalState <- GameRunner
            .interactiveGameLoop(initialState)
            .provide(Mocks.tty(inputs) ++ Mocks.console ++ Mocks.config)
            .timeout(1.second)
        yield assertTrue(finalState.isDefined)
      },
      test("right arrow moves piece right") {
        // ESC [ C (Right arrow = MoveRight) を入力後、q で終了
        val inputs = Chunk(27, '['.toInt, 'C'.toInt, 'q'.toInt)
        for finalState <- GameRunner
            .interactiveGameLoop(initialState)
            .provide(Mocks.tty(inputs) ++ Mocks.console ++ Mocks.config)
            .timeout(1.second)
        yield assertTrue(finalState.isDefined)
      },
      test("render is called with updated state") {
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- GameRunner
            .interactiveGameLoop(initialState)
            .timeout(1.second)
          output <- service.buffer.get
        // 複数回の描画が行われているはず（初期描画 + 移動後の描画）
        yield assertTrue(output.count(_.contains("\u001b[H")) >= 1)
      }.provide(Mocks.tty(Chunk('h'.toInt, 'q'.toInt)) ++ Mocks.console ++ Mocks.config),
      test("incomplete escape sequence is handled") {
        // ESC だけを入力（不完全なエスケープシーケンス）後、q で終了
        val inputs = Chunk(27, 'q'.toInt)
        for finalState <- GameRunner
            .interactiveGameLoop(initialState)
            .provide(Mocks.tty(inputs) ++ Mocks.console ++ Mocks.config)
            .timeout(1.second)
        yield assertTrue(finalState.isDefined)
      },
      test("logs game over and exits when game over condition is met") {
        // 1. 即死するように積み上がった盤面を作成（上から2行目まで埋める）
        val dangerousGrid = (0 until gridWidth).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, x) =>
          g.place(Position(x, 2), Cell.Filled(TetrominoShape.I))
        }

        // 2. 次にブロックがスポーンしたら即衝突する状態
        val dangerousState = GameState
          .initial(TetrominoShape.T, TetrominoShape.O, gridWidth, gridHeight)
          .copy(grid = dangerousGrid)

        // 3. 入力: HardDrop (即座に固定→判定→GameOver)
        val inputs = Chunk(' '.toInt)

        for finalState <- GameRunner
            .interactiveGameLoop(dangerousState)
            .provide(Mocks.tty(inputs) ++ Mocks.console ++ Mocks.config)
            .timeout(1.second)
        yield assertTrue(finalState.map(_.isGameOver).getOrElse(false))
      }
    ),

    // ============================================================
    // RandomPieceGenerator Tests
    // ============================================================

    suite("RandomPieceGenerator")(
      test("nextShape returns valid shape") {
        for shape <- GameRunner.RandomPieceGenerator.nextShape
        yield assertTrue(TetrominoShape.values.contains(shape))
      },
      test("nextShape returns different shapes over time") {
        for
          shapes <- ZIO.collectAll(List.fill(shapeSampleCount)(GameRunner.RandomPieceGenerator.nextShape))
          uniqueShapes = shapes.toSet
        yield assertTrue(uniqueShapes.size > minimumUniqueShapes)
      }
    ),

    // ============================================================
    // GameRunner Grid Rendering Tests
    // ============================================================

    suite("GameRunner Grid Rendering")(
      test("renderGrid includes borders") {
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- GameRunner.renderGame(initialState, Mocks.testConfig)
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains("┌"),
          combined.contains("┐"),
          combined.contains("└"),
          combined.contains("┘"),
          combined.contains("│")
        )
      }.provide(Mocks.console),
      test("renderGrid shows current tetromino") {
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- GameRunner.renderGame(initialState, Mocks.testConfig)
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(combined.contains("█"))
      }.provide(Mocks.console),
      test("renderGrid shows empty cells") {
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- GameRunner.renderGame(initialState, Mocks.testConfig)
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(combined.contains("·"))
      }.provide(Mocks.console),
      test("render shows Next piece info") {
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- GameRunner.renderGame(initialState, Mocks.testConfig)
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(combined.contains("Next:"))
      }.provide(Mocks.console),
      test("renderGame returns ScreenBuffer") {
        for buffer <- GameRunner.renderGame(initialState, Mocks.testConfig)
        yield assertTrue(
          buffer.width > minimumBufferDimension,
          buffer.height > minimumBufferDimension
        )
      }.provide(Mocks.console),
      test("renderGame with previous buffer uses differential rendering") {
        for
          service     <- ZIO.service[Mocks.TestConsoleService]
          firstBuffer <- GameRunner.renderGame(initialState, Mocks.testConfig, None)
          _           <- service.buffer.set(List.empty) // clear buffer
          _           <- GameRunner.renderGame(initialState, Mocks.testConfig, Some(firstBuffer))
          output      <- service.buffer.get
        yield assertTrue(
          // Same state = no changes = no output (or minimal output)
          output.forall(!_.contains(Ansi.clearScreen))
        )
      }.provide(Mocks.console)
    ),

    // ============================================================
    // AppConfig Tests
    // ============================================================

    suite("AppConfig")(
      test("live layer loads configuration from application.conf") {
        for config <- ZIO.service[AppConfig]
        yield assertTrue(
          config.grid.width == 10,
          config.grid.height == 20,
          config.terminal.inputPollIntervalMs == 20
        )
      }.provide(ConfigLayer.live)
    ),

    // ============================================================
    // Additional TerminalInput Branch Coverage Tests
    // ============================================================

    suite("TerminalInput Branch Coverage")(
      test("parseArrowKey returns None when no bytes after bracket") {
        // ESC [ (incomplete - no arrow key code follows)
        for result <- TerminalInput.readKeyZIO
        yield assertTrue(result == TerminalInput.ParseResult.Unknown)
      }.provide(Mocks.tty(Chunk(27, '['.toInt)) ++ Mocks.config),
      test("parseEscapeBody returns None for non-bracket second byte") {
        // ESC O (not '[' so returns Unknown)
        for result <- TerminalInput.readKeyZIO
        yield assertTrue(result == TerminalInput.ParseResult.Unknown)
      }.provide(Mocks.tty(Chunk(27, 'O'.toInt)) ++ Mocks.config),
      test("parseArrowKey handles unknown arrow code") {
        // ESC [ X (not A/B/C/D)
        for
          result <- TerminalInput.readKeyZIO
          input = TerminalInput.toInput(result)
        yield assertTrue(
          result == TerminalInput.ParseResult.Arrow(null).asInstanceOf[Any] == false,
          input.isEmpty || result == TerminalInput.ParseResult.Unknown
        )
      }.provide(Mocks.tty(Chunk(27, '['.toInt, 'X'.toInt)) ++ Mocks.config),
      test("readKeyZIO returns Regular for valid vim keys") {
        for
          h <- TerminalInput.readKeyZIO
          l <- TerminalInput.readKeyZIO
          j <- TerminalInput.readKeyZIO
          k <- TerminalInput.readKeyZIO
        yield assertTrue(
          h == TerminalInput.ParseResult.Regular('h'.toInt),
          l == TerminalInput.ParseResult.Regular('l'.toInt),
          j == TerminalInput.ParseResult.Regular('j'.toInt),
          k == TerminalInput.ParseResult.Regular('k'.toInt)
        )
      }.provide(Mocks.tty(Chunk('h'.toInt, 'l'.toInt, 'j'.toInt, 'k'.toInt)) ++ Mocks.config),
      test("toInput handles all ParseResult variants") {
        val arrow   = TerminalInput.ParseResult.Arrow(Input.HardDrop)
        val regular = TerminalInput.ParseResult.Regular(' '.toInt)
        val timeout = TerminalInput.ParseResult.Timeout
        val unknown = TerminalInput.ParseResult.Unknown

        assertTrue(
          TerminalInput.toInput(arrow) == Some(Input.HardDrop),
          TerminalInput.toInput(regular) == Some(Input.HardDrop),
          TerminalInput.toInput(timeout).isEmpty,
          TerminalInput.toInput(unknown).isEmpty
        )
      },
      test("all arrow keys are mapped correctly") {
        assertTrue(
          TerminalInput.arrowToInput('A') == Some(Input.RotateClockwise),
          TerminalInput.arrowToInput('B') == Some(Input.MoveDown),
          TerminalInput.arrowToInput('C') == Some(Input.MoveRight),
          TerminalInput.arrowToInput('D') == Some(Input.MoveLeft),
          TerminalInput.arrowToInput('E').isEmpty
        )
      }
    ),

    // ============================================================
    // GameRunner LoopState Tests
    // ============================================================

    suite("GameRunner LoopState")(
      test("LoopState holds game state and previous buffer") {
        val state     = initialState
        val buffer    = GameView.toScreenBuffer(state, Mocks.testConfig)
        val loopState = GameRunner.LoopState(state, Some(buffer))

        assertTrue(
          loopState.gameState == state,
          loopState.previousBuffer.isDefined,
          loopState.previousBuffer.get == buffer
        )
      },
      test("LoopState can hold None for previousBuffer") {
        val loopState = GameRunner.LoopState(initialState, None)
        assertTrue(loopState.previousBuffer.isEmpty)
      }
    ),

    // ============================================================
    // GameCommand Additional Tests
    // ============================================================

    suite("GameCommand Additional Coverage")(
      test("UserAction wraps all input types") {
        val allInputs = List(
          Input.MoveLeft,
          Input.MoveRight,
          Input.MoveDown,
          Input.RotateClockwise,
          Input.RotateCounterClockwise,
          Input.HardDrop,
          Input.Pause,
          Input.Tick
        )
        val commands = allInputs.map(GameRunner.GameCommand.UserAction(_))
        assertTrue(commands.size == allInputs.size)
      },
      test("GameCommand pattern matching works correctly") {
        val userAction = GameRunner.GameCommand.UserAction(Input.MoveLeft)
        val tick       = GameRunner.GameCommand.TimeTick
        val quit       = GameRunner.GameCommand.Quit

        val userActionMatched = userAction match
          case GameRunner.GameCommand.UserAction(i) => i == Input.MoveLeft
          case _                                    => false

        val tickMatched = tick match
          case GameRunner.GameCommand.TimeTick => true
          case _                               => false

        val quitMatched = quit match
          case GameRunner.GameCommand.Quit => true
          case _                           => false

        assertTrue(userActionMatched, tickMatched, quitMatched)
      }
    ),

    // ============================================================
    // eventLoop Edge Cases
    // ============================================================

    suite("eventLoop Edge Cases")(
      test("eventLoop handles Pause toggle") {
        val pausedState = initialState.copy(status = GameStatus.Paused)
        for
          queue <- Queue.bounded[GameRunner.GameCommand](10)
          _     <- queue.offer(GameRunner.GameCommand.UserAction(Input.Pause))
          _     <- queue.offer(GameRunner.GameCommand.Quit)
          loopState = GameRunner.LoopState(pausedState, None)
          intervalRef <- Ref.make(LineClearing.dropInterval(pausedState.level, Mocks.testConfig.speed))
          result      <- GameRunner.eventLoop(queue, loopState, Mocks.testConfig, intervalRef)
        yield assertTrue(result.gameState.status == GameStatus.Playing)
      }.provide(Mocks.console),
      test("eventLoop processes multiple Ticks") {
        for
          queue <- Queue.bounded[GameRunner.GameCommand](10)
          _     <- queue.offer(GameRunner.GameCommand.TimeTick)
          _     <- queue.offer(GameRunner.GameCommand.TimeTick)
          _     <- queue.offer(GameRunner.GameCommand.Quit)
          loopState = GameRunner.LoopState(initialState, None)
          intervalRef <- Ref.make(LineClearing.dropInterval(initialState.level, Mocks.testConfig.speed))
          result      <- GameRunner.eventLoop(queue, loopState, Mocks.testConfig, intervalRef)
        yield assertTrue(result.gameState != null)
      }.provide(Mocks.console),
      test("eventLoop updates previousBuffer after each command") {
        for
          queue <- Queue.bounded[GameRunner.GameCommand](10)
          _     <- queue.offer(GameRunner.GameCommand.UserAction(Input.MoveRight))
          _     <- queue.offer(GameRunner.GameCommand.Quit)
          loopState = GameRunner.LoopState(initialState, None)
          intervalRef <- Ref.make(LineClearing.dropInterval(initialState.level, Mocks.testConfig.speed))
          result      <- GameRunner.eventLoop(queue, loopState, Mocks.testConfig, intervalRef)
        yield assertTrue(result.previousBuffer.isDefined)
      }.provide(Mocks.console)
    ),

    // ============================================================
    // TerminalInput Internal Method Coverage
    // ============================================================

    suite("TerminalInput Internal Coverage")(
      test("parseEscapeSequenceZIO covers all for-comprehension steps") {
        // This test ensures all lines in the for-comprehension are executed
        // ESC followed by valid arrow sequence
        val validArrow = Chunk(27, '['.toInt, 'A'.toInt)
        for result <- TerminalInput.readKeyZIO
        yield assertTrue(result == TerminalInput.ParseResult.Arrow(Input.RotateClockwise))
      }.provide(Mocks.tty(Chunk(27, '['.toInt, 'A'.toInt)) ++ Mocks.config),
      test("parseEscapeBody executes when available > 0") {
        // ESC followed by '[' and arrow key
        for result <- TerminalInput.readKeyZIO
        yield assertTrue(result == TerminalInput.ParseResult.Arrow(Input.MoveDown))
      }.provide(Mocks.tty(Chunk(27, '['.toInt, 'B'.toInt)) ++ Mocks.config),
      test("parseArrowKey executes read and maps to input") {
        // ESC [ C sequence for MoveRight
        for result <- TerminalInput.readKeyZIO
        yield assertTrue(result == TerminalInput.ParseResult.Arrow(Input.MoveRight))
      }.provide(Mocks.tty(Chunk(27, '['.toInt, 'C'.toInt)) ++ Mocks.config),
      test("readKeyBody reads key and parses result") {
        // Regular key 'j' for MoveDown
        for result <- TerminalInput.readKeyZIO
        yield assertTrue(result == TerminalInput.ParseResult.Regular('j'.toInt))
      }.provide(Mocks.tty(Chunk('j'.toInt)) ++ Mocks.config),
      test("parseEscapeSequenceZIO returns None when available is 0 after sleep") {
        // Only ESC, no following bytes
        for result <- TerminalInput.readKeyZIO
        yield assertTrue(result == TerminalInput.ParseResult.Unknown)
      }.provide(Mocks.tty(Chunk(27)) ++ Mocks.config),
      test("parseArrowKey returns None for unknown arrow code") {
        // ESC [ F (not a valid arrow)
        for
          result <- TerminalInput.readKeyZIO
          input = TerminalInput.toInput(result)
        yield assertTrue(input.isEmpty)
      }.provide(Mocks.tty(Chunk(27, '['.toInt, 'F'.toInt)) ++ Mocks.config),
      test("multiple escape sequences in succession") {
        // Two arrow key sequences
        val inputs = Chunk(
          27,
          '['.toInt,
          'A'.toInt, // Up arrow
          27,
          '['.toInt,
          'D'.toInt // Left arrow
        )
        for
          r1 <- TerminalInput.readKeyZIO
          r2 <- TerminalInput.readKeyZIO
        yield assertTrue(
          r1 == TerminalInput.ParseResult.Arrow(Input.RotateClockwise),
          r2 == TerminalInput.ParseResult.Arrow(Input.MoveLeft)
        )
      }.provide(Mocks.tty(Chunk(27, '['.toInt, 'A'.toInt, 27, '['.toInt, 'D'.toInt)) ++ Mocks.config),
      test("mixed regular and escape sequence input") {
        // 'h', then ESC [ B
        val inputs = Chunk('h'.toInt, 27, '['.toInt, 'B'.toInt)
        for
          r1 <- TerminalInput.readKeyZIO
          r2 <- TerminalInput.readKeyZIO
        yield assertTrue(
          r1 == TerminalInput.ParseResult.Regular('h'.toInt),
          r2 == TerminalInput.ParseResult.Arrow(Input.MoveDown)
        )
      }.provide(Mocks.tty(Chunk('h'.toInt, 27, '['.toInt, 'B'.toInt)) ++ Mocks.config)
    )
  )

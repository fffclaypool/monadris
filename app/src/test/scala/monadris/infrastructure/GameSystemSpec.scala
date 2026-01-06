package monadris.infrastructure

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import monadris.config.ConfigLayer
import monadris.domain.Input
import monadris.domain.config.AppConfig
import monadris.domain.model.game.TetrisGame
import monadris.infrastructure.TestServices as Mocks

/**
 * IO抽象化レイヤーのテスト
 * TestServices のモック実装を使用
 *
 * Note: withLiveClock is needed because InputLoop uses ZIO.sleep
 * which requires real clock advancement in tests.
 */
object GameSystemSpec extends ZIOSpecDefault:

  val gridWidth: Int  = Mocks.testConfig.grid.width
  val gridHeight: Int = Mocks.testConfig.grid.height

  // ============================================================
  // Test constants
  // ============================================================

  private val testSeed = 42L

  def initialGame: TetrisGame =
    TetrisGame.create(testSeed, gridWidth, gridHeight, Mocks.testConfig.score, Mocks.testConfig.level)

  def spec = suite("GameSystem Tests")(
    // ============================================================
    // Terminal Tests
    // ============================================================

    suite("Terminal")(
      test("test implementation returns queued input") {
        val inputs = Chunk('h'.toInt, 'j'.toInt, 'k'.toInt)
        for
          first  <- Terminal.read
          second <- Terminal.read
          third  <- Terminal.read
        yield assertTrue(
          first == 'h'.toInt,
          second == 'j'.toInt,
          third == 'k'.toInt
        )
      }.provide(Mocks.terminal(Chunk('h'.toInt, 'j'.toInt, 'k'.toInt))),
      test("available returns queue size") {
        val inputs = Chunk(1, 2, 3)
        for
          size1 <- Terminal.available
          _     <- Terminal.read
          size2 <- Terminal.available
        yield assertTrue(size1 == 3, size2 == 2)
      }.provide(Mocks.terminal(Chunk(1, 2, 3)))
    ),

    // ============================================================
    // InputLoop ZIO版 Tests
    // ============================================================

    suite("InputLoop.readKeyZIO")(
      test("returns Timeout when no input available") {
        for result <- InputLoop.readKeyZIO
        yield assertTrue(result == InputLoop.ParseResult.Timeout)
      }.provide(Mocks.terminal(Chunk.empty) ++ Mocks.config),
      test("returns Regular for normal key") {
        for result <- InputLoop.readKeyZIO
        yield assertTrue(result == InputLoop.ParseResult.Regular('h'.toInt))
      }.provide(Mocks.terminal(Chunk('h'.toInt)) ++ Mocks.config),
      test("returns Arrow for up arrow sequence") {
        // ESC [ A = Up arrow (RotateClockwise)
        val upArrow = Chunk(27, '['.toInt, 'A'.toInt)
        for result <- InputLoop.readKeyZIO
        yield assertTrue(result == InputLoop.ParseResult.Arrow(Input.RotateClockwise))
      }.provide(Mocks.terminal(Chunk(27, '['.toInt, 'A'.toInt)) ++ Mocks.config),
      test("returns Arrow for down arrow sequence") {
        // ESC [ B = Down arrow (MoveDown)
        for result <- InputLoop.readKeyZIO
        yield assertTrue(result == InputLoop.ParseResult.Arrow(Input.MoveDown))
      }.provide(Mocks.terminal(Chunk(27, '['.toInt, 'B'.toInt)) ++ Mocks.config),
      test("returns Arrow for right arrow sequence") {
        // ESC [ C = Right arrow (MoveRight)
        for result <- InputLoop.readKeyZIO
        yield assertTrue(result == InputLoop.ParseResult.Arrow(Input.MoveRight))
      }.provide(Mocks.terminal(Chunk(27, '['.toInt, 'C'.toInt)) ++ Mocks.config),
      test("returns Arrow for left arrow sequence") {
        // ESC [ D = Left arrow (MoveLeft)
        for result <- InputLoop.readKeyZIO
        yield assertTrue(result == InputLoop.ParseResult.Arrow(Input.MoveLeft))
      }.provide(Mocks.terminal(Chunk(27, '['.toInt, 'D'.toInt)) ++ Mocks.config),
      test("returns Unknown for incomplete escape sequence") {
        // ESC alone (no following bytes)
        for result <- InputLoop.readKeyZIO
        yield assertTrue(result == InputLoop.ParseResult.Unknown)
      }.provide(Mocks.terminal(Chunk(27)) ++ Mocks.config),
      test("returns Unknown for invalid escape sequence") {
        // ESC X (not [ followed by arrow)
        for result <- InputLoop.readKeyZIO
        yield assertTrue(result == InputLoop.ParseResult.Unknown)
      }.provide(Mocks.terminal(Chunk(27, 'X'.toInt)) ++ Mocks.config)
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
          r1 <- InputLoop.readKeyZIO
          r2 <- InputLoop.readKeyZIO
          r3 <- InputLoop.readKeyZIO
          r4 <- InputLoop.readKeyZIO
        yield assertTrue(
          r1 == InputLoop.ParseResult.Regular('h'.toInt),
          r2 == InputLoop.ParseResult.Regular('j'.toInt),
          r3 == InputLoop.ParseResult.Arrow(Input.RotateClockwise),
          r4 == InputLoop.ParseResult.Regular('q'.toInt)
        )
      }.provide(
        Mocks.terminal(
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
        val arrow   = InputLoop.ParseResult.Arrow(Input.MoveLeft)
        val regular = InputLoop.ParseResult.Regular('h'.toInt)
        val timeout = InputLoop.ParseResult.Timeout
        val unknown = InputLoop.ParseResult.Unknown

        assertTrue(
          InputLoop.toInput(arrow) == Some(Input.MoveLeft),
          InputLoop.toInput(regular) == Some(Input.MoveLeft),
          InputLoop.toInput(timeout) == None,
          InputLoop.toInput(unknown) == None
        )
      },
      test("arrowToInput maps arrow keys correctly") {
        assertTrue(
          InputLoop.arrowToInput('A'.toInt) == Some(Input.RotateClockwise),
          InputLoop.arrowToInput('B'.toInt) == Some(Input.MoveDown),
          InputLoop.arrowToInput('C'.toInt) == Some(Input.MoveRight),
          InputLoop.arrowToInput('D'.toInt) == Some(Input.MoveLeft),
          InputLoop.arrowToInput('X'.toInt) == None
        )
      }
    ),

    // ============================================================
    // Additional Branch Coverage Tests
    // ============================================================

    suite("Additional Coverage")(
      test("parseEscapeSequenceZIO returns None when no bytes available after wait") {
        // Only ESC, but available returns 0 after sleep
        for result <- InputLoop.parseEscapeSequenceZIO
        yield assertTrue(result.isEmpty)
      }.provide(Mocks.terminal(Chunk.empty) ++ Mocks.config),
      test("parseEscapeSequenceZIO returns None for invalid bracket") {
        // ESC followed by non-bracket
        for
          _      <- Terminal.read // consume ESC
          result <- InputLoop.parseEscapeSequenceZIO
        yield assertTrue(result.isEmpty)
      }.provide(Mocks.terminal(Chunk(27, 'X'.toInt)) ++ Mocks.config),
      test("multiple consecutive key reads") {
        val inputs = Chunk('a'.toInt, 'b'.toInt, 'c'.toInt)
        for
          r1 <- InputLoop.readKeyZIO
          r2 <- InputLoop.readKeyZIO
          r3 <- InputLoop.readKeyZIO
          r4 <- InputLoop.readKeyZIO // should be timeout
        yield assertTrue(
          r1 == InputLoop.ParseResult.Regular('a'.toInt),
          r2 == InputLoop.ParseResult.Regular('b'.toInt),
          r3 == InputLoop.ParseResult.Regular('c'.toInt),
          r4 == InputLoop.ParseResult.Timeout
        )
      }.provide(Mocks.terminal(Chunk('a'.toInt, 'b'.toInt, 'c'.toInt)) ++ Mocks.config),
      test("space key is parsed as HardDrop") {
        for
          result <- InputLoop.readKeyZIO
          input = InputLoop.toInput(result)
        yield assertTrue(input == Some(Input.HardDrop))
      }.provide(Mocks.terminal(Chunk(' '.toInt)) ++ Mocks.config),
      test("p key is parsed as Pause") {
        for
          result <- InputLoop.readKeyZIO
          input = InputLoop.toInput(result)
        yield assertTrue(input == Some(Input.Pause))
      }.provide(Mocks.terminal(Chunk('p'.toInt)) ++ Mocks.config),
      test("z key is parsed as RotateCounterClockwise") {
        for
          result <- InputLoop.readKeyZIO
          input = InputLoop.toInput(result)
        yield assertTrue(input == Some(Input.RotateCounterClockwise))
      }.provide(Mocks.terminal(Chunk('z'.toInt)) ++ Mocks.config),
      test("unknown key returns None from toInput") {
        for
          result <- InputLoop.readKeyZIO
          input = InputLoop.toInput(result)
        yield assertTrue(input.isEmpty)
      }.provide(Mocks.terminal(Chunk('x'.toInt)) ++ Mocks.config),
      test("isQuitKey returns true for q and Q") {
        assertTrue(
          InputLoop.isQuitKey('q'.toInt),
          InputLoop.isQuitKey('Q'.toInt),
          !InputLoop.isQuitKey('x'.toInt)
        )
      },
      test("keyToInput handles all vim keys") {
        assertTrue(
          InputLoop.keyToInput('h'.toInt) == Some(Input.MoveLeft),
          InputLoop.keyToInput('H'.toInt) == Some(Input.MoveLeft),
          InputLoop.keyToInput('l'.toInt) == Some(Input.MoveRight),
          InputLoop.keyToInput('L'.toInt) == Some(Input.MoveRight),
          InputLoop.keyToInput('j'.toInt) == Some(Input.MoveDown),
          InputLoop.keyToInput('J'.toInt) == Some(Input.MoveDown),
          InputLoop.keyToInput('k'.toInt) == Some(Input.RotateClockwise),
          InputLoop.keyToInput('K'.toInt) == Some(Input.RotateClockwise),
          InputLoop.keyToInput('z'.toInt) == Some(Input.RotateCounterClockwise),
          InputLoop.keyToInput('Z'.toInt) == Some(Input.RotateCounterClockwise),
          InputLoop.keyToInput('P'.toInt) == Some(Input.Pause)
        )
      },
      test("EscapeKeyCode constant is 27") {
        assertTrue(InputLoop.EscapeKeyCode == 27)
      }
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
    // InputLoop Branch Coverage Tests
    // ============================================================

    suite("InputLoop Branch Coverage")(
      test("parseArrowKey returns None when no bytes after bracket") {
        // ESC [ (incomplete - no arrow key code follows)
        for result <- InputLoop.readKeyZIO
        yield assertTrue(result == InputLoop.ParseResult.Unknown)
      }.provide(Mocks.terminal(Chunk(27, '['.toInt)) ++ Mocks.config),
      test("parseEscapeBody returns None for non-bracket second byte") {
        // ESC O (not '[' so returns Unknown)
        for result <- InputLoop.readKeyZIO
        yield assertTrue(result == InputLoop.ParseResult.Unknown)
      }.provide(Mocks.terminal(Chunk(27, 'O'.toInt)) ++ Mocks.config),
      test("parseArrowKey handles unknown arrow code") {
        // ESC [ X (not A/B/C/D)
        for
          result <- InputLoop.readKeyZIO
          input = InputLoop.toInput(result)
        yield assertTrue(
          result == InputLoop.ParseResult.Arrow(null).asInstanceOf[Any] == false,
          input.isEmpty || result == InputLoop.ParseResult.Unknown
        )
      }.provide(Mocks.terminal(Chunk(27, '['.toInt, 'X'.toInt)) ++ Mocks.config),
      test("readKeyZIO returns Regular for valid vim keys") {
        for
          h <- InputLoop.readKeyZIO
          l <- InputLoop.readKeyZIO
          j <- InputLoop.readKeyZIO
          k <- InputLoop.readKeyZIO
        yield assertTrue(
          h == InputLoop.ParseResult.Regular('h'.toInt),
          l == InputLoop.ParseResult.Regular('l'.toInt),
          j == InputLoop.ParseResult.Regular('j'.toInt),
          k == InputLoop.ParseResult.Regular('k'.toInt)
        )
      }.provide(Mocks.terminal(Chunk('h'.toInt, 'l'.toInt, 'j'.toInt, 'k'.toInt)) ++ Mocks.config),
      test("toInput handles all ParseResult variants") {
        val arrow   = InputLoop.ParseResult.Arrow(Input.HardDrop)
        val regular = InputLoop.ParseResult.Regular(' '.toInt)
        val timeout = InputLoop.ParseResult.Timeout
        val unknown = InputLoop.ParseResult.Unknown

        assertTrue(
          InputLoop.toInput(arrow) == Some(Input.HardDrop),
          InputLoop.toInput(regular) == Some(Input.HardDrop),
          InputLoop.toInput(timeout).isEmpty,
          InputLoop.toInput(unknown).isEmpty
        )
      },
      test("all arrow keys are mapped correctly") {
        assertTrue(
          InputLoop.arrowToInput('A') == Some(Input.RotateClockwise),
          InputLoop.arrowToInput('B') == Some(Input.MoveDown),
          InputLoop.arrowToInput('C') == Some(Input.MoveRight),
          InputLoop.arrowToInput('D') == Some(Input.MoveLeft),
          InputLoop.arrowToInput('E').isEmpty
        )
      }
    ),

    // ============================================================
    // InputLoop Internal Coverage
    // ============================================================

    suite("InputLoop Internal Coverage")(
      test("parseEscapeSequenceZIO covers all for-comprehension steps") {
        // This test ensures all lines in the for-comprehension are executed
        // ESC followed by valid arrow sequence
        val validArrow = Chunk(27, '['.toInt, 'A'.toInt)
        for result <- InputLoop.readKeyZIO
        yield assertTrue(result == InputLoop.ParseResult.Arrow(Input.RotateClockwise))
      }.provide(Mocks.terminal(Chunk(27, '['.toInt, 'A'.toInt)) ++ Mocks.config),
      test("parseEscapeBody executes when available > 0") {
        // ESC followed by '[' and arrow key
        for result <- InputLoop.readKeyZIO
        yield assertTrue(result == InputLoop.ParseResult.Arrow(Input.MoveDown))
      }.provide(Mocks.terminal(Chunk(27, '['.toInt, 'B'.toInt)) ++ Mocks.config),
      test("parseArrowKey executes read and maps to input") {
        // ESC [ C sequence for MoveRight
        for result <- InputLoop.readKeyZIO
        yield assertTrue(result == InputLoop.ParseResult.Arrow(Input.MoveRight))
      }.provide(Mocks.terminal(Chunk(27, '['.toInt, 'C'.toInt)) ++ Mocks.config),
      test("readKeyBody reads key and parses result") {
        // Regular key 'j' for MoveDown
        for result <- InputLoop.readKeyZIO
        yield assertTrue(result == InputLoop.ParseResult.Regular('j'.toInt))
      }.provide(Mocks.terminal(Chunk('j'.toInt)) ++ Mocks.config),
      test("parseEscapeSequenceZIO returns None when available is 0 after sleep") {
        // Only ESC, no following bytes
        for result <- InputLoop.readKeyZIO
        yield assertTrue(result == InputLoop.ParseResult.Unknown)
      }.provide(Mocks.terminal(Chunk(27)) ++ Mocks.config),
      test("parseArrowKey returns None for unknown arrow code") {
        // ESC [ F (not a valid arrow)
        for
          result <- InputLoop.readKeyZIO
          input = InputLoop.toInput(result)
        yield assertTrue(input.isEmpty)
      }.provide(Mocks.terminal(Chunk(27, '['.toInt, 'F'.toInt)) ++ Mocks.config),
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
          r1 <- InputLoop.readKeyZIO
          r2 <- InputLoop.readKeyZIO
        yield assertTrue(
          r1 == InputLoop.ParseResult.Arrow(Input.RotateClockwise),
          r2 == InputLoop.ParseResult.Arrow(Input.MoveLeft)
        )
      }.provide(Mocks.terminal(Chunk(27, '['.toInt, 'A'.toInt, 27, '['.toInt, 'D'.toInt)) ++ Mocks.config),
      test("mixed regular and escape sequence input") {
        // 'h', then ESC [ B
        val inputs = Chunk('h'.toInt, 27, '['.toInt, 'B'.toInt)
        for
          r1 <- InputLoop.readKeyZIO
          r2 <- InputLoop.readKeyZIO
        yield assertTrue(
          r1 == InputLoop.ParseResult.Regular('h'.toInt),
          r2 == InputLoop.ParseResult.Arrow(Input.MoveDown)
        )
      }.provide(Mocks.terminal(Chunk('h'.toInt, 27, '['.toInt, 'B'.toInt)) ++ Mocks.config)
    )
  ) @@ withLiveClock

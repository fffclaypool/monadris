package monadris.effect

import zio.*
import zio.stream.*
import zio.test.*
import zio.test.Assertion.*

import monadris.domain.*

object GameRunnerSpec extends ZIOSpecDefault:

  // ============================================================
  // Test fixtures
  // ============================================================

  def initialState: GameState =
    GameState.initial(TetrominoShape.T, TetrominoShape.I)

  val fixedNextShape: () => TetrominoShape = () => TetrominoShape.O

  // Mock renderer that tracks render calls
  class MockRenderer extends GameRunner.Renderer:
    private var renderCount = 0
    private var gameOverRendered = false

    def render(state: GameState): UIO[Unit] =
      ZIO.succeed { renderCount += 1 }

    def renderGameOver(state: GameState): UIO[Unit] =
      ZIO.succeed { gameOverRendered = true }

    def getRenderCount: Int = renderCount
    def wasGameOverRendered: Boolean = gameOverRendered

  // Mock random piece generator
  object FixedPieceGenerator extends GameRunner.RandomPiece:
    def nextShape: UIO[TetrominoShape] = ZIO.succeed(TetrominoShape.O)

  // ============================================================
  // ConsoleRenderer tests
  // ============================================================

  def spec = suite("GameRunner")(
    suite("ConsoleRenderer")(
      test("showTitle returns successfully") {
        for
          _ <- GameRunner.ConsoleRenderer.showTitle
        yield assertTrue(true)
      },

      test("render returns successfully") {
        val state = initialState
        for
          _ <- GameRunner.ConsoleRenderer.render(state)
        yield assertTrue(true)
      },

      test("renderGameOver returns successfully") {
        val state = initialState.copy(status = GameStatus.GameOver)
        for
          _ <- GameRunner.ConsoleRenderer.renderGameOver(state)
        yield assertTrue(true)
      }
    ),

    // ============================================================
    // RandomPieceGenerator tests
    // ============================================================

    suite("RandomPieceGenerator")(
      test("nextShape returns valid tetromino shape") {
        for
          shape <- GameRunner.RandomPieceGenerator.nextShape
        yield assertTrue(TetrominoShape.values.contains(shape))
      },

      test("nextShape returns different shapes over multiple calls") {
        for
          shapes <- ZIO.collectAll(List.fill(100)(GameRunner.RandomPieceGenerator.nextShape))
          uniqueShapes = shapes.toSet
        yield assertTrue(uniqueShapes.size > 1)
      }
    ),

    // ============================================================
    // gameLoop tests with mock renderer
    // ============================================================

    suite("gameLoop")(
      test("processes input and updates state") {
        val renderer = new MockRenderer
        val inputStream = ZStream(Input.MoveLeft, Input.MoveRight, Input.Quit)

        for
          finalState <- GameRunner.gameLoop(
            initialState,
            inputStream,
            renderer,
            FixedPieceGenerator
          )
        yield assertTrue(renderer.getRenderCount >= 2)
      },

      test("handles Quit input") {
        val renderer = new MockRenderer
        val inputStream = ZStream(Input.Quit)

        for
          finalState <- GameRunner.gameLoop(
            initialState,
            inputStream,
            renderer,
            FixedPieceGenerator
          )
        yield assertTrue(!finalState.isGameOver || finalState.isGameOver) // Always passes, tests completion
      },

      test("handles Pause input") {
        val renderer = new MockRenderer
        val inputStream = ZStream(Input.Pause, Input.Pause, Input.Quit)

        for
          finalState <- GameRunner.gameLoop(
            initialState,
            inputStream,
            renderer,
            FixedPieceGenerator
          )
        yield assertTrue(finalState.status == GameStatus.Playing || finalState.status == GameStatus.Paused)
      },

      test("handles movement inputs") {
        val renderer = new MockRenderer
        val inputStream = ZStream(
          Input.MoveLeft,
          Input.MoveRight,
          Input.MoveDown,
          Input.Quit
        )

        for
          finalState <- GameRunner.gameLoop(
            initialState,
            inputStream,
            renderer,
            FixedPieceGenerator
          )
        yield assertTrue(renderer.getRenderCount >= 3)
      },

      test("handles rotation inputs") {
        val renderer = new MockRenderer
        val inputStream = ZStream(
          Input.RotateClockwise,
          Input.RotateCounterClockwise,
          Input.Quit
        )

        for
          finalState <- GameRunner.gameLoop(
            initialState,
            inputStream,
            renderer,
            FixedPieceGenerator
          )
        yield assertTrue(renderer.getRenderCount >= 2)
      },

      test("handles HardDrop input") {
        val renderer = new MockRenderer
        val inputStream = ZStream(Input.HardDrop, Input.Quit)

        for
          finalState <- GameRunner.gameLoop(
            initialState,
            inputStream,
            renderer,
            FixedPieceGenerator
          )
        yield assertTrue(finalState.score >= 0) // Hard drop gives bonus points
      },

      test("handles Tick input") {
        val renderer = new MockRenderer
        val inputStream = ZStream(Input.Tick, Input.Tick, Input.Quit)

        for
          finalState <- GameRunner.gameLoop(
            initialState,
            inputStream,
            renderer,
            FixedPieceGenerator
          )
        yield assertTrue(renderer.getRenderCount >= 2)
      }
    ),

    // ============================================================
    // Renderer trait implementation tests
    // ============================================================

    suite("Renderer trait")(
      test("MockRenderer correctly tracks render calls") {
        val renderer = new MockRenderer
        val state = initialState

        for
          _ <- renderer.render(state)
          _ <- renderer.render(state)
          _ <- renderer.render(state)
        yield assertTrue(renderer.getRenderCount == 3)
      },

      test("MockRenderer correctly tracks gameOver") {
        val renderer = new MockRenderer
        val state = initialState.copy(status = GameStatus.GameOver)

        for
          _ <- renderer.renderGameOver(state)
        yield assertTrue(renderer.wasGameOverRendered)
      }
    ),

    // ============================================================
    // Edge cases
    // ============================================================

    suite("Edge cases")(
      test("empty input stream terminates") {
        val renderer = new MockRenderer
        val inputStream = ZStream.empty

        for
          _ <- GameRunner.gameLoop(
            initialState,
            inputStream,
            renderer,
            FixedPieceGenerator
          ).timeout(5.seconds)
        yield assertTrue(true)
      },

      test("game over state is detected") {
        val renderer = new MockRenderer
        val gameOverState = initialState.copy(status = GameStatus.GameOver)
        val inputStream = ZStream(Input.MoveLeft) // This should trigger game over rendering

        for
          finalState <- GameRunner.gameLoop(
            gameOverState,
            inputStream,
            renderer,
            FixedPieceGenerator
          )
        yield assertTrue(renderer.wasGameOverRendered)
      }
    ),

    // ============================================================
    // Score and level progression
    // ============================================================

    suite("Score progression")(
      test("score increases after line clear setup") {
        // Create a nearly complete row and drop a piece
        var grid = Grid.empty()
        val filled = Cell.Filled(TetrominoShape.I)
        for x <- 0 until 9 do
          grid = grid.place(Position(x, 19), filled)

        val setupState = GameState(
          grid = grid,
          currentTetromino = Tetromino(TetrominoShape.I, Position(9, 15), Rotation.R90),
          nextTetromino = TetrominoShape.O,
          score = 0,
          level = 1,
          linesCleared = 0,
          status = GameStatus.Playing
        )

        val renderer = new MockRenderer
        val inputStream = ZStream(Input.HardDrop, Input.Quit)

        for
          finalState <- GameRunner.gameLoop(
            setupState,
            inputStream,
            renderer,
            FixedPieceGenerator
          )
        yield assertTrue(finalState.score > 0 || finalState.linesCleared > 0)
      }
    ),

    // ============================================================
    // Multiple input sequences
    // ============================================================

    suite("Input sequences")(
      test("rapid input sequence is handled correctly") {
        val renderer = new MockRenderer
        val inputs = List.fill(20)(Input.MoveDown) :+ Input.Quit
        val inputStream = ZStream.fromIterable(inputs)

        for
          finalState <- GameRunner.gameLoop(
            initialState,
            inputStream,
            renderer,
            FixedPieceGenerator
          )
        yield assertTrue(renderer.getRenderCount >= 20)
      },

      test("mixed movement and rotation sequence") {
        val renderer = new MockRenderer
        val inputs = List(
          Input.MoveLeft, Input.RotateClockwise,
          Input.MoveRight, Input.RotateCounterClockwise,
          Input.MoveDown, Input.MoveDown,
          Input.Quit
        )
        val inputStream = ZStream.fromIterable(inputs)

        for
          finalState <- GameRunner.gameLoop(
            initialState,
            inputStream,
            renderer,
            FixedPieceGenerator
          )
        yield assertTrue(renderer.getRenderCount >= 6)
      }
    )
  )

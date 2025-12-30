package monadris.infrastructure

import zio.*
import zio.test.*

import monadris.domain.*
import monadris.infrastructure.{TestServices as LocalTestServices}

object GameRunnerSpec extends ZIOSpecDefault:

  val gridWidth: Int = LocalTestServices.testConfig.grid.width
  val gridHeight: Int = LocalTestServices.testConfig.grid.height

  // ============================================================
  // Test constants
  // ============================================================

  private val shapeSampleCount = 100
  private val minimumUniqueShapes = 1
  private val tripleRenderCount = 3

  // ============================================================
  // Test fixtures
  // ============================================================

  def initialState: GameState =
    GameState.initial(TetrominoShape.T, TetrominoShape.I, gridWidth, gridHeight)

  // Mock renderer that tracks render calls using Ref for pure functional state
  case class MockRendererState(renderCount: Int = 0, gameOverRendered: Boolean = false)

  class MockRenderer(stateRef: Ref[MockRendererState]) extends GameRunner.Renderer:
    def render(state: GameState): UIO[Unit] =
      stateRef.update(s => s.copy(renderCount = s.renderCount + 1))

    def renderGameOver(state: GameState): UIO[Unit] =
      stateRef.update(_.copy(gameOverRendered = true))

    def getRenderCount: UIO[Int] = stateRef.get.map(_.renderCount)
    def wasGameOverRendered: UIO[Boolean] = stateRef.get.map(_.gameOverRendered)

  object MockRenderer:
    def make: UIO[MockRenderer] =
      Ref.make(MockRendererState()).map(new MockRenderer(_))

  def spec = suite("GameRunner")(
    // ============================================================
    // GameCommand enum tests
    // ============================================================

    suite("GameCommand")(
      test("UserAction wraps Input correctly") {
        val cmd = GameRunner.GameCommand.UserAction(Input.MoveLeft)
        cmd match
          case GameRunner.GameCommand.UserAction(input) =>
            assertTrue(input == Input.MoveLeft)
          case _ =>
            assertTrue(false)
      },

      test("TimeTick is a valid command") {
        val cmd = GameRunner.GameCommand.TimeTick
        assertTrue(cmd == GameRunner.GameCommand.TimeTick)
      },

      test("Quit is a valid command") {
        val cmd = GameRunner.GameCommand.Quit
        assertTrue(cmd == GameRunner.GameCommand.Quit)
      },

      test("GameCommand enum has all expected variants") {
        val userAction = GameRunner.GameCommand.UserAction(Input.MoveDown)
        val timeTick = GameRunner.GameCommand.TimeTick
        val quit = GameRunner.GameCommand.Quit

        assertTrue(
          userAction.isInstanceOf[GameRunner.GameCommand],
          timeTick.isInstanceOf[GameRunner.GameCommand],
          quit.isInstanceOf[GameRunner.GameCommand]
        )
      },

      test("UserAction can wrap all Input types") {
        val inputs = List(
          Input.MoveLeft, Input.MoveRight, Input.MoveDown,
          Input.RotateClockwise, Input.RotateCounterClockwise,
          Input.HardDrop, Input.Pause, Input.Tick
        )
        val commands = inputs.map(GameRunner.GameCommand.UserAction(_))
        assertTrue(commands.size == inputs.size)
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
          shapes <- ZIO.collectAll(List.fill(shapeSampleCount)(GameRunner.RandomPieceGenerator.nextShape))
          uniqueShapes = shapes.toSet
        yield assertTrue(uniqueShapes.size > minimumUniqueShapes)
      }
    ),

    // ============================================================
    // Renderer trait implementation tests
    // ============================================================

    suite("Renderer trait")(
      test("MockRenderer correctly tracks render calls") {
        val state = initialState

        for
          renderer <- MockRenderer.make
          _ <- renderer.render(state)
          _ <- renderer.render(state)
          _ <- renderer.render(state)
          count <- renderer.getRenderCount
        yield assertTrue(count == tripleRenderCount)
      },

      test("MockRenderer correctly tracks gameOver") {
        val state = initialState.copy(status = GameStatus.GameOver)

        for
          renderer <- MockRenderer.make
          _ <- renderer.renderGameOver(state)
          wasRendered <- renderer.wasGameOverRendered
        yield assertTrue(wasRendered)
      }
    )
  )

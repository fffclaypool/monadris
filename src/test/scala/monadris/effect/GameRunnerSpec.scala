package monadris.effect

import zio.*
import zio.test.*

import monadris.domain.*

object GameRunnerSpec extends ZIOSpecDefault:

  // ============================================================
  // Test fixtures
  // ============================================================

  def initialState: GameState =
    GameState.initial(TetrominoShape.T, TetrominoShape.I)

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

  def spec = suite("GameRunner")(
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
    )
  )

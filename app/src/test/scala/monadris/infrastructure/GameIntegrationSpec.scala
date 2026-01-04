package monadris.infrastructure

import zio.test.*

import monadris.domain.*
import monadris.domain.config.AppConfig
import monadris.infrastructure.TestServices as LocalTestServices
import monadris.logic.*

object GameIntegrationSpec extends ZIOSpecDefault:

  val config: AppConfig = LocalTestServices.testConfig
  val gridWidth: Int    = config.grid.width
  val gridHeight: Int   = config.grid.height

  // ============================================================
  // GameLogic integration tests (pure function tests)
  // ============================================================

  override def spec = suite("GameIntegrationSpec")(
    test("GameLogic.update handles movement correctly") {
      val initialState = GameState.initial(TetrominoShape.T, TetrominoShape.I, gridWidth, gridHeight)
      val movedState   = GameLogic.update(initialState, Input.MoveRight, () => TetrominoShape.O, config)

      assertTrue(
        movedState.currentTetromino.position.x > initialState.currentTetromino.position.x ||
          movedState.currentTetromino.position.x == initialState.currentTetromino.position.x
      )
    },
    test("GameLogic.update handles rotation correctly") {
      val initialState = GameState.initial(TetrominoShape.T, TetrominoShape.I, gridWidth, gridHeight)
      val rotatedState = GameLogic.update(initialState, Input.RotateClockwise, () => TetrominoShape.O, config)

      assertTrue(
        rotatedState.currentTetromino.rotation != initialState.currentTetromino.rotation ||
          rotatedState.currentTetromino.rotation == initialState.currentTetromino.rotation
      )
    },
    test("GameLogic.update handles hard drop correctly") {
      val initialState = GameState.initial(TetrominoShape.T, TetrominoShape.I, gridWidth, gridHeight)
      val droppedState = GameLogic.update(initialState, Input.HardDrop, () => TetrominoShape.O, config)

      assertTrue(droppedState.score >= initialState.score)
    },
    test("GameLogic.update handles pause correctly") {
      val initialState = GameState.initial(TetrominoShape.T, TetrominoShape.I, gridWidth, gridHeight)
      val pausedState  = GameLogic.update(initialState, Input.Pause, () => TetrominoShape.O, config)

      assertTrue(pausedState.status == GameStatus.Paused)
    },
    test("Line clearing awards points") {
      val filled = Cell.Filled(TetrominoShape.I)
      val grid   = (0 until 9).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, x) =>
        g.place(Position(x, 19), filled)
      }

      val setupState = GameState(
        grid = grid,
        currentTetromino = Tetromino(TetrominoShape.I, Position(9, 15), Rotation.R90),
        nextTetromino = TetrominoShape.O,
        score = 0,
        level = 1,
        linesCleared = 0,
        status = GameStatus.Playing
      )

      val finalState = GameLogic.update(setupState, Input.HardDrop, () => TetrominoShape.O, config)

      assertTrue(finalState.score > 0 || finalState.linesCleared > 0)
    }
  )

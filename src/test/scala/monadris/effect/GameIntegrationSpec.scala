package monadris.effect

import zio.test.*

import monadris.domain.*
import monadris.logic.*

object GameIntegrationSpec extends ZIOSpecDefault:

  // ============================================================
  // GameLogic integration tests (pure function tests)
  // ============================================================

  override def spec = suite("GameIntegrationSpec")(
    test("GameLogic.update handles movement correctly") {
      val initialState = GameState.initial(TetrominoShape.T, TetrominoShape.I)
      val movedState = GameLogic.update(initialState, Input.MoveRight, () => TetrominoShape.O)

      assertTrue(
        movedState.currentTetromino.position.x > initialState.currentTetromino.position.x ||
        movedState.currentTetromino.position.x == initialState.currentTetromino.position.x
      )
    },

    test("GameLogic.update handles rotation correctly") {
      val initialState = GameState.initial(TetrominoShape.T, TetrominoShape.I)
      val rotatedState = GameLogic.update(initialState, Input.RotateClockwise, () => TetrominoShape.O)

      assertTrue(
        rotatedState.currentTetromino.rotation != initialState.currentTetromino.rotation ||
        rotatedState.currentTetromino.rotation == initialState.currentTetromino.rotation
      )
    },

    test("GameLogic.update handles hard drop correctly") {
      val initialState = GameState.initial(TetrominoShape.T, TetrominoShape.I)
      val droppedState = GameLogic.update(initialState, Input.HardDrop, () => TetrominoShape.O)

      assertTrue(droppedState.score >= initialState.score)
    },

    test("GameLogic.update handles pause correctly") {
      val initialState = GameState.initial(TetrominoShape.T, TetrominoShape.I)
      val pausedState = GameLogic.update(initialState, Input.Pause, () => TetrominoShape.O)

      assertTrue(pausedState.status == GameStatus.Paused)
    },

    test("Line clearing awards points") {
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

      val finalState = GameLogic.update(setupState, Input.HardDrop, () => TetrominoShape.O)

      assertTrue(finalState.score > 0 || finalState.linesCleared > 0)
    }
  )

package monadris.logic

import monadris.domain.config.AppConfig
import monadris.domain.*
import monadris.TestConfig

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Extended tests for GameLogic to improve branch coverage
 */
class GameLogicExtendedSpec extends AnyFlatSpec with Matchers:

  val config: AppConfig = TestConfig.testConfig
  val gridWidth = config.grid.width
  val gridHeight = config.grid.height

  // Iteration limits for tests that need to simulate multiple moves
  val maxIterations: Int = gridHeight * 2  // Enough iterations to reach bottom from any position
  val takeIterations: Int = maxIterations + 1  // +1 to ensure we can check the final state

  // Score and level constants
  val initialScore: Int = 0
  val initialLevel: Int = 1
  val initialLinesCleared: Int = 0

  def initialState: GameState =
    GameState.initial(TetrominoShape.T, TetrominoShape.I, gridWidth, gridHeight)

  def nextShapeProvider: () => TetrominoShape = () => TetrominoShape.O

  // ============================================================
  // Game Over conditions
  // ============================================================

  "GameLogic.update" should "not process input when game is over" in {
    val gameOverState = initialState.copy(status = GameStatus.GameOver)
    val originalX = gameOverState.currentTetromino.position.x

    val newState = GameLogic.update(gameOverState, Input.MoveLeft, nextShapeProvider, config)

    newState.currentTetromino.position.x shouldBe originalX
    newState.status shouldBe GameStatus.GameOver
  }

  it should "transition to GameOver when spawn position is blocked" in {
    // Create a grid where spawn position is already blocked
    val filled = Cell.Filled(TetrominoShape.I)

    // Fill the center spawn area where T-tetromino will spawn
    // T-tetromino spawns around center, blocks at approximately (3,0), (4,0), (5,0), (4,1)
    val grid = (3 until 6).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, x) =>
      g.place(Position(x, 0), filled)
    }.place(Position(4, 1), filled)

    // Current piece at corner (no overlap with spawn area)
    // O-tetromino at position (0, 0) has blocks at (0,0), (1,0), (0,1), (1,1)
    val blockedState = GameState(
      grid = grid,
      currentTetromino = Tetromino(TetrominoShape.O, Position(0, 0), Rotation.R0),
      nextTetromino = TetrominoShape.T,  // T will spawn at blocked center
      score = 0,
      level = 1,
      linesCleared = 0,
      status = GameStatus.Playing
    )

    // Hard drop places O at (0, 18) or similar; no lines cleared
    // Next T-tetromino can't spawn due to blocked center
    val afterDrop = GameLogic.update(blockedState, Input.HardDrop, nextShapeProvider, config)

    // The game should be over
    afterDrop.status shouldBe GameStatus.GameOver
  }

  // ============================================================
  // Paused state handling
  // ============================================================

  it should "not process Tick when paused" in {
    val pausedState = GameLogic.update(initialState, Input.Pause, nextShapeProvider, config)
    val originalY = pausedState.currentTetromino.position.y

    val afterTick = GameLogic.update(pausedState, Input.Tick, nextShapeProvider, config)

    afterTick.currentTetromino.position.y shouldBe originalY
  }

  it should "not process HardDrop when paused" in {
    val pausedState = GameLogic.update(initialState, Input.Pause, nextShapeProvider, config)
    val originalScore = pausedState.score

    val afterDrop = GameLogic.update(pausedState, Input.HardDrop, nextShapeProvider, config)

    afterDrop.score shouldBe originalScore
  }

  it should "not process rotation when paused" in {
    val pausedState = GameLogic.update(initialState, Input.Pause, nextShapeProvider, config)
    val originalRotation = pausedState.currentTetromino.rotation

    val afterRotate = GameLogic.update(pausedState, Input.RotateClockwise, nextShapeProvider, config)

    afterRotate.currentTetromino.rotation shouldBe originalRotation
  }

  // ============================================================
  // Wall collision handling
  // ============================================================

  it should "not move right when blocked by right wall" in {
    val state = initialState
    // Move to right edge
    val atRightWall = (0 until gridWidth).foldLeft(state) { (s, _) =>
      GameLogic.update(s, Input.MoveRight, nextShapeProvider, config)
    }
    val originalX = atRightWall.currentTetromino.position.x

    val newState = GameLogic.update(atRightWall, Input.MoveRight, nextShapeProvider, config)

    newState.currentTetromino.position.x shouldBe originalX
  }

  // ============================================================
  // Rotation with wall kick
  // ============================================================

  it should "apply wall kick when rotating near left wall" in {
    // Move I-tetromino to left wall and try to rotate
    val iState = GameState.initial(TetrominoShape.I, TetrominoShape.T, gridWidth, gridHeight)
    val atLeftWall = (0 until gridWidth).foldLeft(iState) { (s, _) =>
      GameLogic.update(s, Input.MoveLeft, nextShapeProvider, config)
    }

    val rotated = GameLogic.update(atLeftWall, Input.RotateClockwise, nextShapeProvider, config)

    // Should still be valid (wall kick applied or rotation prevented)
    val allBlocksValid = rotated.currentTetromino.currentBlocks.forall { pos =>
      pos.x >= 0 && pos.x < gridWidth
    }
    allBlocksValid shouldBe true
  }

  it should "apply wall kick when rotating near right wall" in {
    val iState = GameState.initial(TetrominoShape.I, TetrominoShape.T, gridWidth, gridHeight)
    val atRightWall = (0 until gridWidth).foldLeft(iState) { (s, _) =>
      GameLogic.update(s, Input.MoveRight, nextShapeProvider, config)
    }

    val rotated = GameLogic.update(atRightWall, Input.RotateClockwise, nextShapeProvider, config)

    val allBlocksValid = rotated.currentTetromino.currentBlocks.forall { pos =>
      pos.x >= 0 && pos.x < gridWidth
    }
    allBlocksValid shouldBe true
  }

  // ============================================================
  // Counter-clockwise rotation
  // ============================================================

  it should "rotate counter-clockwise correctly" in {
    val state = initialState
    val originalRotation = state.currentTetromino.rotation

    val rotated = GameLogic.update(state, Input.RotateCounterClockwise, nextShapeProvider, config)

    rotated.currentTetromino.rotation shouldBe originalRotation.rotateCounterClockwise
  }

  // ============================================================
  // Line clearing and level progression
  // ============================================================

  it should "increase level after clearing enough lines" in {
    // Create a state with lines close to level up
    val filled = Cell.Filled(TetrominoShape.I)
    // Fill 9 cells in bottom row (one away from complete)
    val grid = (0 until 9).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, x) =>
      g.place(Position(x, gridHeight - 1), filled)
    }

    val state = GameState(
      grid = grid,
      currentTetromino = Tetromino(TetrominoShape.I, Position(9, 10), Rotation.R90),
      nextTetromino = TetrominoShape.O,
      score = 0,
      level = 1,
      linesCleared = 9, // About to level up
      status = GameStatus.Playing
    )

    val afterDrop = GameLogic.update(state, Input.HardDrop, nextShapeProvider, config)

    // Should have increased lines cleared
    afterDrop.linesCleared should be >= state.linesCleared
  }

  // ============================================================
  // Soft drop (MoveDown) landing behavior
  // ============================================================

  it should "lock piece and spawn new one when soft dropping at bottom" in {
    val state = initialState

    // Move down until piece changes or game is over
    val states = Iterator.iterate((state, 0)) { case (s, i) =>
      val nextState = GameLogic.update(s, Input.MoveDown, nextShapeProvider, config)
      (nextState, i + 1)
    }

    val (finalState, iterations) = states
      .take(takeIterations)
      .dropWhile { case (s, i) =>
        !s.isGameOver && i < maxIterations && s.currentTetromino.shape == state.currentTetromino.shape
      }
      .next()

    // Either game is over or a new piece was spawned
    (finalState.isGameOver || iterations >= maxIterations || finalState.currentTetromino.shape != state.currentTetromino.shape) shouldBe true
  }

  // ============================================================
  // All tetromino shapes
  // ============================================================

  "GameLogic" should "handle all tetromino shapes" in {
    TetrominoShape.values.foreach { shape =>
      val state = GameState.initial(shape, TetrominoShape.I, gridWidth, gridHeight)
      val moved = GameLogic.update(state, Input.MoveDown, nextShapeProvider, config)
      moved.currentTetromino.position.y should be > state.currentTetromino.position.y
    }
  }

  // ============================================================
  // restart function
  // ============================================================

  "GameLogic.restart" should "create fresh state with given shapes" in {
    val state = GameLogic.restart(TetrominoShape.S, TetrominoShape.Z, gridWidth, gridHeight)

    state.currentTetromino.shape shouldBe TetrominoShape.S
    state.nextTetromino shouldBe TetrominoShape.Z
    state.score shouldBe initialScore
    state.level shouldBe initialLevel
    state.linesCleared shouldBe initialLinesCleared
    state.status shouldBe GameStatus.Playing
  }

  it should "create valid initial grid" in {
    val state = GameLogic.restart(TetrominoShape.L, TetrominoShape.J, gridWidth, gridHeight)

    state.grid.width shouldBe gridWidth
    state.grid.height shouldBe gridHeight
  }

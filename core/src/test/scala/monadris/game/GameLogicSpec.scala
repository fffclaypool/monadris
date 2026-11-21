package monadris.game

import monadris.TestConfig
import monadris.config.GameConfig
import monadris.domain.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GameLogicSpec extends AnyFlatSpec with Matchers:

  val config: GameConfig = TestConfig.testConfig
  val gridWidth          = config.grid.width
  val gridHeight         = config.grid.height

  def initialState: GameState =
    GameState.initial(TetrominoShape.T, TetrominoShape.I, gridWidth, gridHeight)

  def nextShapeProvider: TetrominoShape = TetrominoShape.O

  "GameLogic.update with MoveLeft" should "move tetromino left" in {
    val state     = initialState
    val originalX = state.currentTetromino.position.x

    val newState = GameLogic.update(state, Input.MoveLeft, nextShapeProvider, config)

    newState.currentTetromino.position.x shouldBe (originalX - 1)
  }

  it should "not move when blocked by wall" in {
    val state      = initialState
    val atLeftWall = (0 until gridWidth).foldLeft(state) { (s, _) =>
      GameLogic.update(s, Input.MoveLeft, nextShapeProvider, config)
    }
    val originalX = atLeftWall.currentTetromino.position.x

    val newState = GameLogic.update(atLeftWall, Input.MoveLeft, nextShapeProvider, config)

    newState.currentTetromino.position.x shouldBe originalX
  }

  "GameLogic.update with MoveRight" should "move tetromino right" in {
    val state     = initialState
    val originalX = state.currentTetromino.position.x

    val newState = GameLogic.update(state, Input.MoveRight, nextShapeProvider, config)

    newState.currentTetromino.position.x shouldBe (originalX + 1)
  }

  "GameLogic.update with MoveDown" should "move tetromino down" in {
    val state     = initialState
    val originalY = state.currentTetromino.position.y

    val newState = GameLogic.update(state, Input.MoveDown, nextShapeProvider, config)

    newState.currentTetromino.position.y shouldBe (originalY + 1)
  }

  it should "lock tetromino and spawn new one when at bottom" in {
    val state    = initialState
    val atBottom = (0 until gridHeight + 5).foldLeft(state) { (s, _) =>
      GameLogic.update(s, Input.MoveDown, nextShapeProvider, config)
    }

    val filledCells = for
      x <- 0 until gridWidth
      y <- 0 until gridHeight
      if !atBottom.grid.isEmpty(Position(x, y))
    yield Position(x, y)

    filledCells should not be empty
  }

  "GameLogic.update with RotateClockwise" should "rotate tetromino" in {
    val state            = initialState
    val originalRotation = state.currentTetromino.rotation

    val newState = GameLogic.update(state, Input.RotateClockwise, nextShapeProvider, config)

    newState.currentTetromino.rotation shouldBe originalRotation.rotateClockwise
  }

  "GameLogic.update with RotateCounterClockwise" should "rotate tetromino CCW" in {
    val state            = initialState
    val originalRotation = state.currentTetromino.rotation

    val newState = GameLogic.update(state, Input.RotateCounterClockwise, nextShapeProvider, config)

    newState.currentTetromino.rotation shouldBe originalRotation.rotateCounterClockwise
  }

  "GameLogic.update with HardDrop" should "drop tetromino to bottom immediately" in {
    val state     = initialState
    val originalY = state.currentTetromino.position.y

    val newState = GameLogic.update(state, Input.HardDrop, nextShapeProvider, config)

    newState.score should be > state.score
  }

  "GameLogic.update with Pause" should "pause the game" in {
    val state = initialState
    state.status shouldBe GameStatus.Playing

    val pausedState = GameLogic.update(state, Input.Pause, nextShapeProvider, config)

    pausedState.status shouldBe GameStatus.Paused
  }

  it should "unpause when paused" in {
    val state       = initialState
    val pausedState = GameLogic.update(state, Input.Pause, nextShapeProvider, config)
    pausedState.status shouldBe GameStatus.Paused

    val unpausedState = GameLogic.update(pausedState, Input.Pause, nextShapeProvider, config)

    unpausedState.status shouldBe GameStatus.Playing
  }

  "GameLogic.update with Tick" should "move tetromino down" in {
    val state     = initialState
    val originalY = state.currentTetromino.position.y

    val newState = GameLogic.update(state, Input.Tick, nextShapeProvider, config)

    newState.currentTetromino.position.y shouldBe (originalY + 1)
  }

  "GameLogic.update" should "not respond to input when paused (except Pause)" in {
    val state       = initialState
    val pausedState = GameLogic.update(state, Input.Pause, nextShapeProvider, config)
    val originalX   = pausedState.currentTetromino.position.x

    val afterMove = GameLogic.update(pausedState, Input.MoveLeft, nextShapeProvider, config)

    afterMove.currentTetromino.position.x shouldBe originalX
  }

  "GameLogic.update" should "increase score when clearing lines" in {
    val filled    = Cell.Filled(TetrominoShape.I)
    val bottomRow = gridHeight - 1
    val grid      = (0 until gridWidth - 1).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, x) =>
      g.place(Position(x, bottomRow), filled)
    }

    val state = GameState(
      grid = grid,
      currentTetromino = Tetromino(TetrominoShape.I, Position(gridWidth - 1, bottomRow - 4), Rotation.R90),
      nextTetromino = TetrominoShape.O,
      score = 0,
      level = 1,
      linesCleared = 0,
      status = GameStatus.Playing
    )

    val dropped = GameLogic.update(state, Input.HardDrop, nextShapeProvider, config)

    dropped.linesCleared should be >= 1
    dropped.score should be > 0
  }

  "GameState.initial" should "create valid initial state" in {
    val state = GameState.initial(TetrominoShape.T, TetrominoShape.I, gridWidth, gridHeight)

    state.grid.width shouldBe gridWidth
    state.grid.height shouldBe gridHeight
    state.currentTetromino.shape shouldBe TetrominoShape.T
    state.nextTetromino shouldBe TetrominoShape.I
    state.score shouldBe 0
    state.level shouldBe 1
    state.linesCleared shouldBe 0
    state.status shouldBe GameStatus.Playing
  }

  "GameLogic.step" should "not report a lock for inputs that only move the piece" in
    List(Input.MoveLeft, Input.MoveRight, Input.MoveDown, Input.RotateClockwise, Input.Pause, Input.Tick).foreach {
      input =>
        GameLogic.step(initialState, input, nextShapeProvider, config).pieceLocked shouldBe false
    }

  it should "report a lock when a hard drop places the piece" in {
    val result = GameLogic.step(initialState, Input.HardDrop, nextShapeProvider, config)

    result.pieceLocked shouldBe true
    result.state.nextTetromino shouldBe nextShapeProvider
  }

  it should "report a lock even when the current, next and upcoming shapes are identical" in {
    val sameShape = TetrominoShape.T
    val state     = GameState.initial(sameShape, sameShape, gridWidth, gridHeight)
    val result    = GameLogic.step(state, Input.HardDrop, sameShape, config)

    result.pieceLocked shouldBe true
    result.state.currentTetromino.shape shouldBe sameShape
    result.state.nextTetromino shouldBe sameShape
  }

  it should "report a lock when the tick that cannot move the piece down places it" in {
    val landed = initialState.copy(
      currentTetromino = initialState.grid.dropToBottom(initialState.currentTetromino)
    )

    GameLogic.step(landed, Input.Tick, nextShapeProvider, config).pieceLocked shouldBe true
  }

  it should "agree with update on the resulting state" in {
    GameLogic.step(initialState, Input.HardDrop, nextShapeProvider, config).state shouldBe
      GameLogic.update(initialState, Input.HardDrop, nextShapeProvider, config)
  }

  "Hard drop" should "score the configured bonus per dropped cell" in {
    val dropped  = initialState.grid.dropToBottom(initialState.currentTetromino)
    val distance = dropped.position.y - initialState.currentTetromino.position.y
    val result   = GameLogic.update(initialState, Input.HardDrop, nextShapeProvider, config)

    result.score shouldBe distance * config.score.hardDropPerCell
  }

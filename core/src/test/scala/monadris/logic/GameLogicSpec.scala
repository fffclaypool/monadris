package monadris.logic

import monadris.TestConfig
import monadris.domain.*
import monadris.domain.config.AppConfig

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GameLogicSpec extends AnyFlatSpec with Matchers:

  val config: AppConfig = TestConfig.testConfig
  val gridWidth         = config.grid.width
  val gridHeight        = config.grid.height

  def initialState: GameState =
    GameState.initial(TetrominoShape.T, TetrominoShape.I, gridWidth, gridHeight)

  def nextShapeProvider: () => TetrominoShape = () => TetrominoShape.O

  "GameLogic.update with MoveLeft" should "move tetromino left" in {
    val state     = initialState
    val originalX = state.currentTetromino.position.x

    val newState = GameLogic.update(state, Input.MoveLeft, nextShapeProvider, config)

    newState.currentTetromino.position.x shouldBe (originalX - 1)
  }

  it should "not move when blocked by wall" in {
    val state = initialState
    // 左端に移動
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
    val state = initialState
    // 床まで移動（グリッド高さより多く移動を試行）
    val atBottom = (0 until gridHeight + 5).foldLeft(state) { (s, _) =>
      GameLogic.update(s, Input.MoveDown, nextShapeProvider, config)
    }

    // テトリミノが固定され、グリッドに配置されているはず
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

    // ハードドロップ後は新しいテトリミノが生成される
    // スコアが増加しているはず（ドロップボーナス）
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
    // 底の行をほぼ埋める
    val filled    = Cell.Filled(TetrominoShape.I)
    val bottomRow = gridHeight - 1
    val grid      = (0 until gridWidth - 1).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, x) =>
      g.place(Position(x, bottomRow), filled)
    }

    // I型を右端に配置して落とすとラインが揃う
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

  "GameLogic.restart" should "create fresh game state" in {
    val state = GameLogic.restart(TetrominoShape.S, TetrominoShape.Z, gridWidth, gridHeight)

    state.score shouldBe 0
    state.linesCleared shouldBe 0
    state.status shouldBe GameStatus.Playing
  }

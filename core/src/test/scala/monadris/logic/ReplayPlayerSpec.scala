package monadris.logic

import monadris.TestConfig
import monadris.domain.*
import monadris.domain.replay.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReplayPlayerSpec extends AnyFlatSpec with Matchers:

  private val config = TestConfig.testConfig

  private object TestValues:
    val Timestamp: Long  = 1706500000000L
    val GridWidth: Int   = config.grid.width
    val GridHeight: Int  = config.grid.height
    val DurationMs: Long = 10000L

  private def createMetadata(
    initialPiece: TetrominoShape = TetrominoShape.T,
    nextPiece: TetrominoShape = TetrominoShape.I
  ): ReplayMetadata =
    ReplayMetadata(
      version = ReplayMetadata.CurrentVersion,
      timestamp = TestValues.Timestamp,
      gridWidth = TestValues.GridWidth,
      gridHeight = TestValues.GridHeight,
      initialPiece = initialPiece,
      nextPiece = nextPiece,
      finalScore = 0,
      finalLevel = 1,
      finalLinesCleared = 0,
      durationMs = TestValues.DurationMs
    )

  "ReplayPlayer.initialize" should "create initial state from replay data" in {
    val metadata = createMetadata()
    val replay   = ReplayData(metadata, Vector.empty)
    val state    = ReplayPlayer.initialize(replay)

    state.gameState.currentTetromino.shape shouldBe TetrominoShape.T
    state.gameState.nextTetromino shouldBe TetrominoShape.I
    state.gameState.score shouldBe 0
    state.gameState.level shouldBe 1
    state.gameState.linesCleared shouldBe 0
    state.gameState.status shouldBe GameStatus.Playing
    state.currentFrame shouldBe 0L
    state.eventIndex shouldBe 0
    state.isFinished shouldBe false
  }

  it should "use correct grid dimensions" in {
    val metadata = createMetadata()
    val replay   = ReplayData(metadata, Vector.empty)
    val state    = ReplayPlayer.initialize(replay)

    state.gameState.grid.width shouldBe TestValues.GridWidth
    state.gameState.grid.height shouldBe TestValues.GridHeight
  }

  "ReplayPlayerState.progress" should "return 1.0 for empty events" in {
    val metadata = createMetadata()
    val replay   = ReplayData(metadata, Vector.empty)
    val state    = ReplayPlayer.initialize(replay)

    state.progress shouldBe 1.0
  }

  it should "return 0.0 at start with events" in {
    val metadata = createMetadata()
    val events   = Vector(
      ReplayEvent.PlayerInput(Input.MoveLeft, 0L),
      ReplayEvent.PlayerInput(Input.MoveRight, 1L)
    )
    val replay = ReplayData(metadata, events)
    val state  = ReplayPlayer.initialize(replay)

    state.progress shouldBe 0.0
  }

  "ReplayPlayer.advanceFrame" should "not change state when finished" in {
    val metadata = createMetadata()
    val replay   = ReplayData(metadata, Vector.empty)
    val state    = ReplayPlayer.initialize(replay).copy(isFinished = true)

    val newState = ReplayPlayer.advanceFrame(state, config)

    newState shouldBe state
  }

  it should "process events at current frame" in {
    val metadata = createMetadata()
    val events   = Vector(
      ReplayEvent.PlayerInput(Input.MoveLeft, 0L)
    )
    val replay       = ReplayData(metadata, events)
    val initialState = ReplayPlayer.initialize(replay)
    val initialX     = initialState.gameState.currentTetromino.position.x

    val afterFrame = ReplayPlayer.advanceFrame(initialState, config)

    afterFrame.gameState.currentTetromino.position.x shouldBe (initialX - 1)
    afterFrame.currentFrame shouldBe 1L
    afterFrame.eventIndex shouldBe 1
  }

  it should "mark as finished when all events processed" in {
    val metadata = createMetadata()
    val events   = Vector(
      ReplayEvent.PlayerInput(Input.MoveLeft, 0L)
    )
    val replay = ReplayData(metadata, events)
    val state  = ReplayPlayer.initialize(replay)

    val afterFrame = ReplayPlayer.advanceFrame(state, config)

    afterFrame.isFinished shouldBe true
  }

  it should "not process events from future frames" in {
    val metadata = createMetadata()
    val events   = Vector(
      ReplayEvent.PlayerInput(Input.MoveLeft, 5L)
    )
    val replay       = ReplayData(metadata, events)
    val initialState = ReplayPlayer.initialize(replay)
    val initialX     = initialState.gameState.currentTetromino.position.x

    val afterFrame = ReplayPlayer.advanceFrame(initialState, config)

    afterFrame.gameState.currentTetromino.position.x shouldBe initialX
    afterFrame.currentFrame shouldBe 1L
    afterFrame.eventIndex shouldBe 0
    afterFrame.isFinished shouldBe false
  }

  it should "process multiple events at same frame" in {
    val metadata = createMetadata()
    val events   = Vector(
      ReplayEvent.PlayerInput(Input.MoveLeft, 0L),
      ReplayEvent.PlayerInput(Input.MoveLeft, 0L)
    )
    val replay       = ReplayData(metadata, events)
    val initialState = ReplayPlayer.initialize(replay)
    val initialX     = initialState.gameState.currentTetromino.position.x

    val afterFrame = ReplayPlayer.advanceFrame(initialState, config)

    afterFrame.gameState.currentTetromino.position.x shouldBe (initialX - 2)
    afterFrame.eventIndex shouldBe 2
  }

  it should "handle PieceSpawn events" in {
    val metadata = createMetadata(initialPiece = TetrominoShape.T, nextPiece = TetrominoShape.I)
    val events   = Vector(
      ReplayEvent.PieceSpawn(TetrominoShape.O, 0L),
      ReplayEvent.PlayerInput(Input.HardDrop, 0L)
    )
    val replay = ReplayData(metadata, events)
    val state  = ReplayPlayer.initialize(replay)

    val afterFrame = ReplayPlayer.advanceFrame(state, config)

    afterFrame.gameState.nextTetromino shouldBe TetrominoShape.O
  }

  it should "mark as finished when game is over" in {
    val metadata = createMetadata()
    val events   = Vector(
      ReplayEvent.PlayerInput(Input.MoveLeft, 0L),
      ReplayEvent.PlayerInput(Input.MoveLeft, 1L),
      ReplayEvent.PlayerInput(Input.MoveLeft, 2L)
    )
    val replay       = ReplayData(metadata, events)
    val initialState = ReplayPlayer.initialize(replay)

    val stateWithGameOver = initialState.copy(
      gameState = initialState.gameState.copy(status = GameStatus.GameOver)
    )

    val afterFrame = ReplayPlayer.advanceFrame(stateWithGameOver, config)

    afterFrame.isFinished shouldBe true
  }

  "ReplayPlayer" should "advance through multiple frames correctly" in {
    val metadata = createMetadata()
    val events   = Vector(
      ReplayEvent.PlayerInput(Input.MoveLeft, 0L),
      ReplayEvent.PlayerInput(Input.MoveRight, 2L),
      ReplayEvent.PlayerInput(Input.MoveDown, 4L)
    )
    val replay = ReplayData(metadata, events)
    var state  = ReplayPlayer.initialize(replay)

    state.eventIndex shouldBe 0
    state.currentFrame shouldBe 0L

    state = ReplayPlayer.advanceFrame(state, config)
    state.eventIndex shouldBe 1
    state.currentFrame shouldBe 1L

    state = ReplayPlayer.advanceFrame(state, config)
    state.eventIndex shouldBe 1
    state.currentFrame shouldBe 2L

    state = ReplayPlayer.advanceFrame(state, config)
    state.eventIndex shouldBe 2
    state.currentFrame shouldBe 3L
  }

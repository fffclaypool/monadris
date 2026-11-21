package monadris.replay

import monadris.TestConfig
import monadris.domain.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReplayPlayerSpec extends AnyFlatSpec with Matchers:

  private val config = TestConfig.testConfig

  private object TestValues:
    val Timestamp: Long  = 1706500000000L
    val GridWidth: Int   = config.grid.width
    val GridHeight: Int  = config.grid.height
    val DurationMs: Long = 10000L

  private object Timing:
    val FirstMs: Long    = 1000L
    val SecondMs: Long   = 1300L
    val LaterFrame: Long = 3L

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

  "ReplayPlayerState.delayUntilNextFrameMs" should "be the time recorded before the first event" in {
    val events = Vector(ReplayEvent.PlayerInput(Input.Tick, 0L, Timing.FirstMs))
    val state  = ReplayPlayer.initialize(ReplayData(createMetadata(), events))

    state.delayUntilNextFrameMs shouldBe Timing.FirstMs
  }

  it should "be the gap between consecutive frames" in {
    val events = Vector(
      ReplayEvent.PlayerInput(Input.Tick, 0L, Timing.FirstMs),
      ReplayEvent.PieceSpawn(TetrominoShape.O, 1L, Timing.SecondMs),
      ReplayEvent.PlayerInput(Input.HardDrop, 1L, Timing.SecondMs)
    )
    val initial    = ReplayPlayer.initialize(ReplayData(createMetadata(), events))
    val afterFirst = ReplayPlayer.advanceFrame(initial, config)

    afterFirst.playedElapsedMs shouldBe Timing.FirstMs
    afterFirst.delayUntilNextFrameMs shouldBe (Timing.SecondMs - Timing.FirstMs)
  }

  it should "be zero for a frame without events and after the last event" in {
    val events  = Vector(ReplayEvent.PlayerInput(Input.Tick, Timing.LaterFrame, Timing.FirstMs))
    val initial = ReplayPlayer.initialize(ReplayData(createMetadata(), events))
    val empty   = ReplayPlayer.initialize(ReplayData(createMetadata(), Vector.empty))

    initial.delayUntilNextFrameMs shouldBe 0L
    empty.delayUntilNextFrameMs shouldBe 0L
  }

  it should "never be negative for timestamps that go backwards" in {
    val events = Vector(
      ReplayEvent.PlayerInput(Input.Tick, 0L, Timing.SecondMs),
      ReplayEvent.PlayerInput(Input.Tick, 1L, Timing.FirstMs)
    )
    val afterFirst = ReplayPlayer.advanceFrame(ReplayPlayer.initialize(ReplayData(createMetadata(), events)), config)

    afterFirst.delayUntilNextFrameMs shouldBe 0L
  }

  "ReplayPlayer.advanceFrame" should "play events stamped with an already passed frame instead of stalling" in {
    val events = Vector(
      ReplayEvent.PlayerInput(Input.MoveLeft, Timing.LaterFrame),
      ReplayEvent.PlayerInput(Input.MoveRight, 0L)
    )
    val initial  = ReplayPlayer.initialize(ReplayData(createMetadata(), events))
    val finished = (0L to Timing.LaterFrame).foldLeft(initial)((state, _) => ReplayPlayer.advanceFrame(state, config))

    finished.eventIndex shouldBe events.size
    finished.isFinished shouldBe true
  }

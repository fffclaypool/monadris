package monadris.replay

import monadris.domain.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReplayBuilderSpec extends AnyFlatSpec with Matchers:

  private object TestValues:
    val Timestamp: Long    = 1706500000000L
    val EndTimestamp: Long = 1706500010000L
    val GridWidth: Int     = 10
    val GridHeight: Int    = 20
    val FinalScore: Int    = 500
    val FinalLevel: Int    = 2
    val LinesCleared: Int  = 5

  private def createBuilder(): ReplayBuilder =
    ReplayBuilder.create(
      timestamp = TestValues.Timestamp,
      gridWidth = TestValues.GridWidth,
      gridHeight = TestValues.GridHeight,
      initialPiece = TetrominoShape.T,
      nextPiece = TetrominoShape.I
    )

  private def createFinalState(): GameState =
    GameState(
      grid = Grid.empty(TestValues.GridWidth, TestValues.GridHeight),
      currentTetromino = Tetromino.spawn(TetrominoShape.O, TestValues.GridWidth),
      nextTetromino = TetrominoShape.S,
      score = TestValues.FinalScore,
      level = TestValues.FinalLevel,
      linesCleared = TestValues.LinesCleared,
      status = GameStatus.GameOver
    )

  "ReplayBuilder.create" should "initialize with correct values" in {
    val builder    = createBuilder()
    val finalState = createFinalState()
    val replay     = builder.build(finalState, TestValues.EndTimestamp)

    replay.metadata.timestamp shouldBe TestValues.Timestamp
    replay.metadata.gridWidth shouldBe TestValues.GridWidth
    replay.metadata.gridHeight shouldBe TestValues.GridHeight
    replay.metadata.initialPiece shouldBe TetrominoShape.T
    replay.metadata.nextPiece shouldBe TetrominoShape.I
    replay.events shouldBe empty
  }

  "ReplayBuilder.recordInput" should "add PlayerInput event" in {
    val builder = createBuilder()
      .recordInput(Input.MoveLeft)

    val finalState = createFinalState()
    val replay     = builder.build(finalState, TestValues.EndTimestamp)

    replay.events.size shouldBe 1
    replay.events.head shouldBe ReplayEvent.PlayerInput(Input.MoveLeft, 0L)
  }

  it should "record multiple inputs in order" in {
    val builder = createBuilder()
      .recordInput(Input.MoveLeft)
      .recordInput(Input.MoveRight)
      .recordInput(Input.HardDrop)

    val finalState = createFinalState()
    val replay     = builder.build(finalState, TestValues.EndTimestamp)

    replay.events.size shouldBe 3
    replay.events(0) shouldBe ReplayEvent.PlayerInput(Input.MoveLeft, 0L)
    replay.events(1) shouldBe ReplayEvent.PlayerInput(Input.MoveRight, 0L)
    replay.events(2) shouldBe ReplayEvent.PlayerInput(Input.HardDrop, 0L)
  }

  "ReplayBuilder.recordPieceSpawn" should "add PieceSpawn event" in {
    val builder = createBuilder()
      .recordPieceSpawn(TetrominoShape.O)

    val finalState = createFinalState()
    val replay     = builder.build(finalState, TestValues.EndTimestamp)

    replay.events.size shouldBe 1
    replay.events.head shouldBe ReplayEvent.PieceSpawn(TetrominoShape.O, 0L)
  }

  "ReplayBuilder.advanceFrame" should "increment frame counter" in {
    val builder = createBuilder()
      .recordInput(Input.MoveLeft)
      .advanceFrame
      .advanceFrame
      .recordInput(Input.MoveRight)

    val finalState = createFinalState()
    val replay     = builder.build(finalState, TestValues.EndTimestamp)

    replay.events.size shouldBe 2
    replay.events(0) shouldBe ReplayEvent.PlayerInput(Input.MoveLeft, 0L)
    replay.events(1) shouldBe ReplayEvent.PlayerInput(Input.MoveRight, 2L)
  }

  "ReplayBuilder.build" should "create ReplayData with correct metadata" in {
    val builder    = createBuilder()
    val finalState = createFinalState()
    val replay     = builder.build(finalState, TestValues.EndTimestamp)

    replay.metadata.version shouldBe ReplayMetadata.CurrentVersion
    replay.metadata.finalScore shouldBe TestValues.FinalScore
    replay.metadata.finalLevel shouldBe TestValues.FinalLevel
    replay.metadata.finalLinesCleared shouldBe TestValues.LinesCleared
    replay.metadata.durationMs shouldBe (TestValues.EndTimestamp - TestValues.Timestamp)
  }

  it should "record mixed events correctly" in {
    val builder = createBuilder()
      .recordInput(Input.MoveLeft)
      .advanceFrame
      .recordPieceSpawn(TetrominoShape.O)
      .recordInput(Input.HardDrop)
      .advanceFrame
      .advanceFrame
      .recordInput(Input.Tick)

    val finalState = createFinalState()
    val replay     = builder.build(finalState, TestValues.EndTimestamp)

    replay.events.size shouldBe 4
    replay.events(0) shouldBe ReplayEvent.PlayerInput(Input.MoveLeft, 0L)
    replay.events(1) shouldBe ReplayEvent.PieceSpawn(TetrominoShape.O, 1L)
    replay.events(2) shouldBe ReplayEvent.PlayerInput(Input.HardDrop, 1L)
    replay.events(3) shouldBe ReplayEvent.PlayerInput(Input.Tick, 3L)
  }

  "ReplayBuilder" should "be immutable" in {
    val builder1 = createBuilder()
    val builder2 = builder1.recordInput(Input.MoveLeft)
    val builder3 = builder1.recordInput(Input.MoveRight)

    val finalState = createFinalState()

    val replay1 = builder1.build(finalState, TestValues.EndTimestamp)
    val replay2 = builder2.build(finalState, TestValues.EndTimestamp)
    val replay3 = builder3.build(finalState, TestValues.EndTimestamp)

    replay1.events shouldBe empty
    replay2.events.size shouldBe 1
    replay2.events.head shouldBe ReplayEvent.PlayerInput(Input.MoveLeft, 0L)
    replay3.events.size shouldBe 1
    replay3.events.head shouldBe ReplayEvent.PlayerInput(Input.MoveRight, 0L)
  }

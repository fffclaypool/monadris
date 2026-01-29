package monadris.domain.replay

import monadris.domain.Input
import monadris.domain.TetrominoShape

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReplayDataSpec extends AnyFlatSpec with Matchers:

  private object TestValues:
    val FrameNumber: Long = 100L
    val Timestamp: Long   = 1706500000000L
    val GridWidth: Int    = 10
    val GridHeight: Int   = 20
    val FinalScore: Int   = 1250
    val FinalLevel: Int   = 3
    val LinesCleared: Int = 12
    val DurationMs: Long  = 180000L

  "ReplayEvent.PlayerInput" should "store input and frame number" in {
    val event = ReplayEvent.PlayerInput(Input.MoveLeft, TestValues.FrameNumber)
    event match
      case ReplayEvent.PlayerInput(input, frame) =>
        input shouldBe Input.MoveLeft
        frame shouldBe TestValues.FrameNumber
      case _ => fail("Expected PlayerInput")
  }

  it should "support all input types" in {
    val inputs = List(
      Input.MoveLeft,
      Input.MoveRight,
      Input.MoveDown,
      Input.RotateClockwise,
      Input.RotateCounterClockwise,
      Input.HardDrop,
      Input.Pause,
      Input.Tick
    )
    inputs.foreach { expectedInput =>
      val event = ReplayEvent.PlayerInput(expectedInput, 0L)
      event match
        case ReplayEvent.PlayerInput(input, _) =>
          input shouldBe expectedInput
        case _ => fail("Expected PlayerInput")
    }
  }

  "ReplayEvent.PieceSpawn" should "store shape and frame number" in {
    val event = ReplayEvent.PieceSpawn(TetrominoShape.T, TestValues.FrameNumber)
    event match
      case ReplayEvent.PieceSpawn(shape, frame) =>
        shape shouldBe TetrominoShape.T
        frame shouldBe TestValues.FrameNumber
      case _ => fail("Expected PieceSpawn")
  }

  it should "support all tetromino shapes" in
    TetrominoShape.values.foreach { expectedShape =>
      val event = ReplayEvent.PieceSpawn(expectedShape, 0L)
      event match
        case ReplayEvent.PieceSpawn(shape, _) =>
          shape shouldBe expectedShape
        case _ => fail("Expected PieceSpawn")
    }

  "ReplayMetadata" should "store all metadata fields" in {
    val metadata = ReplayMetadata(
      version = ReplayMetadata.CurrentVersion,
      timestamp = TestValues.Timestamp,
      gridWidth = TestValues.GridWidth,
      gridHeight = TestValues.GridHeight,
      initialPiece = TetrominoShape.I,
      nextPiece = TetrominoShape.O,
      finalScore = TestValues.FinalScore,
      finalLevel = TestValues.FinalLevel,
      finalLinesCleared = TestValues.LinesCleared,
      durationMs = TestValues.DurationMs
    )

    metadata.version shouldBe ReplayMetadata.CurrentVersion
    metadata.timestamp shouldBe TestValues.Timestamp
    metadata.gridWidth shouldBe TestValues.GridWidth
    metadata.gridHeight shouldBe TestValues.GridHeight
    metadata.initialPiece shouldBe TetrominoShape.I
    metadata.nextPiece shouldBe TetrominoShape.O
    metadata.finalScore shouldBe TestValues.FinalScore
    metadata.finalLevel shouldBe TestValues.FinalLevel
    metadata.finalLinesCleared shouldBe TestValues.LinesCleared
    metadata.durationMs shouldBe TestValues.DurationMs
  }

  "ReplayMetadata.CurrentVersion" should "be defined" in {
    ReplayMetadata.CurrentVersion should not be empty
  }

  "ReplayData" should "store metadata and events" in {
    val metadata = ReplayMetadata(
      version = ReplayMetadata.CurrentVersion,
      timestamp = TestValues.Timestamp,
      gridWidth = TestValues.GridWidth,
      gridHeight = TestValues.GridHeight,
      initialPiece = TetrominoShape.T,
      nextPiece = TetrominoShape.I,
      finalScore = TestValues.FinalScore,
      finalLevel = TestValues.FinalLevel,
      finalLinesCleared = TestValues.LinesCleared,
      durationMs = TestValues.DurationMs
    )
    val events = Vector(
      ReplayEvent.PlayerInput(Input.MoveLeft, 0L),
      ReplayEvent.PieceSpawn(TetrominoShape.O, 10L),
      ReplayEvent.PlayerInput(Input.HardDrop, 15L)
    )
    val replay = ReplayData(metadata, events)

    replay.metadata shouldBe metadata
    replay.events shouldBe events
  }

  "ReplayData.eventCount" should "return correct event count" in {
    val metadata = ReplayMetadata(
      version = ReplayMetadata.CurrentVersion,
      timestamp = TestValues.Timestamp,
      gridWidth = TestValues.GridWidth,
      gridHeight = TestValues.GridHeight,
      initialPiece = TetrominoShape.T,
      nextPiece = TetrominoShape.I,
      finalScore = 0,
      finalLevel = 1,
      finalLinesCleared = 0,
      durationMs = 0L
    )

    val emptyReplay = ReplayData(metadata, Vector.empty)
    emptyReplay.eventCount shouldBe 0

    val threeEventReplay = ReplayData(
      metadata,
      Vector(
        ReplayEvent.PlayerInput(Input.MoveLeft, 0L),
        ReplayEvent.PlayerInput(Input.MoveRight, 1L),
        ReplayEvent.PieceSpawn(TetrominoShape.S, 2L)
      )
    )
    threeEventReplay.eventCount shouldBe 3
  }

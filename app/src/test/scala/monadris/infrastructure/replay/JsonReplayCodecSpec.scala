package monadris.infrastructure.replay

import zio.*
import zio.test.*
import zio.test.Assertion.*

import monadris.domain.Input
import monadris.domain.TetrominoShape
import monadris.domain.replay.*

object JsonReplayCodecSpec extends ZIOSpecDefault:

  private object TestValues:
    val Timestamp: Long   = 1706500000000L
    val GridWidth: Int    = 10
    val GridHeight: Int   = 20
    val FinalScore: Int   = 1250
    val FinalLevel: Int   = 3
    val LinesCleared: Int = 12
    val DurationMs: Long  = 180000L

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
      finalScore = TestValues.FinalScore,
      finalLevel = TestValues.FinalLevel,
      finalLinesCleared = TestValues.LinesCleared,
      durationMs = TestValues.DurationMs
    )

  def spec = suite("JsonReplayCodec")(
    suite("encode")(
      test("encodes empty replay data") {
        val metadata = createMetadata()
        val replay   = ReplayData(metadata, Vector.empty)

        val result = JsonReplayCodec.encode(replay)

        assertTrue(result.isRight)
      },
      test("encodes replay with PlayerInput events") {
        val metadata = createMetadata()
        val events   = Vector(
          ReplayEvent.PlayerInput(Input.MoveLeft, 0L),
          ReplayEvent.PlayerInput(Input.MoveRight, 10L)
        )
        val replay = ReplayData(metadata, events)

        val result = JsonReplayCodec.encode(replay)

        assertTrue(
          result.isRight,
          result.getOrElse("").contains("PlayerInput"),
          result.getOrElse("").contains("MoveLeft"),
          result.getOrElse("").contains("MoveRight")
        )
      },
      test("encodes replay with PieceSpawn events") {
        val metadata = createMetadata()
        val events   = Vector(
          ReplayEvent.PieceSpawn(TetrominoShape.O, 5L),
          ReplayEvent.PieceSpawn(TetrominoShape.S, 15L)
        )
        val replay = ReplayData(metadata, events)

        val result = JsonReplayCodec.encode(replay)

        assertTrue(
          result.isRight,
          result.getOrElse("").contains("PieceSpawn")
        )
      },
      test("encodes all input types") {
        val metadata = createMetadata()
        val inputs   = List(
          Input.MoveLeft,
          Input.MoveRight,
          Input.MoveDown,
          Input.RotateClockwise,
          Input.RotateCounterClockwise,
          Input.HardDrop,
          Input.Pause,
          Input.Tick
        )
        val events = inputs.zipWithIndex.map { case (input, i) =>
          ReplayEvent.PlayerInput(input, i.toLong)
        }.toVector
        val replay = ReplayData(metadata, events)

        val result = JsonReplayCodec.encode(replay)

        assertTrue(result.isRight)
      },
      test("encodes all tetromino shapes") {
        val metadata = createMetadata()
        val events   = TetrominoShape.values.zipWithIndex.map { case (shape, i) =>
          ReplayEvent.PieceSpawn(shape, i.toLong)
        }.toVector
        val replay = ReplayData(metadata, events)

        val result = JsonReplayCodec.encode(replay)

        assertTrue(result.isRight)
      }
    ),
    suite("decode")(
      test("decodes empty replay data") {
        val metadata = createMetadata()
        val replay   = ReplayData(metadata, Vector.empty)

        val encoded = JsonReplayCodec.encode(replay)
        val decoded = encoded.flatMap(JsonReplayCodec.decode)

        assertTrue(
          decoded.isRight,
          decoded.map(_.eventCount) == Right(0)
        )
      },
      test("decodes replay with events") {
        val metadata = createMetadata()
        val events   = Vector(
          ReplayEvent.PlayerInput(Input.MoveLeft, 0L),
          ReplayEvent.PieceSpawn(TetrominoShape.O, 10L)
        )
        val replay = ReplayData(metadata, events)

        val encoded = JsonReplayCodec.encode(replay)
        val decoded = encoded.flatMap(JsonReplayCodec.decode)

        assertTrue(
          decoded.isRight,
          decoded.map(_.eventCount) == Right(2)
        )
      },
      test("returns error for invalid JSON") {
        val result = JsonReplayCodec.decode("not valid json")

        assertTrue(result.isLeft)
      },
      test("returns error for missing fields") {
        val result = JsonReplayCodec.decode("{}")

        assertTrue(result.isLeft)
      }
    ),
    suite("roundtrip")(
      test("preserves metadata through encode/decode") {
        val metadata = createMetadata(
          initialPiece = TetrominoShape.Z,
          nextPiece = TetrominoShape.L
        )
        val replay = ReplayData(metadata, Vector.empty)

        val encoded = JsonReplayCodec.encode(replay)
        val decoded = encoded.flatMap(JsonReplayCodec.decode)

        assertTrue(
          decoded.isRight,
          decoded.map(_.metadata) == Right(metadata)
        )
      },
      test("preserves events through encode/decode") {
        val metadata = createMetadata()
        val events   = Vector(
          ReplayEvent.PlayerInput(Input.MoveLeft, 0L),
          ReplayEvent.PlayerInput(Input.MoveRight, 5L),
          ReplayEvent.PieceSpawn(TetrominoShape.O, 10L),
          ReplayEvent.PlayerInput(Input.HardDrop, 15L)
        )
        val replay = ReplayData(metadata, events)

        val encoded = JsonReplayCodec.encode(replay)
        val decoded = encoded.flatMap(JsonReplayCodec.decode)

        assertTrue(
          decoded.isRight,
          decoded.map(_.events) == Right(events)
        )
      },
      test("preserves all input types through roundtrip") {
        val metadata = createMetadata()
        val inputs   = List(
          Input.MoveLeft,
          Input.MoveRight,
          Input.MoveDown,
          Input.RotateClockwise,
          Input.RotateCounterClockwise,
          Input.HardDrop,
          Input.Pause,
          Input.Tick,
          Input.Quit
        )
        val events = inputs.zipWithIndex.map { case (input, i) =>
          ReplayEvent.PlayerInput(input, i.toLong)
        }.toVector
        val replay = ReplayData(metadata, events)

        val encoded = JsonReplayCodec.encode(replay)
        val decoded = encoded.flatMap(JsonReplayCodec.decode)

        assertTrue(
          decoded.isRight,
          decoded.map(_.events) == Right(events)
        )
      },
      test("preserves all tetromino shapes through roundtrip") {
        val metadata = createMetadata()
        val events   = TetrominoShape.values.zipWithIndex.map { case (shape, i) =>
          ReplayEvent.PieceSpawn(shape, i.toLong)
        }.toVector
        val replay = ReplayData(metadata, events)

        val encoded = JsonReplayCodec.encode(replay)
        val decoded = encoded.flatMap(JsonReplayCodec.decode)

        assertTrue(
          decoded.isRight,
          decoded.map(_.events) == Right(events)
        )
      }
    )
  )

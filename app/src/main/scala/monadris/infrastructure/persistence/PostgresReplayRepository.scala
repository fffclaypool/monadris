package monadris.infrastructure.persistence

import java.time.Instant
import java.time.OffsetDateTime
import java.time.ZoneOffset
import java.util.UUID

import zio.*
import zio.json.*

import monadris.domain.Input
import monadris.domain.TetrominoShape
import monadris.replay.*

import io.getquill.*
import io.getquill.jdbczio.Quill

final case class GameResultRow(
  id: UUID,
  replayName: String,
  timestamp: OffsetDateTime,
  finalScore: Int,
  finalLevel: Int,
  linesCleared: Int,
  durationMs: Long,
  gridWidth: Int,
  gridHeight: Int,
  version: String,
  createdAt: OffsetDateTime
)

final case class ReplayRow(
  id: UUID,
  gameResultId: UUID,
  initialPiece: String,
  nextPiece: String,
  events: String,
  eventCount: Int,
  createdAt: OffsetDateTime
)

final class PostgresReplayRepository(
  quill: Quill.Postgres[SnakeCase]
) extends ReplayRepository:

  import quill.*

  private inline def gameResults = quote(querySchema[GameResultRow]("game_results"))
  private inline def replays     = quote(querySchema[ReplayRow]("replays"))

  override def save(name: String, replay: ReplayData): Task[Unit] =
    for
      eventsJson <- encodeEvents(replay.events)
      now           = OffsetDateTime.now(ZoneOffset.UTC)
      gameResultId  = UUID.randomUUID()
      gameResultRow = GameResultRow(
        id = gameResultId,
        replayName = name,
        timestamp = OffsetDateTime.ofInstant(Instant.ofEpochMilli(replay.metadata.timestamp), ZoneOffset.UTC),
        finalScore = replay.metadata.finalScore,
        finalLevel = replay.metadata.finalLevel,
        linesCleared = replay.metadata.finalLinesCleared,
        durationMs = replay.metadata.durationMs,
        gridWidth = replay.metadata.gridWidth,
        gridHeight = replay.metadata.gridHeight,
        version = replay.metadata.version,
        createdAt = now
      )
      replayRow = ReplayRow(
        id = UUID.randomUUID(),
        gameResultId = gameResultId,
        initialPiece = replay.metadata.initialPiece.toString,
        nextPiece = replay.metadata.nextPiece.toString,
        events = eventsJson,
        eventCount = replay.eventCount,
        createdAt = now
      )
      _ <- transaction {
        for
          _ <- run(gameResults.insertValue(lift(gameResultRow)))
          _ <- run(replays.insertValue(lift(replayRow)))
        yield ()
      }
    yield ()

  override def load(name: String): Task[ReplayData] =
    for
      gameResultOpt <- run(gameResults.filter(_.replayName == lift(name))).map(_.headOption)
      gameResult    <- ZIO.fromOption(gameResultOpt).orElseFail(new RuntimeException(s"Replay not found: $name"))
      replayRowOpt  <- run(replays.filter(_.gameResultId == lift(gameResult.id))).map(_.headOption)
      replayRow     <- ZIO.fromOption(replayRowOpt).orElseFail(new RuntimeException(s"Replay data not found: $name"))
      events        <- decodeEvents(replayRow.events)
      initialPiece  <- parseShape(replayRow.initialPiece)
      nextPiece     <- parseShape(replayRow.nextPiece)
      metadata = ReplayMetadata(
        version = gameResult.version,
        timestamp = gameResult.timestamp.toInstant.toEpochMilli,
        gridWidth = gameResult.gridWidth,
        gridHeight = gameResult.gridHeight,
        initialPiece = initialPiece,
        nextPiece = nextPiece,
        finalScore = gameResult.finalScore,
        finalLevel = gameResult.finalLevel,
        finalLinesCleared = gameResult.linesCleared,
        durationMs = gameResult.durationMs
      )
    yield ReplayData(metadata, events)

  override def list: Task[Vector[String]] =
    run(gameResults.sortBy(_.timestamp)(Ord.desc).map(_.replayName)).map(_.toVector)

  override def exists(name: String): Task[Boolean] =
    run(gameResults.filter(_.replayName == lift(name)).nonEmpty)

  override def delete(name: String): Task[Unit] =
    run(gameResults.filter(_.replayName == lift(name)).delete).unit

  private def encodeEvents(events: Vector[ReplayEvent]): Task[String] =
    ZIO.succeed(events.toJson)

  private def decodeEvents(json: String): Task[Vector[ReplayEvent]] =
    ZIO
      .fromEither(json.fromJson[Vector[ReplayEvent]])
      .mapError(msg => new RuntimeException(s"Failed to decode events: $msg"))

  private def parseShape(s: String): Task[TetrominoShape] =
    ZIO
      .fromOption(TetrominoShape.values.find(_.toString == s))
      .orElseFail(new RuntimeException(s"Unknown tetromino shape: $s"))

  // JSON codecs for ReplayEvent
  private given JsonEncoder[TetrominoShape] = JsonEncoder[String].contramap(_.toString)
  private given JsonDecoder[TetrominoShape] = JsonDecoder[String].mapOrFail { s =>
    TetrominoShape.values.find(_.toString == s).toRight(s"Unknown shape: $s")
  }

  private given JsonEncoder[Input] = JsonEncoder[String].contramap(_.toString)
  private given JsonDecoder[Input] = JsonDecoder[String].mapOrFail { s =>
    Input.values.find(_.toString == s).toRight(s"Unknown input: $s")
  }

  final private case class PlayerInputJson(input: String, frameNumber: Long)
  private given JsonCodec[PlayerInputJson] = DeriveJsonCodec.gen[PlayerInputJson]

  final private case class PieceSpawnJson(shape: String, frameNumber: Long)
  private given JsonCodec[PieceSpawnJson] = DeriveJsonCodec.gen[PieceSpawnJson]

  final private case class ReplayEventJson(
    PlayerInput: Option[PlayerInputJson] = None,
    PieceSpawn: Option[PieceSpawnJson] = None
  )
  private given JsonCodec[ReplayEventJson] = DeriveJsonCodec.gen[ReplayEventJson]

  private given JsonEncoder[ReplayEvent] = JsonEncoder[ReplayEventJson].contramap {
    case ReplayEvent.PlayerInput(input, frameNumber) =>
      ReplayEventJson(PlayerInput = Some(PlayerInputJson(input.toString, frameNumber)))
    case ReplayEvent.PieceSpawn(shape, frameNumber) =>
      ReplayEventJson(PieceSpawn = Some(PieceSpawnJson(shape.toString, frameNumber)))
  }

  private given JsonDecoder[ReplayEvent] = JsonDecoder[ReplayEventJson].mapOrFail {
    case ReplayEventJson(Some(pi), _) =>
      Input.values.find(_.toString == pi.input) match
        case Some(input) => Right(ReplayEvent.PlayerInput(input, pi.frameNumber))
        case None        => Left(s"Unknown input: ${pi.input}")
    case ReplayEventJson(_, Some(ps)) =>
      TetrominoShape.values.find(_.toString == ps.shape) match
        case Some(shape) => Right(ReplayEvent.PieceSpawn(shape, ps.frameNumber))
        case None        => Left(s"Unknown shape: ${ps.shape}")
    case _ => Left("Invalid replay event: missing PlayerInput or PieceSpawn")
  }

object PostgresReplayRepository:

  val layer: ZLayer[Quill.Postgres[SnakeCase], Nothing, ReplayRepository] =
    ZLayer.fromFunction((quill: Quill.Postgres[SnakeCase]) => new PostgresReplayRepository(quill))

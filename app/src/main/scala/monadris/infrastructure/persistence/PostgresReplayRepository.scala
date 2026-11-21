package monadris.infrastructure.persistence

import java.time.Instant
import java.time.OffsetDateTime
import java.time.ZoneOffset
import java.util.UUID

import zio.*

import monadris.domain.TetrominoShape
import monadris.replay.*

import io.getquill.*
import io.getquill.jdbczio.Quill
import org.postgresql.util.PGobject

// Wrapper for JSONB type
opaque type JsonbString = String

object JsonbString:
  def apply(s: String): JsonbString            = s
  extension (j: JsonbString) def value: String = j

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
  events: JsonbString,
  eventCount: Int,
  createdAt: OffsetDateTime
)

final class PostgresReplayRepository(
  quill: Quill.Postgres[SnakeCase]
) extends ReplayRepository:

  import quill.*

  // Custom encoder/decoder for JSONB columns
  private given jsonbEncoder: Encoder[JsonbString] = encoder(
    java.sql.Types.OTHER,
    (index, value, row) =>
      val pgObj = new PGobject()
      pgObj.setType("jsonb")
      pgObj.setValue(value.value)
      row.setObject(index, pgObj)
  )

  private given jsonbDecoder: Decoder[JsonbString] = decoder(row => index => JsonbString(row.getObject(index).toString))

  private inline def gameResults = quote(querySchema[GameResultRow]("game_results"))
  private inline def replays     = quote(querySchema[ReplayRow]("replays"))

  override def save(name: String, replay: ReplayData): Task[Unit] =
    for
      now          <- Clock.currentDateTime.map(_.withOffsetSameInstant(ZoneOffset.UTC))
      gameResultId <- Random.nextUUID
      replayId     <- Random.nextUUID
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
        id = replayId,
        gameResultId = gameResultId,
        initialPiece = replay.metadata.initialPiece.toString,
        nextPiece = replay.metadata.nextPiece.toString,
        events = JsonbString(JsonReplayCodec.encodeEvents(replay.events)),
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
      events        <- decodeEvents(replayRow.events.value)
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

  private def decodeEvents(json: String): Task[Vector[ReplayEvent]] =
    ZIO
      .fromEither(JsonReplayCodec.decodeEvents(json))
      .mapError(msg => new RuntimeException(s"Failed to decode events: $msg"))

  private def parseShape(s: String): Task[TetrominoShape] =
    ZIO.fromEither(JsonReplayCodec.parseShape(s)).mapError(msg => new RuntimeException(msg))

object PostgresReplayRepository:

  val layer: ZLayer[Quill.Postgres[SnakeCase], Nothing, ReplayRepository] =
    ZLayer.fromFunction((quill: Quill.Postgres[SnakeCase]) => new PostgresReplayRepository(quill))

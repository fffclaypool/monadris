package monadris.infrastructure.persistence

import java.sql.SQLException
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
  import PostgresReplayRepository.SqlState

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

  override def save(name: String, replay: ReplayData): IO[ReplayError, Unit] =
    for
      validName    <- ZIO.fromEither(ReplayName.validate(name)).mapError(ReplayError.InvalidName(_))
      now          <- Clock.currentDateTime.map(_.withOffsetSameInstant(ZoneOffset.UTC))
      gameResultId <- Random.nextUUID
      replayId     <- Random.nextUUID
      gameResultRow = GameResultRow(
        id = gameResultId,
        replayName = validName,
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
      }.mapError {
        case e: SQLException if e.getSQLState == SqlState.UniqueViolation => ReplayError.AlreadyExists(validName)
        case other                                                        => ReplayError.StorageFailure(other)
      }
    yield ()

  override def load(name: String): IO[ReplayError, ReplayData] =
    for
      gameResultOpt <- query(run(gameResults.filter(_.replayName == lift(name)))).map(_.headOption)
      gameResult    <- ZIO.fromOption(gameResultOpt).orElseFail(ReplayError.NotFound(name))
      replayRowOpt  <- query(run(replays.filter(_.gameResultId == lift(gameResult.id)))).map(_.headOption)
      replayRow     <- ZIO.fromOption(replayRowOpt).orElseFail(ReplayError.Corrupted(s"no events stored for '$name'"))
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

  override def list: IO[ReplayError, Vector[String]] =
    query(run(gameResults.sortBy(_.timestamp)(Ord.desc).map(_.replayName))).map(_.toVector)

  override def exists(name: String): IO[ReplayError, Boolean] =
    query(run(gameResults.filter(_.replayName == lift(name)).nonEmpty))

  override def delete(name: String): IO[ReplayError, Unit] =
    query(run(gameResults.filter(_.replayName == lift(name)).delete)).unit

  private def query[A](effect: ZIO[Any, SQLException, A]): IO[ReplayError, A] =
    effect.mapError(ReplayError.StorageFailure(_))

  private def decodeEvents(json: String): IO[ReplayError, Vector[ReplayEvent]] =
    ZIO.fromEither(JsonReplayCodec.decodeEvents(json)).mapError(ReplayError.Corrupted(_))

  private def parseShape(s: String): IO[ReplayError, TetrominoShape] =
    ZIO.fromEither(JsonReplayCodec.parseShape(s)).mapError(ReplayError.Corrupted(_))

object PostgresReplayRepository:

  private[persistence] object SqlState:
    /** PostgreSQL error code for a violated UNIQUE constraint */
    val UniqueViolation = "23505"

  val layer: ZLayer[Quill.Postgres[SnakeCase], Nothing, ReplayRepository] =
    ZLayer.fromFunction((quill: Quill.Postgres[SnakeCase]) => new PostgresReplayRepository(quill))

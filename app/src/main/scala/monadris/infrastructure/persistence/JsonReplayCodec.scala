package monadris.infrastructure.persistence

import zio.json.*

import monadris.domain.Input
import monadris.domain.TetrominoShape
import monadris.replay.*

/** The single JSON representation of replays, shared by file and database storage. */
object JsonReplayCodec extends ReplayCodec:

  def parseShape(s: String): Either[String, TetrominoShape] =
    TetrominoShape.values.find(_.toString == s).toRight(s"Unknown shape: $s")

  def parseInput(s: String): Either[String, Input] =
    Input.values.find(_.toString == s).toRight(s"Unknown input: $s")

  given JsonEncoder[TetrominoShape] = JsonEncoder[String].contramap(_.toString)
  given JsonDecoder[TetrominoShape] = JsonDecoder[String].mapOrFail(parseShape)

  // `elapsedMs` is optional so that replays recorded before version 1.1 still load.
  final private case class PlayerInputJson(input: String, frameNumber: Long, elapsedMs: Option[Long] = None)
  private given JsonCodec[PlayerInputJson] = DeriveJsonCodec.gen[PlayerInputJson]

  final private case class PieceSpawnJson(shape: String, frameNumber: Long, elapsedMs: Option[Long] = None)
  private given JsonCodec[PieceSpawnJson] = DeriveJsonCodec.gen[PieceSpawnJson]

  // Wrapper case class for ReplayEvent discriminated union
  final private case class ReplayEventJson(
    PlayerInput: Option[PlayerInputJson] = None,
    PieceSpawn: Option[PieceSpawnJson] = None
  )
  private given JsonCodec[ReplayEventJson] = DeriveJsonCodec.gen[ReplayEventJson]

  private def elapsedOrLegacy(elapsedMs: Option[Long], frameNumber: Long): Long =
    elapsedMs.getOrElse(ReplayEvent.legacyElapsedMs(frameNumber))

  given JsonEncoder[ReplayEvent] = JsonEncoder[ReplayEventJson].contramap {
    case ReplayEvent.PlayerInput(input, frameNumber, elapsedMs) =>
      ReplayEventJson(PlayerInput = Some(PlayerInputJson(input.toString, frameNumber, Some(elapsedMs))))
    case ReplayEvent.PieceSpawn(shape, frameNumber, elapsedMs) =>
      ReplayEventJson(PieceSpawn = Some(PieceSpawnJson(shape.toString, frameNumber, Some(elapsedMs))))
  }

  given JsonDecoder[ReplayEvent] = JsonDecoder[ReplayEventJson].mapOrFail {
    case ReplayEventJson(Some(pi), _) =>
      parseInput(pi.input).map { input =>
        ReplayEvent.PlayerInput(input, pi.frameNumber, elapsedOrLegacy(pi.elapsedMs, pi.frameNumber))
      }
    case ReplayEventJson(_, Some(ps)) =>
      parseShape(ps.shape).map { shape =>
        ReplayEvent.PieceSpawn(shape, ps.frameNumber, elapsedOrLegacy(ps.elapsedMs, ps.frameNumber))
      }
    case _ => Left("Invalid replay event: missing PlayerInput or PieceSpawn")
  }

  given JsonCodec[ReplayMetadata] = DeriveJsonCodec.gen[ReplayMetadata]

  given JsonCodec[ReplayData] = DeriveJsonCodec.gen[ReplayData]

  def encodeEvents(events: Vector[ReplayEvent]): String =
    events.toJson

  def decodeEvents(json: String): Either[String, Vector[ReplayEvent]] =
    json.fromJson[Vector[ReplayEvent]]

  override def encode(replay: ReplayData): Either[String, String] =
    Right(replay.toJsonPretty)

  override def decode(data: String): Either[String, ReplayData] =
    data.fromJson[ReplayData]

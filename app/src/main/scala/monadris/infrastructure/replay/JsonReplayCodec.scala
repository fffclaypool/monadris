package monadris.infrastructure.replay

import zio.json.*

import monadris.domain.Input
import monadris.domain.TetrominoShape
import monadris.domain.replay.*

object JsonReplayCodec extends ReplayCodec:

  // TetrominoShape codec
  given JsonEncoder[TetrominoShape] = JsonEncoder[String].contramap(_.toString)
  given JsonDecoder[TetrominoShape] = JsonDecoder[String].mapOrFail { s =>
    TetrominoShape.values.find(_.toString == s).toRight(s"Unknown shape: $s")
  }

  // Input codec
  given JsonEncoder[Input] = JsonEncoder[String].contramap(_.toString)
  given JsonDecoder[Input] = JsonDecoder[String].mapOrFail { s =>
    Input.values.find(_.toString == s).toRight(s"Unknown input: $s")
  }

  // PlayerInput helper case class for JSON
  final private case class PlayerInputJson(input: String, frameNumber: Long)
  private given JsonCodec[PlayerInputJson] = DeriveJsonCodec.gen[PlayerInputJson]

  // PieceSpawn helper case class for JSON
  final private case class PieceSpawnJson(shape: String, frameNumber: Long)
  private given JsonCodec[PieceSpawnJson] = DeriveJsonCodec.gen[PieceSpawnJson]

  // ReplayEvent wrapper case class for discriminated union
  final private case class ReplayEventJson(
    PlayerInput: Option[PlayerInputJson] = None,
    PieceSpawn: Option[PieceSpawnJson] = None
  )
  private given JsonCodec[ReplayEventJson] = DeriveJsonCodec.gen[ReplayEventJson]

  given JsonEncoder[ReplayEvent] = JsonEncoder[ReplayEventJson].contramap {
    case ReplayEvent.PlayerInput(input, frameNumber) =>
      ReplayEventJson(PlayerInput = Some(PlayerInputJson(input.toString, frameNumber)))
    case ReplayEvent.PieceSpawn(shape, frameNumber) =>
      ReplayEventJson(PieceSpawn = Some(PieceSpawnJson(shape.toString, frameNumber)))
  }

  given JsonDecoder[ReplayEvent] = JsonDecoder[ReplayEventJson].mapOrFail {
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

  // ReplayMetadata codec
  given JsonCodec[ReplayMetadata] = DeriveJsonCodec.gen[ReplayMetadata]

  // ReplayData codec
  given JsonCodec[ReplayData] = DeriveJsonCodec.gen[ReplayData]

  override def encode(replay: ReplayData): Either[String, String] =
    Right(replay.toJsonPretty)

  override def decode(data: String): Either[String, ReplayData] =
    data.fromJson[ReplayData]

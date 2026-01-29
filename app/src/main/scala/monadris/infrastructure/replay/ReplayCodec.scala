package monadris.infrastructure.replay

import monadris.domain.replay.ReplayData

trait ReplayCodec:
  def encode(replay: ReplayData): Either[String, String]
  def decode(data: String): Either[String, ReplayData]

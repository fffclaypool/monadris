package monadris.infrastructure.persistence

import monadris.replay.ReplayData

trait ReplayCodec:
  def encode(replay: ReplayData): Either[String, String]
  def decode(data: String): Either[String, ReplayData]

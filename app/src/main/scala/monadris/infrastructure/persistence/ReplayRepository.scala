package monadris.infrastructure.persistence

import zio.*

import monadris.replay.ReplayData

trait ReplayRepository:
  /** Fails with [[ReplayError.AlreadyExists]] rather than overwriting an existing replay */
  def save(name: String, replay: ReplayData): IO[ReplayError, Unit]
  def load(name: String): IO[ReplayError, ReplayData]

  /** Replay names, newest first */
  def list: IO[ReplayError, Vector[String]]
  def exists(name: String): IO[ReplayError, Boolean]
  def delete(name: String): IO[ReplayError, Unit]

object ReplayRepository:
  def save(name: String, replay: ReplayData): ZIO[ReplayRepository, ReplayError, Unit] =
    ZIO.serviceWithZIO(_.save(name, replay))

  def load(name: String): ZIO[ReplayRepository, ReplayError, ReplayData] =
    ZIO.serviceWithZIO(_.load(name))

  def list: ZIO[ReplayRepository, ReplayError, Vector[String]] =
    ZIO.serviceWithZIO(_.list)

  def exists(name: String): ZIO[ReplayRepository, ReplayError, Boolean] =
    ZIO.serviceWithZIO(_.exists(name))

  def delete(name: String): ZIO[ReplayRepository, ReplayError, Unit] =
    ZIO.serviceWithZIO(_.delete(name))

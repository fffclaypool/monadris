package monadris.infrastructure.persistence

import zio.*

import monadris.replay.ReplayData

trait ReplayRepository:
  def save(name: String, replay: ReplayData): Task[Unit]
  def load(name: String): Task[ReplayData]
  def list: Task[Vector[String]]
  def exists(name: String): Task[Boolean]
  def delete(name: String): Task[Unit]

object ReplayRepository:
  def save(name: String, replay: ReplayData): ZIO[ReplayRepository, Throwable, Unit] =
    ZIO.serviceWithZIO(_.save(name, replay))

  def load(name: String): ZIO[ReplayRepository, Throwable, ReplayData] =
    ZIO.serviceWithZIO(_.load(name))

  def list: ZIO[ReplayRepository, Throwable, Vector[String]] =
    ZIO.serviceWithZIO(_.list)

  def exists(name: String): ZIO[ReplayRepository, Throwable, Boolean] =
    ZIO.serviceWithZIO(_.exists(name))

  def delete(name: String): ZIO[ReplayRepository, Throwable, Unit] =
    ZIO.serviceWithZIO(_.delete(name))

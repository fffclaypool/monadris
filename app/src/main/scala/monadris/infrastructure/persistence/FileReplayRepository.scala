package monadris.infrastructure.persistence

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import scala.jdk.CollectionConverters.*

import zio.*

import monadris.replay.ReplayData

final class FileReplayRepository(
  replayDir: Path,
  codec: ReplayCodec
) extends ReplayRepository:

  private object FileExtension:
    val ReplayJson = ".replay.json"

  override def save(name: String, replay: ReplayData): Task[Unit] =
    for
      _       <- ensureDirectoryExists
      encoded <- ZIO.fromEither(codec.encode(replay)).mapError(msg => new RuntimeException(msg))
      filePath = replayDir.resolve(s"$name${FileExtension.ReplayJson}")
      _ <- ZIO.attemptBlocking(Files.writeString(filePath, encoded))
    yield ()

  override def load(name: String): Task[ReplayData] =
    for
      filePath <- ZIO.succeed(replayDir.resolve(s"$name${FileExtension.ReplayJson}"))
      _        <- ZIO
        .fail(new RuntimeException(s"Replay not found: $name"))
        .when(!Files.exists(filePath))
      content <- ZIO.attemptBlocking(Files.readString(filePath))
      replay  <- ZIO.fromEither(codec.decode(content)).mapError(msg => new RuntimeException(msg))
    yield replay

  override def list: Task[Vector[String]] =
    for
      exists <- ZIO.attemptBlocking(Files.exists(replayDir))
      names  <- if exists then listReplayFiles else ZIO.succeed(Vector.empty)
    yield names

  private def listReplayFiles: Task[Vector[String]] =
    ZIO.attemptBlocking {
      val stream = Files.list(replayDir)
      try
        val iterator: java.util.Iterator[Path] = stream.iterator()
        iterator.asScala.toVector
          .filter(_.toString.endsWith(FileExtension.ReplayJson))
          .map(_.getFileName.toString.stripSuffix(FileExtension.ReplayJson))
          .sorted
      finally stream.close()
    }

  override def exists(name: String): Task[Boolean] =
    val filePath = replayDir.resolve(s"$name${FileExtension.ReplayJson}")
    ZIO.attemptBlocking(Files.exists(filePath))

  override def delete(name: String): Task[Unit] =
    val filePath = replayDir.resolve(s"$name${FileExtension.ReplayJson}")
    ZIO.attemptBlocking(Files.deleteIfExists(filePath)).unit

  private val ensureDirectoryExists: Task[Unit] =
    ZIO.attemptBlocking {
      if !Files.exists(replayDir) then Files.createDirectories(replayDir)
    }.unit

object FileReplayRepository:
  private object Defaults:
    val DirName      = ".monadris"
    val ReplaySubdir = "replays"

  val layer: ZLayer[Any, Nothing, ReplayRepository] =
    ZLayer.succeed {
      val homeDir   = java.lang.System.getProperty("user.home")
      val replayDir = Paths.get(homeDir, Defaults.DirName, Defaults.ReplaySubdir)
      new FileReplayRepository(replayDir, JsonReplayCodec: ReplayCodec)
    }

  def make(replayDir: Path): ReplayRepository =
    new FileReplayRepository(replayDir, JsonReplayCodec: ReplayCodec)

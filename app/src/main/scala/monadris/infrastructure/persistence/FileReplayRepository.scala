package monadris.infrastructure.persistence

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import scala.jdk.CollectionConverters.*

import zio.*

import monadris.replay.ReplayData
import monadris.replay.ReplayName

final class FileReplayRepository(
  replayDir: Path,
  codec: ReplayCodec
) extends ReplayRepository:

  private object FileExtension:
    val ReplayJson = ".replay.json"

  override def save(name: String, replay: ReplayData): Task[Unit] =
    for
      filePath <- resolvePath(name)
      _        <- ensureDirectoryExists
      encoded  <- ZIO.fromEither(codec.encode(replay)).mapError(msg => new RuntimeException(msg))
      _        <- ZIO.attemptBlocking(Files.writeString(filePath, encoded))
    yield ()

  override def load(name: String): Task[ReplayData] =
    for
      filePath <- resolvePath(name)
      found    <- ZIO.attemptBlocking(Files.exists(filePath))
      _        <- ZIO.fail(new RuntimeException(s"Replay not found: $name")).unless(found)
      content  <- ZIO.attemptBlocking(Files.readString(filePath))
      replay   <- ZIO.fromEither(codec.decode(content)).mapError(msg => new RuntimeException(msg))
    yield replay

  /** Newest first (by modification time), matching PostgresReplayRepository. */
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
          .map(path => (replayNameOf(path), Files.getLastModifiedTime(path).toMillis))
          .sortBy { case (name, modifiedAt) => (-modifiedAt, name) }
          .map { case (name, _) => name }
      finally stream.close()
    }

  override def exists(name: String): Task[Boolean] =
    resolvePath(name).flatMap(filePath => ZIO.attemptBlocking(Files.exists(filePath)))

  override def delete(name: String): Task[Unit] =
    resolvePath(name).flatMap(filePath => ZIO.attemptBlocking(Files.deleteIfExists(filePath))).unit

  private def replayNameOf(path: Path): String =
    path.getFileName.toString.stripSuffix(FileExtension.ReplayJson)

  /** Rejects names that could escape the replay directory (e.g. "../x"). */
  private def resolvePath(name: String): Task[Path] =
    ZIO
      .fromEither(ReplayName.validate(name))
      .mapBoth(
        msg => new IllegalArgumentException(msg),
        validName => replayDir.resolve(s"$validName${FileExtension.ReplayJson}")
      )

  private val ensureDirectoryExists: Task[Unit] =
    ZIO.attemptBlocking(Files.createDirectories(replayDir)).unit

object FileReplayRepository:
  private object Defaults:
    val DirName      = ".monadris"
    val ReplaySubdir = "replays"
    val HomeProperty = "user.home"

  val layer: ZLayer[Any, Throwable, ReplayRepository] =
    ZLayer.fromZIO {
      ZIO
        .attempt(java.lang.System.getProperty(Defaults.HomeProperty))
        .map(homeDir => make(Paths.get(homeDir, Defaults.DirName, Defaults.ReplaySubdir)))
    }

  def make(replayDir: Path): ReplayRepository =
    new FileReplayRepository(replayDir, JsonReplayCodec: ReplayCodec)

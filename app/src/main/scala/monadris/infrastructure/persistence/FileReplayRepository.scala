package monadris.infrastructure.persistence

import java.nio.file.FileAlreadyExistsException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardOpenOption

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

  override def save(name: String, replay: ReplayData): IO[ReplayError, Unit] =
    for
      filePath <- resolvePath(name)
      _        <- blockingIO(Files.createDirectories(replayDir))
      encoded  <- ZIO.fromEither(codec.encode(replay)).mapError(ReplayError.Corrupted(_))
      // CREATE_NEW makes "does not exist yet" and "write" a single atomic step.
      _ <- ZIO
        .attemptBlocking(Files.writeString(filePath, encoded, StandardOpenOption.CREATE_NEW))
        .mapError {
          case _: FileAlreadyExistsException => ReplayError.AlreadyExists(name)
          case other                         => ReplayError.StorageFailure(other)
        }
    yield ()

  override def load(name: String): IO[ReplayError, ReplayData] =
    for
      filePath <- resolvePath(name)
      found    <- blockingIO(Files.exists(filePath))
      _        <- ZIO.fail(ReplayError.NotFound(name)).unless(found)
      content  <- blockingIO(Files.readString(filePath))
      replay   <- ZIO.fromEither(codec.decode(content)).mapError(ReplayError.Corrupted(_))
    yield replay

  /** Newest first (by modification time), matching PostgresReplayRepository. */
  override def list: IO[ReplayError, Vector[String]] =
    for
      exists <- blockingIO(Files.exists(replayDir))
      names  <- if exists then listReplayFiles else ZIO.succeed(Vector.empty)
    yield names

  private def listReplayFiles: IO[ReplayError, Vector[String]] =
    blockingIO {
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

  override def exists(name: String): IO[ReplayError, Boolean] =
    resolvePath(name).flatMap(filePath => blockingIO(Files.exists(filePath)))

  override def delete(name: String): IO[ReplayError, Unit] =
    resolvePath(name).flatMap(filePath => blockingIO(Files.deleteIfExists(filePath))).unit

  private def replayNameOf(path: Path): String =
    path.getFileName.toString.stripSuffix(FileExtension.ReplayJson)

  /** Rejects names that could escape the replay directory (e.g. "../x"). */
  private def resolvePath(name: String): IO[ReplayError, Path] =
    ZIO
      .fromEither(ReplayName.validate(name))
      .mapBoth(
        ReplayError.InvalidName(_),
        validName => replayDir.resolve(s"$validName${FileExtension.ReplayJson}")
      )

  private def blockingIO[A](effect: => A): IO[ReplayError, A] =
    ZIO.attemptBlocking(effect).mapError(ReplayError.StorageFailure(_))

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

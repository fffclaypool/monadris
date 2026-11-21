package monadris.infrastructure.persistence

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.FileTime

import zio.*
import zio.test.*
import zio.test.Assertion.*

import monadris.domain.Input
import monadris.domain.TetrominoShape
import monadris.replay.*

object FileReplayRepositorySpec extends ZIOSpecDefault:

  private object TestValues:
    val Timestamp: Long   = 1706500000000L
    val GridWidth: Int    = 10
    val GridHeight: Int   = 20
    val FinalScore: Int   = 500
    val FinalLevel: Int   = 2
    val LinesCleared: Int = 5
    val DurationMs: Long  = 60000L

  private def createReplayData(name: String = "test"): ReplayData =
    val metadata = ReplayMetadata(
      version = ReplayMetadata.CurrentVersion,
      timestamp = TestValues.Timestamp,
      gridWidth = TestValues.GridWidth,
      gridHeight = TestValues.GridHeight,
      initialPiece = TetrominoShape.T,
      nextPiece = TetrominoShape.I,
      finalScore = TestValues.FinalScore,
      finalLevel = TestValues.FinalLevel,
      finalLinesCleared = TestValues.LinesCleared,
      durationMs = TestValues.DurationMs
    )
    val events = Vector(
      ReplayEvent.PlayerInput(Input.MoveLeft, 0L, ReplayEvent.StartOfRecordingMs),
      ReplayEvent.PieceSpawn(TetrominoShape.O, 10L, ReplayEvent.StartOfRecordingMs)
    )
    ReplayData(metadata, events)

  private object ModifiedAt:
    val Oldest: Long = 1000L
    val Middle: Long = 2000L
    val Newest: Long = 3000L

  private val ReplayFileSuffix = ".replay.json"

  private def withTempDir[A](test: Path => ZIO[Any, Any, A]): ZIO[Any, Any, A] =
    ZIO.acquireReleaseWith(
      ZIO.attemptBlocking(Files.createTempDirectory("monadris-test-replays"))
    )(dir =>
      ZIO.attemptBlocking {
        Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))
      }.ignore
    )(test)

  private def withTempRepo[A](test: ReplayRepository => ZIO[Any, Any, A]): ZIO[Any, Any, A] =
    withTempDir(dir => test(FileReplayRepository.make(dir)))

  private def setModifiedAt(dir: Path, name: String, millis: Long): Task[Unit] =
    ZIO.attemptBlocking {
      Files.setLastModifiedTime(dir.resolve(s"$name$ReplayFileSuffix"), FileTime.fromMillis(millis))
    }.unit

  def spec = suite("FileReplayRepository")(
    suite("save")(
      test("saves replay to file") {
        withTempRepo { repo =>
          val replay = createReplayData()
          for
            _      <- repo.save("test-replay", replay)
            exists <- repo.exists("test-replay")
          yield assertTrue(exists)
        }
      },
      test("refuses to overwrite an existing replay") {
        withTempRepo { repo =>
          val original    = createReplayData()
          val replacement =
            original.copy(metadata = original.metadata.copy(finalScore = original.metadata.finalScore + 1))
          for
            _      <- repo.save("test-replay", original)
            second <- repo.save("test-replay", replacement).either
            loaded <- repo.load("test-replay")
          yield assertTrue(
            second == Left(ReplayError.AlreadyExists("test-replay")),
            loaded.metadata.finalScore == original.metadata.finalScore
          )
        }
      }
    ),
    suite("load")(
      test("loads saved replay") {
        withTempRepo { repo =>
          val replay = createReplayData()
          for
            _      <- repo.save("test-replay", replay)
            loaded <- repo.load("test-replay")
          yield assertTrue(
            loaded.metadata == replay.metadata,
            loaded.events == replay.events
          )
        }
      },
      test("fails for non-existent replay") {
        withTempRepo { repo =>
          for result <- repo.load("non-existent").either
          yield assertTrue(result.isLeft)
        }
      }
    ),
    suite("list")(
      test("returns empty list when no replays") {
        withTempRepo { repo =>
          for replays <- repo.list
          yield assertTrue(replays.isEmpty)
        }
      },
      test("returns list of saved replays") {
        withTempRepo { repo =>
          for
            _       <- repo.save("replay1", createReplayData())
            _       <- repo.save("replay2", createReplayData())
            _       <- repo.save("replay3", createReplayData())
            replays <- repo.list
          yield assertTrue(
            replays.size == 3,
            replays.contains("replay1"),
            replays.contains("replay2"),
            replays.contains("replay3")
          )
        }
      },
      test("returns newest replay first") {
        withTempDir { dir =>
          val repo = FileReplayRepository.make(dir)
          for
            _       <- repo.save("charlie", createReplayData())
            _       <- repo.save("alpha", createReplayData())
            _       <- repo.save("bravo", createReplayData())
            _       <- setModifiedAt(dir, "charlie", ModifiedAt.Middle)
            _       <- setModifiedAt(dir, "alpha", ModifiedAt.Oldest)
            _       <- setModifiedAt(dir, "bravo", ModifiedAt.Newest)
            replays <- repo.list
          yield assertTrue(
            replays == Vector("bravo", "charlie", "alpha")
          )
        }
      },
      test("orders replays with the same modification time by name") {
        withTempDir { dir =>
          val repo = FileReplayRepository.make(dir)
          for
            _       <- repo.save("bravo", createReplayData())
            _       <- repo.save("alpha", createReplayData())
            _       <- setModifiedAt(dir, "bravo", ModifiedAt.Oldest)
            _       <- setModifiedAt(dir, "alpha", ModifiedAt.Oldest)
            replays <- repo.list
          yield assertTrue(replays == Vector("alpha", "bravo"))
        }
      }
    ),
    suite("name validation")(
      test("save rejects a name that would escape the replay directory") {
        withTempDir { dir =>
          val repo = FileReplayRepository.make(dir.resolve("replays"))
          for
            result  <- repo.save("../escaped", createReplayData()).either
            escaped <- ZIO.attemptBlocking(Files.exists(dir.resolve(s"escaped$ReplayFileSuffix")))
          yield assertTrue(result.isLeft, !escaped)
        }
      },
      test("load, exists and delete reject invalid names") {
        withTempRepo { repo =>
          for
            loaded  <- repo.load("a/b").either
            exists  <- repo.exists("").either
            deleted <- repo.delete("..").either
          yield assertTrue(loaded.isLeft, exists.isLeft, deleted.isLeft)
        }
      }
    ),
    suite("exists")(
      test("returns false for non-existent replay") {
        withTempRepo { repo =>
          for exists <- repo.exists("non-existent")
          yield assertTrue(!exists)
        }
      },
      test("returns true for existing replay") {
        withTempRepo { repo =>
          for
            _      <- repo.save("test-replay", createReplayData())
            exists <- repo.exists("test-replay")
          yield assertTrue(exists)
        }
      }
    ),
    suite("delete")(
      test("deletes existing replay") {
        withTempRepo { repo =>
          for
            _            <- repo.save("test-replay", createReplayData())
            existsBefore <- repo.exists("test-replay")
            _            <- repo.delete("test-replay")
            existsAfter  <- repo.exists("test-replay")
          yield assertTrue(
            existsBefore,
            !existsAfter
          )
        }
      },
      test("does not fail for non-existent replay") {
        withTempRepo { repo =>
          for _ <- repo.delete("non-existent")
          yield assertTrue(true)
        }
      }
    ),
    suite("list edge cases")(
      test("returns empty when directory does not exist") {
        ZIO.acquireReleaseWith(
          ZIO.attemptBlocking(Files.createTempDirectory("monadris-test-nodir"))
        )(dir =>
          ZIO.attemptBlocking {
            Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))
          }.ignore
        ) { parentDir =>
          val nonExistentDir = parentDir.resolve("does-not-exist")
          val repo           = FileReplayRepository.make(nonExistentDir)
          for replays <- repo.list
          yield assertTrue(replays.isEmpty)
        }
      }
    ),
    suite("integration")(
      test("full workflow: save, list, load, delete") {
        withTempRepo { repo =>
          val replay = createReplayData()
          for
            _        <- repo.save("my-game", replay)
            replays1 <- repo.list
            loaded   <- repo.load("my-game")
            _        <- repo.delete("my-game")
            replays2 <- repo.list
          yield assertTrue(
            replays1.contains("my-game"),
            loaded.metadata == replay.metadata,
            !replays2.contains("my-game")
          )
        }
      },
      test("save creates directory if it does not exist") {
        ZIO.acquireReleaseWith(
          ZIO.attemptBlocking(Files.createTempDirectory("monadris-test-parent"))
        )(dir =>
          ZIO.attemptBlocking {
            Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))
          }.ignore
        ) { parentDir =>
          val nestedDir = parentDir.resolve("nested").resolve("replays")
          val repo      = FileReplayRepository.make(nestedDir)
          val replay    = createReplayData()
          for
            _      <- repo.save("nested-test", replay)
            exists <- repo.exists("nested-test")
            loaded <- repo.load("nested-test")
          yield assertTrue(
            exists,
            loaded.metadata == replay.metadata
          )
        }
      },
      test("multiple saves and deletes maintain consistency") {
        withTempRepo { repo =>
          val replay = createReplayData()
          for
            _       <- repo.save("a", replay)
            _       <- repo.save("b", replay)
            _       <- repo.save("c", replay)
            _       <- repo.delete("b")
            replays <- repo.list
          yield assertTrue(replays.toSet == Set("a", "c"))
        }
      }
    )
  )

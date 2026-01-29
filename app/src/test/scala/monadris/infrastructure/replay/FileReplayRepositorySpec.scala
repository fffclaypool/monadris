package monadris.infrastructure.replay

import java.nio.file.Files
import java.nio.file.Path

import zio.*
import zio.test.*
import zio.test.Assertion.*

import monadris.domain.Input
import monadris.domain.TetrominoShape
import monadris.domain.replay.*

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
      ReplayEvent.PlayerInput(Input.MoveLeft, 0L),
      ReplayEvent.PieceSpawn(TetrominoShape.O, 10L)
    )
    ReplayData(metadata, events)

  private def withTempRepo[A](test: ReplayRepository => Task[A]): Task[A] =
    ZIO.acquireReleaseWith(
      ZIO.attemptBlocking(Files.createTempDirectory("monadris-test-replays"))
    )(dir =>
      ZIO.attemptBlocking {
        Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))
      }.ignore
    )(dir => test(FileReplayRepository.make(dir)))

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
      test("overwrites existing replay") {
        withTempRepo { repo =>
          val replay1   = createReplayData()
          val metadata2 = replay1.metadata.copy(finalScore = 999)
          val replay2   = replay1.copy(metadata = metadata2)
          for
            _      <- repo.save("test-replay", replay1)
            _      <- repo.save("test-replay", replay2)
            loaded <- repo.load("test-replay")
          yield assertTrue(loaded.metadata.finalScore == 999)
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
      test("returns sorted list") {
        withTempRepo { repo =>
          for
            _       <- repo.save("charlie", createReplayData())
            _       <- repo.save("alpha", createReplayData())
            _       <- repo.save("bravo", createReplayData())
            replays <- repo.list
          yield assertTrue(
            replays == Vector("alpha", "bravo", "charlie")
          )
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
      }
    )
  )

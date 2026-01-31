package monadris.infrastructure.persistence

import javax.sql.DataSource

import zio.*
import zio.test.*

import monadris.domain.Input
import monadris.domain.TetrominoShape
import monadris.replay.*

import io.getquill.*
import io.getquill.jdbczio.Quill
import org.flywaydb.core.Flyway
import org.testcontainers.containers.PostgreSQLContainer

object PostgresReplayRepositoryIntegrationSpec extends ZIOSpecDefault:

  private object TestValues:
    val Timestamp: Long   = 1706500000000L
    val GridWidth: Int    = 10
    val GridHeight: Int   = 20
    val FinalScore: Int   = 500
    val FinalLevel: Int   = 2
    val LinesCleared: Int = 5
    val DurationMs: Long  = 60000L

  private def createReplayData(
    name: String = "test",
    score: Int = TestValues.FinalScore
  ): ReplayData =
    val metadata = ReplayMetadata(
      version = ReplayMetadata.CurrentVersion,
      timestamp = TestValues.Timestamp,
      gridWidth = TestValues.GridWidth,
      gridHeight = TestValues.GridHeight,
      initialPiece = TetrominoShape.T,
      nextPiece = TetrominoShape.I,
      finalScore = score,
      finalLevel = TestValues.FinalLevel,
      finalLinesCleared = TestValues.LinesCleared,
      durationMs = TestValues.DurationMs
    )
    val events = Vector(
      ReplayEvent.PieceSpawn(TetrominoShape.O, 0L),
      ReplayEvent.PlayerInput(Input.MoveLeft, 0L),
      ReplayEvent.PlayerInput(Input.HardDrop, 10L)
    )
    ReplayData(metadata, events)

  private class PostgresContainer:
    private val container: PostgreSQLContainer[?] =
      val c = new PostgreSQLContainer("postgres:16-alpine")
      c.withDatabaseName("monadris_test")
      c.withUsername("test")
      c.withPassword("test")
      c

    def start(): Unit = container.start()
    def stop(): Unit  = container.stop()

    def jdbcUrl: String  = container.getJdbcUrl
    def username: String = container.getUsername
    def password: String = container.getPassword

  private def createDataSource(container: PostgresContainer): DataSource =
    val ds = new com.zaxxer.hikari.HikariDataSource()
    ds.setJdbcUrl(container.jdbcUrl)
    ds.setUsername(container.username)
    ds.setPassword(container.password)
    ds.setMaximumPoolSize(2)
    ds

  private def runMigrations(ds: DataSource): Unit =
    Flyway
      .configure()
      .dataSource(ds)
      .locations("classpath:db/migration")
      .load()
      .migrate()

  private def withPostgresRepo[A](
    test: ReplayRepository => Task[A]
  ): Task[A] =
    ZIO.scoped {
      for
        container <- ZIO.acquireRelease(
          ZIO.attempt {
            val c = new PostgresContainer()
            c.start()
            c
          }
        )(c => ZIO.attempt(c.stop()).ignore)
        ds <- ZIO.acquireRelease(
          ZIO.attempt(createDataSource(container))
        )(ds => ZIO.attempt(ds.asInstanceOf[com.zaxxer.hikari.HikariDataSource].close()).ignore)
        _ <- ZIO.attempt(runMigrations(ds))
        quill = new Quill.Postgres(SnakeCase, ds)
        repo  = new PostgresReplayRepository(quill)
        result <- test(repo)
      yield result
    }

  def spec = suite("PostgresReplayRepository Integration")(
    suite("save")(
      test("saves replay with JSONB events") {
        withPostgresRepo { repo =>
          val replay = createReplayData()
          for
            _      <- repo.save("test-replay", replay)
            exists <- repo.exists("test-replay")
          yield assertTrue(exists)
        }
      },
      test("overwrites existing replay") {
        withPostgresRepo { repo =>
          val replay1 = createReplayData(score = 100)
          val replay2 = createReplayData(score = 999)
          for
            _      <- repo.save("test-replay", replay1)
            _      <- repo.delete("test-replay")
            _      <- repo.save("test-replay", replay2)
            loaded <- repo.load("test-replay")
          yield assertTrue(loaded.metadata.finalScore == 999)
        }
      }
    ),
    suite("load")(
      test("loads saved replay with correct metadata") {
        withPostgresRepo { repo =>
          val replay = createReplayData()
          for
            _      <- repo.save("test-replay", replay)
            loaded <- repo.load("test-replay")
          yield assertTrue(
            loaded.metadata.version == replay.metadata.version,
            loaded.metadata.gridWidth == replay.metadata.gridWidth,
            loaded.metadata.gridHeight == replay.metadata.gridHeight,
            loaded.metadata.initialPiece == replay.metadata.initialPiece,
            loaded.metadata.nextPiece == replay.metadata.nextPiece,
            loaded.metadata.finalScore == replay.metadata.finalScore,
            loaded.metadata.finalLevel == replay.metadata.finalLevel,
            loaded.metadata.finalLinesCleared == replay.metadata.finalLinesCleared,
            loaded.metadata.durationMs == replay.metadata.durationMs
          )
        }
      },
      test("loads saved replay with correct events") {
        withPostgresRepo { repo =>
          val replay = createReplayData()
          for
            _      <- repo.save("test-replay", replay)
            loaded <- repo.load("test-replay")
          yield assertTrue(loaded.events == replay.events)
        }
      },
      test("fails for non-existent replay") {
        withPostgresRepo { repo =>
          for result <- repo.load("non-existent").either
          yield assertTrue(result.isLeft)
        }
      }
    ),
    suite("list")(
      test("returns empty list when no replays") {
        withPostgresRepo { repo =>
          for replays <- repo.list
          yield assertTrue(replays.isEmpty)
        }
      },
      test("returns list of saved replays") {
        withPostgresRepo { repo =>
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
      }
    ),
    suite("exists")(
      test("returns false for non-existent replay") {
        withPostgresRepo { repo =>
          for exists <- repo.exists("non-existent")
          yield assertTrue(!exists)
        }
      },
      test("returns true for existing replay") {
        withPostgresRepo { repo =>
          for
            _      <- repo.save("test-replay", createReplayData())
            exists <- repo.exists("test-replay")
          yield assertTrue(exists)
        }
      }
    ),
    suite("delete")(
      test("deletes existing replay") {
        withPostgresRepo { repo =>
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
      }
    ),
    suite("JSONB encoding/decoding")(
      test("correctly encodes and decodes PieceSpawn events") {
        withPostgresRepo { repo =>
          val metadata = ReplayMetadata(
            version = "1.0",
            timestamp = TestValues.Timestamp,
            gridWidth = TestValues.GridWidth,
            gridHeight = TestValues.GridHeight,
            initialPiece = TetrominoShape.I,
            nextPiece = TetrominoShape.O,
            finalScore = 0,
            finalLevel = 1,
            finalLinesCleared = 0,
            durationMs = 1000L
          )
          val events = Vector(
            ReplayEvent.PieceSpawn(TetrominoShape.T, 0L),
            ReplayEvent.PieceSpawn(TetrominoShape.S, 5L),
            ReplayEvent.PieceSpawn(TetrominoShape.Z, 10L)
          )
          val replay = ReplayData(metadata, events)

          for
            _      <- repo.save("piece-spawn-test", replay)
            loaded <- repo.load("piece-spawn-test")
          yield assertTrue(
            loaded.events.size == 3,
            loaded.events(0) == ReplayEvent.PieceSpawn(TetrominoShape.T, 0L),
            loaded.events(1) == ReplayEvent.PieceSpawn(TetrominoShape.S, 5L),
            loaded.events(2) == ReplayEvent.PieceSpawn(TetrominoShape.Z, 10L)
          )
        }
      },
      test("correctly encodes and decodes PlayerInput events") {
        withPostgresRepo { repo =>
          val metadata = ReplayMetadata(
            version = "1.0",
            timestamp = TestValues.Timestamp,
            gridWidth = TestValues.GridWidth,
            gridHeight = TestValues.GridHeight,
            initialPiece = TetrominoShape.I,
            nextPiece = TetrominoShape.O,
            finalScore = 0,
            finalLevel = 1,
            finalLinesCleared = 0,
            durationMs = 1000L
          )
          val events = Vector(
            ReplayEvent.PlayerInput(Input.MoveLeft, 0L),
            ReplayEvent.PlayerInput(Input.MoveRight, 1L),
            ReplayEvent.PlayerInput(Input.RotateClockwise, 2L),
            ReplayEvent.PlayerInput(Input.HardDrop, 3L)
          )
          val replay = ReplayData(metadata, events)

          for
            _      <- repo.save("player-input-test", replay)
            loaded <- repo.load("player-input-test")
          yield assertTrue(
            loaded.events.size == 4,
            loaded.events(0) == ReplayEvent.PlayerInput(Input.MoveLeft, 0L),
            loaded.events(1) == ReplayEvent.PlayerInput(Input.MoveRight, 1L),
            loaded.events(2) == ReplayEvent.PlayerInput(Input.RotateClockwise, 2L),
            loaded.events(3) == ReplayEvent.PlayerInput(Input.HardDrop, 3L)
          )
        }
      },
      test("correctly handles mixed event types") {
        withPostgresRepo { repo =>
          val metadata = ReplayMetadata(
            version = "1.0",
            timestamp = TestValues.Timestamp,
            gridWidth = TestValues.GridWidth,
            gridHeight = TestValues.GridHeight,
            initialPiece = TetrominoShape.I,
            nextPiece = TetrominoShape.O,
            finalScore = 100,
            finalLevel = 2,
            finalLinesCleared = 4,
            durationMs = 5000L
          )
          val events = Vector(
            ReplayEvent.PieceSpawn(TetrominoShape.L, 0L),
            ReplayEvent.PlayerInput(Input.MoveLeft, 0L),
            ReplayEvent.PlayerInput(Input.HardDrop, 0L),
            ReplayEvent.PieceSpawn(TetrominoShape.J, 1L),
            ReplayEvent.PlayerInput(Input.Tick, 1L)
          )
          val replay = ReplayData(metadata, events)

          for
            _      <- repo.save("mixed-events-test", replay)
            loaded <- repo.load("mixed-events-test")
          yield assertTrue(loaded.events == events)
        }
      }
    )
  ) @@ TestAspect.sequential @@ TestAspect.withLiveClock @@ TestAspect.ifEnvSet("RUN_INTEGRATION_TESTS")

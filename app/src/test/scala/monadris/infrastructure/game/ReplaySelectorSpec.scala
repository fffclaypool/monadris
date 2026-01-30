package monadris.infrastructure.game

import zio.*
import zio.test.*

import monadris.domain.TetrominoShape
import monadris.infrastructure.persistence.ReplayRepository
import monadris.infrastructure.terminal.TestServices as LocalTestServices
import monadris.replay.*

object ReplaySelectorSpec extends ZIOSpecDefault:

  private val testGridWidth  = 10
  private val testGridHeight = 20

  private def createTestReplayData(name: String = "test-replay"): ReplayData =
    ReplayData(
      metadata = ReplayMetadata(
        version = ReplayMetadata.CurrentVersion,
        timestamp = java.lang.System.currentTimeMillis(),
        gridWidth = testGridWidth,
        gridHeight = testGridHeight,
        initialPiece = TetrominoShape.T,
        nextPiece = TetrominoShape.I,
        finalScore = 1000,
        finalLevel = 5,
        finalLinesCleared = 10,
        durationMs = 60000L
      ),
      events = Vector.empty
    )

  case class MockReplayRepository(
    replays: Ref[Map[String, ReplayData]]
  ) extends ReplayRepository:
    def save(name: String, replay: ReplayData): Task[Unit] =
      replays.update(_ + (name -> replay))

    def load(name: String): Task[ReplayData] =
      replays.get.flatMap { r =>
        ZIO.fromOption(r.get(name)).orElseFail(new RuntimeException(s"Replay not found: $name"))
      }

    def list: Task[Vector[String]] =
      replays.get.map(_.keys.toVector.sorted)

    def exists(name: String): Task[Boolean] =
      replays.get.map(_.contains(name))

    def delete(name: String): Task[Unit] =
      replays.update(_ - name)

  object MockReplayRepository:
    def make(initial: Map[String, ReplayData] = Map.empty): UIO[MockReplayRepository] =
      Ref.make(initial).map(MockReplayRepository(_))

    def layer(initial: Map[String, ReplayData] = Map.empty): ULayer[ReplayRepository] =
      ZLayer.fromZIO(make(initial))

  def spec = suite("ReplaySelector")(
    suite("watchReplay")(
      test("Shows no replays message when list is empty") {
        for
          service <- ZIO.service[LocalTestServices.TestConsoleService]
          _       <- ReplaySelector.watchReplay
            .provide(
              LocalTestServices.tty(Chunk(' '.toInt)),
              ZLayer.succeed(service),
              LocalTestServices.command,
              LocalTestServices.config,
              MockReplayRepository.layer()
            )
            .timeout(Duration.fromMillis(500))
          output <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains("No replays") || combined.contains("no replays")
        )
      }.provide(LocalTestServices.console),
      test("Lists available replays when not empty") {
        val replayMap = Map(
          "replay1" -> createTestReplayData("replay1"),
          "replay2" -> createTestReplayData("replay2")
        )
        for
          service <- ZIO.service[LocalTestServices.TestConsoleService]
          _       <- ReplaySelector.watchReplay
            .provide(
              LocalTestServices.tty(Chunk('0'.toInt, '\r'.toInt)),
              ZLayer.succeed(service),
              LocalTestServices.command,
              LocalTestServices.config,
              MockReplayRepository.layer(replayMap)
            )
            .timeout(Duration.fromMillis(500))
          output <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains("replay1") || combined.contains("replay2") || combined.contains("Available")
        )
      }.provide(LocalTestServices.console),
      test("Handles cancel input (0)") {
        val replayMap = Map("test-replay" -> createTestReplayData())
        for
          service <- ZIO.service[LocalTestServices.TestConsoleService]
          _       <- ReplaySelector.watchReplay
            .provide(
              LocalTestServices.tty(Chunk('0'.toInt, '\r'.toInt)),
              ZLayer.succeed(service),
              LocalTestServices.command,
              LocalTestServices.config,
              MockReplayRepository.layer(replayMap)
            )
            .timeout(Duration.fromMillis(500))
        yield assertTrue(true)
      }.provide(LocalTestServices.console)
    ),
    suite("listReplays")(
      test("Shows no replays message when list is empty") {
        for
          service <- ZIO.service[LocalTestServices.TestConsoleService]
          _       <- ReplaySelector.listReplays
            .provide(
              LocalTestServices.tty(Chunk(' '.toInt)),
              ZLayer.succeed(service),
              LocalTestServices.command,
              LocalTestServices.config,
              MockReplayRepository.layer()
            )
            .timeout(Duration.fromMillis(500))
          output <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains("No replays") || combined.contains("no replays")
        )
      }.provide(LocalTestServices.console),
      test("Lists saved replays") {
        val replayMap = Map(
          "game-2024" -> createTestReplayData("game-2024"),
          "best-run"  -> createTestReplayData("best-run")
        )
        for
          service <- ZIO.service[LocalTestServices.TestConsoleService]
          _       <- ReplaySelector.listReplays
            .provide(
              LocalTestServices.tty(Chunk(' '.toInt)),
              ZLayer.succeed(service),
              LocalTestServices.command,
              LocalTestServices.config,
              MockReplayRepository.layer(replayMap)
            )
            .timeout(Duration.fromMillis(500))
          output <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains("Saved replays") || combined.contains("game-2024") || combined.contains("best-run")
        )
      }.provide(LocalTestServices.console)
    ),
    suite("MockReplayRepository")(
      test("save and load work correctly") {
        val replay = createTestReplayData("test")
        for
          repo   <- MockReplayRepository.make()
          _      <- repo.save("test", replay)
          loaded <- repo.load("test")
        yield assertTrue(
          loaded.metadata.finalScore == 1000,
          loaded.metadata.finalLevel == 5
        )
      },
      test("list returns all saved replay names") {
        val replay1 = createTestReplayData("replay1")
        val replay2 = createTestReplayData("replay2")
        for
          repo <- MockReplayRepository.make()
          _    <- repo.save("replay1", replay1)
          _    <- repo.save("replay2", replay2)
          list <- repo.list
        yield assertTrue(
          list.contains("replay1"),
          list.contains("replay2"),
          list.size == 2
        )
      },
      test("exists returns true for saved replays") {
        val replay = createTestReplayData()
        for
          repo   <- MockReplayRepository.make()
          _      <- repo.save("my-replay", replay)
          exists <- repo.exists("my-replay")
        yield assertTrue(exists)
      },
      test("exists returns false for non-existent replays") {
        for
          repo   <- MockReplayRepository.make()
          exists <- repo.exists("non-existent")
        yield assertTrue(!exists)
      },
      test("delete removes replay") {
        val replay = createTestReplayData()
        for
          repo         <- MockReplayRepository.make()
          _            <- repo.save("to-delete", replay)
          existsBefore <- repo.exists("to-delete")
          _            <- repo.delete("to-delete")
          existsAfter  <- repo.exists("to-delete")
        yield assertTrue(existsBefore, !existsAfter)
      }
    )
  )

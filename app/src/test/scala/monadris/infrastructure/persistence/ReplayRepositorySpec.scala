package monadris.infrastructure.persistence

import zio.*
import zio.test.*

import monadris.domain.TetrominoShape
import monadris.replay.*

object ReplayRepositorySpec extends ZIOSpecDefault:

  private val testReplayData: ReplayData = ReplayData(
    metadata = ReplayMetadata(
      version = ReplayMetadata.CurrentVersion,
      timestamp = 1706500000000L,
      gridWidth = 10,
      gridHeight = 20,
      initialPiece = TetrominoShape.T,
      nextPiece = TetrominoShape.I,
      finalScore = 1000,
      finalLevel = 5,
      finalLinesCleared = 10,
      durationMs = 60000L
    ),
    events = Vector.empty
  )

  private val mockRepo: ULayer[ReplayRepository] = ZLayer.fromZIO {
    Ref.make(Map.empty[String, ReplayData]).map { store =>
      new ReplayRepository:
        def save(name: String, replay: ReplayData): Task[Unit] =
          store.update(_ + (name -> replay))
        def load(name: String): Task[ReplayData] =
          store.get.flatMap(m => ZIO.fromOption(m.get(name)).orElseFail(new RuntimeException(s"Not found: $name")))
        def list: Task[Vector[String]] =
          store.get.map(_.keys.toVector.sorted)
        def exists(name: String): Task[Boolean] =
          store.get.map(_.contains(name))
        def delete(name: String): Task[Unit] =
          store.update(_ - name)
    }
  }

  def spec = suite("ReplayRepository companion object")(
    test("save delegates to service") {
      for
        _      <- ReplayRepository.save("test", testReplayData)
        exists <- ReplayRepository.exists("test")
      yield assertTrue(exists)
    },
    test("load delegates to service") {
      for
        _      <- ReplayRepository.save("my-replay", testReplayData)
        loaded <- ReplayRepository.load("my-replay")
      yield assertTrue(loaded.metadata.finalScore == 1000)
    },
    test("load fails for non-existent replay") {
      for result <- ReplayRepository.load("missing").either
      yield assertTrue(result.isLeft)
    },
    test("list delegates to service") {
      for
        _       <- ReplayRepository.save("alpha", testReplayData)
        _       <- ReplayRepository.save("bravo", testReplayData)
        replays <- ReplayRepository.list
      yield assertTrue(
        replays.size == 2,
        replays.contains("alpha"),
        replays.contains("bravo")
      )
    },
    test("exists delegates to service") {
      for
        _           <- ReplayRepository.save("exists-test", testReplayData)
        existsTrue  <- ReplayRepository.exists("exists-test")
        existsFalse <- ReplayRepository.exists("no-such-replay")
      yield assertTrue(existsTrue, !existsFalse)
    },
    test("delete delegates to service") {
      for
        _            <- ReplayRepository.save("to-delete", testReplayData)
        existsBefore <- ReplayRepository.exists("to-delete")
        _            <- ReplayRepository.delete("to-delete")
        existsAfter  <- ReplayRepository.exists("to-delete")
      yield assertTrue(existsBefore, !existsAfter)
    }
  ).provideLayer(mockRepo)

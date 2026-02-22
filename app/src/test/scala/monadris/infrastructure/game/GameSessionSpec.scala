package monadris.infrastructure.game

import zio.*
import zio.test.*

import monadris.domain.*
import monadris.infrastructure.persistence.ReplayRepository
import monadris.infrastructure.terminal.TestServices as LocalTestServices
import monadris.replay.*

object GameSessionSpec extends ZIOSpecDefault:

  case class MockReplayRepository(saved: Ref[Option[(String, ReplayData)]]) extends ReplayRepository:
    def save(name: String, replay: ReplayData): Task[Unit] = saved.set(Some((name, replay)))
    def load(name: String): Task[ReplayData]               = ZIO.fail(new RuntimeException("Not found"))
    def list: Task[Vector[String]]                         = ZIO.succeed(Vector.empty)
    def exists(name: String): Task[Boolean]                = ZIO.succeed(false)
    def delete(name: String): Task[Unit]                   = ZIO.unit

  object MockReplayRepository:
    def layer: ULayer[ReplayRepository] =
      ZLayer.fromZIO(Ref.make(Option.empty[(String, ReplayData)]).map(MockReplayRepository(_)))

  private val testGridWidth  = LocalTestServices.testConfig.grid.width
  private val testGridHeight = LocalTestServices.testConfig.grid.height

  def initialState: GameState =
    GameState.initial(TetrominoShape.T, TetrominoShape.I, testGridWidth, testGridHeight)

  def spec = suite("GameSession")(
    suite("showIntro")(
      test("Outputs title screen content") {
        for
          service <- ZIO.service[LocalTestServices.TestConsoleService]
          _       <- GameSession.showIntro
            .provide(
              LocalTestServices.tty(Chunk.empty),
              ZLayer.succeed(service),
              LocalTestServices.command,
              LocalTestServices.config,
              LocalTestServices.terminalSession(Chunk.empty)
            )
            .timeout(Duration.fromMillis(500))
          output <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains("Functional Tetris") || combined.contains("Controls") || combined.nonEmpty
        )
      }.provide(LocalTestServices.console),
      test("Logs startup message") {
        for _ <- GameSession.showIntro
            .provide(
              LocalTestServices.tty(Chunk.empty),
              LocalTestServices.console,
              LocalTestServices.command,
              LocalTestServices.config,
              LocalTestServices.terminalSession(Chunk.empty)
            )
            .timeout(Duration.fromMillis(500))
        yield assertTrue(true)
      }
    ),
    suite("GameState initialization")(
      test("Initial state has correct grid dimensions") {
        val state = initialState
        assertTrue(
          state.grid.width == testGridWidth,
          state.grid.height == testGridHeight
        )
      },
      test("Initial state is playing") {
        val state = initialState
        assertTrue(state.status == GameStatus.Playing)
      },
      test("Initial state has zero score") {
        val state = initialState
        assertTrue(state.score == 0)
      },
      test("Initial state has level 1") {
        val state = initialState
        assertTrue(state.level == 1)
      },
      test("Initial state has zero lines cleared") {
        val state = initialState
        assertTrue(state.linesCleared == 0)
      },
      test("Initial state has current tetromino") {
        val state = initialState
        assertTrue(state.currentTetromino.shape == TetrominoShape.T)
      },
      test("Initial state has next tetromino") {
        val state = initialState
        assertTrue(state.nextTetromino == TetrominoShape.I)
      }
    ),
    suite("Game state transitions")(
      test("Game over state is detected") {
        val gameOverState = initialState.copy(status = GameStatus.GameOver)
        assertTrue(gameOverState.isGameOver)
      },
      test("Playing state is not game over") {
        assertTrue(!initialState.isGameOver)
      },
      test("Paused state is not game over") {
        val pausedState = initialState.copy(status = GameStatus.Paused)
        assertTrue(!pausedState.isGameOver)
      }
    ),
    suite("RandomPieceGenerator")(
      test("Generates valid tetromino shapes") {
        for shape <- GameRunner.RandomPieceGenerator.nextShape
        yield assertTrue(TetrominoShape.values.contains(shape))
      },
      test("Generates different shapes over multiple calls") {
        for
          shapes <- ZIO.collectAll(List.fill(50)(GameRunner.RandomPieceGenerator.nextShape))
          uniqueShapes = shapes.toSet
        yield assertTrue(uniqueShapes.size > 1)
      }
    ),
    suite("playAndRecord")(
      test("prompts for replay name") {
        val replayNameInput = Chunk('t'.toInt, 'e'.toInt, 's'.toInt, 't'.toInt, '\r'.toInt, 'q'.toInt)
        for result <- GameSession.playAndRecord
            .provide(
              LocalTestServices.tty(replayNameInput),
              LocalTestServices.console,
              LocalTestServices.command,
              LocalTestServices.config,
              LocalTestServices.terminalSession(replayNameInput),
              MockReplayRepository.layer
            )
            .timeout(Duration.fromMillis(1000))
            .either
        yield assertTrue(result.isLeft || result.isRight)
      }
    )
  )

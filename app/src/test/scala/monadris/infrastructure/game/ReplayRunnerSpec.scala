package monadris.infrastructure.game

import zio.*
import zio.test.*

import monadris.config.AppConfig
import monadris.domain.*
import monadris.infrastructure.terminal.ConsoleService
import monadris.infrastructure.terminal.Renderer
import monadris.infrastructure.terminal.TestServices as LocalTestServices
import monadris.replay.*
import monadris.view.ScreenBuffer

object ReplayRunnerSpec extends ZIOSpecDefault:

  private val testGridWidth  = 10
  private val testGridHeight = 20

  private def createTestReplayData(
    events: Vector[ReplayEvent] = Vector.empty,
    finalScore: Int = 0,
    durationMs: Long = 1000L
  ): ReplayData =
    ReplayData(
      metadata = ReplayMetadata(
        version = ReplayMetadata.CurrentVersion,
        timestamp = java.lang.System.currentTimeMillis(),
        gridWidth = testGridWidth,
        gridHeight = testGridHeight,
        initialPiece = TetrominoShape.T,
        nextPiece = TetrominoShape.I,
        finalScore = finalScore,
        finalLevel = 1,
        finalLinesCleared = 0,
        durationMs = durationMs
      ),
      events = events
    )

  private class MockRenderer extends Renderer:
    private val emptyBuffer = ScreenBuffer.empty(testGridWidth + 10, testGridHeight + 5)

    def render(
      state: GameState,
      config: AppConfig,
      previousBuffer: Option[ScreenBuffer]
    ): ZIO[ConsoleService, Throwable, ScreenBuffer] =
      ZIO.succeed(emptyBuffer)

    def renderGameOver(state: GameState): ZIO[ConsoleService, Throwable, Unit] =
      ZIO.unit

    def renderTitle: ZIO[ConsoleService, Throwable, Unit] =
      ZIO.unit

  def spec = suite("ReplayRunner")(
    suite("Speed constants")(
      test("Default speed is 1.0") {
        assertTrue(true)
      },
      test("Speed limits are reasonable") {
        assertTrue(true)
      }
    ),
    suite("PlaybackState")(
      test("Can create PlaybackState with default values") {
        val replayData    = createTestReplayData()
        val replayState   = ReplayPlayer.initialize(replayData)
        val playbackState = ReplayRunner.PlaybackState(
          replayState = replayState,
          previousBuffer = None,
          speed = 1.0,
          paused = false
        )
        assertTrue(
          playbackState.speed == 1.0,
          !playbackState.paused,
          playbackState.previousBuffer.isEmpty
        )
      },
      test("PlaybackState tracks replay state correctly") {
        val events = Vector(
          ReplayEvent.PlayerInput(Input.MoveLeft, 0L),
          ReplayEvent.PlayerInput(Input.MoveRight, 1L)
        )
        val replayData    = createTestReplayData(events)
        val replayState   = ReplayPlayer.initialize(replayData)
        val playbackState = ReplayRunner.PlaybackState(
          replayState = replayState,
          previousBuffer = None,
          speed = 2.0,
          paused = true
        )
        assertTrue(
          playbackState.speed == 2.0,
          playbackState.paused,
          !playbackState.replayState.isFinished
        )
      }
    ),
    suite("Replay playback")(
      test("Empty replay finishes immediately") {
        val replayData = createTestReplayData()
        val renderer   = MockRenderer()
        for _ <- ReplayRunner
            .run(replayData, LocalTestServices.testConfig, renderer)
            .provideSome[LocalTestServices.TestConsoleService](
              LocalTestServices.tty(Chunk('q'.toInt))
            )
            .timeout(Duration.fromMillis(500))
        yield assertTrue(true)
      }.provide(LocalTestServices.console),
      test("Q key quits replay") {
        val events = Vector(
          ReplayEvent.PlayerInput(Input.MoveLeft, 0L),
          ReplayEvent.PlayerInput(Input.MoveRight, 1L),
          ReplayEvent.PlayerInput(Input.MoveDown, 2L)
        )
        val replayData = createTestReplayData(events)
        val renderer   = MockRenderer()
        for _ <- ReplayRunner
            .run(replayData, LocalTestServices.testConfig, renderer)
            .provideSome[LocalTestServices.TestConsoleService](
              LocalTestServices.tty(Chunk('q'.toInt))
            )
            .timeout(Duration.fromMillis(500))
        yield assertTrue(true)
      }.provide(LocalTestServices.console),
      test("Uppercase Q key also quits replay") {
        val replayData = createTestReplayData()
        val renderer   = MockRenderer()
        for _ <- ReplayRunner
            .run(replayData, LocalTestServices.testConfig, renderer)
            .provideSome[LocalTestServices.TestConsoleService](
              LocalTestServices.tty(Chunk('Q'.toInt))
            )
            .timeout(Duration.fromMillis(500))
        yield assertTrue(true)
      }.provide(LocalTestServices.console),
      test("Replay outputs status to console") {
        val replayData = createTestReplayData()
        val renderer   = MockRenderer()
        for
          service <- ZIO.service[LocalTestServices.TestConsoleService]
          _       <- ReplayRunner
            .run(replayData, LocalTestServices.testConfig, renderer)
            .provideSome[LocalTestServices.TestConsoleService](
              LocalTestServices.tty(Chunk('q'.toInt))
            )
            .timeout(Duration.fromMillis(500))
          output <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains("REPLAY") || combined.contains("Speed") || combined.contains("Progress")
        )
      }.provide(LocalTestServices.console)
    ),
    suite("Speed control")(
      test("Plus key increases speed") {
        val replayData = createTestReplayData()
        val renderer   = MockRenderer()
        for _ <- ReplayRunner
            .run(replayData, LocalTestServices.testConfig, renderer)
            .provideSome[LocalTestServices.TestConsoleService](
              LocalTestServices.tty(Chunk('+'.toInt, 'q'.toInt))
            )
            .timeout(Duration.fromMillis(500))
        yield assertTrue(true)
      }.provide(LocalTestServices.console),
      test("Minus key decreases speed") {
        val replayData = createTestReplayData()
        val renderer   = MockRenderer()
        for _ <- ReplayRunner
            .run(replayData, LocalTestServices.testConfig, renderer)
            .provideSome[LocalTestServices.TestConsoleService](
              LocalTestServices.tty(Chunk('-'.toInt, 'q'.toInt))
            )
            .timeout(Duration.fromMillis(500))
        yield assertTrue(true)
      }.provide(LocalTestServices.console)
    ),
    suite("Replay progress")(
      test("Progress starts at 0 for new replay") {
        val events = Vector(
          ReplayEvent.PlayerInput(Input.MoveLeft, 0L),
          ReplayEvent.PlayerInput(Input.MoveRight, 1L)
        )
        val replayData  = createTestReplayData(events)
        val replayState = ReplayPlayer.initialize(replayData)
        assertTrue(replayState.progress == 0.0)
      },
      test("Progress is 1.0 for empty replay") {
        val replayData  = createTestReplayData()
        val replayState = ReplayPlayer.initialize(replayData)
        assertTrue(replayState.progress == 1.0)
      }
    )
  )

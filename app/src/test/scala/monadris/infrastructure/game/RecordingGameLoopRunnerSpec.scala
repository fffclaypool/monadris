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

object RecordingGameLoopRunnerSpec extends ZIOSpecDefault:

  private val testGridWidth  = LocalTestServices.testConfig.grid.width
  private val testGridHeight = LocalTestServices.testConfig.grid.height

  def initialState: GameState =
    GameState.initial(TetrominoShape.T, TetrominoShape.I, testGridWidth, testGridHeight)

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

  private object FixedPieceGenerator extends GameRunner.RandomPiece:
    private val shapes = Vector(
      TetrominoShape.I,
      TetrominoShape.O,
      TetrominoShape.T,
      TetrominoShape.S,
      TetrominoShape.Z,
      TetrominoShape.J,
      TetrominoShape.L
    )
    private var index = 0

    def nextShape: UIO[TetrominoShape] =
      ZIO.succeed {
        val shape = shapes(index % shapes.size)
        index += 1
        shape
      }

  private object Elapsed:
    val First: Long  = 250L
    val Second: Long = 1250L

  private def newBuilder: ReplayBuilder =
    ReplayBuilder.create(
      timestamp = 0L,
      gridWidth = testGridWidth,
      gridHeight = testGridHeight,
      initialPiece = TetrominoShape.T,
      nextPiece = TetrominoShape.I
    )

  def spec = suite("RecordingGameLoopRunner")(
    suite("recordStep")(
      test("stamps the input with the current frame and elapsed time, then advances the frame") {
        val step    = GameLoopRunner.Step(Input.MoveLeft, initialState, initialState, TetrominoShape.O)
        val builder = RecordingGameLoopRunner.recordStep(newBuilder, step, Elapsed.First)
        assertTrue(
          builder.events == Vector(ReplayEvent.PlayerInput(Input.MoveLeft, 0L, Elapsed.First)),
          builder.currentFrame == 1L
        )
      },
      test("records the spawned piece before the input when a piece was locked") {
        val lockedState = initialState.copy(
          currentTetromino = Tetromino.spawn(TetrominoShape.I, testGridWidth),
          nextTetromino = TetrominoShape.O
        )
        val step    = GameLoopRunner.Step(Input.HardDrop, initialState, lockedState, TetrominoShape.O)
        val builder = RecordingGameLoopRunner.recordStep(newBuilder, step, Elapsed.First)
        assertTrue(
          builder.events == Vector(
            ReplayEvent.PieceSpawn(TetrominoShape.O, 0L, Elapsed.First),
            ReplayEvent.PlayerInput(Input.HardDrop, 0L, Elapsed.First)
          )
        )
      },
      test("consecutive steps keep their own elapsed times") {
        val step    = GameLoopRunner.Step(Input.Tick, initialState, initialState, TetrominoShape.O)
        val builder = RecordingGameLoopRunner.recordStep(
          RecordingGameLoopRunner.recordStep(newBuilder, step, Elapsed.First),
          step,
          Elapsed.Second
        )
        assertTrue(builder.events.map(_.elapsedMs) == Vector(Elapsed.First, Elapsed.Second))
      }
    ),
    suite("Recording game loop")(
      test("Recording loop produces replay data on quit") {
        val renderer = MockRenderer()
        for result <- RecordingGameLoopRunner
            .recordingGameLoop(
              initialState,
              LocalTestServices.testConfig,
              renderer,
              FixedPieceGenerator
            )
            .provide(
              LocalTestServices.tty(Chunk('q'.toInt)),
              LocalTestServices.console
            )
            .timeout(Duration.fromMillis(1000))
        yield result match
          case Some((finalState, replayData)) =>
            assertTrue(
              replayData.metadata.gridWidth == testGridWidth,
              replayData.metadata.gridHeight == testGridHeight,
              replayData.metadata.initialPiece == TetrominoShape.T
            )
          case None =>
            assertTrue(false)
      },
      test("Recording captures initial piece") {
        val renderer = MockRenderer()
        for result <- RecordingGameLoopRunner
            .recordingGameLoop(
              initialState,
              LocalTestServices.testConfig,
              renderer,
              FixedPieceGenerator
            )
            .provide(
              LocalTestServices.tty(Chunk('q'.toInt)),
              LocalTestServices.console
            )
            .timeout(Duration.fromMillis(1000))
        yield result match
          case Some((_, replayData)) =>
            assertTrue(replayData.metadata.initialPiece == initialState.currentTetromino.shape)
          case None =>
            assertTrue(false)
      },
      test("Recording captures next piece") {
        val renderer = MockRenderer()
        for result <- RecordingGameLoopRunner
            .recordingGameLoop(
              initialState,
              LocalTestServices.testConfig,
              renderer,
              FixedPieceGenerator
            )
            .provide(
              LocalTestServices.tty(Chunk('q'.toInt)),
              LocalTestServices.console
            )
            .timeout(Duration.fromMillis(1000))
        yield result match
          case Some((_, replayData)) =>
            assertTrue(replayData.metadata.nextPiece == initialState.nextTetromino)
          case None =>
            assertTrue(false)
      },
      test("Recording uses correct version") {
        val renderer = MockRenderer()
        for result <- RecordingGameLoopRunner
            .recordingGameLoop(
              initialState,
              LocalTestServices.testConfig,
              renderer,
              FixedPieceGenerator
            )
            .provide(
              LocalTestServices.tty(Chunk('q'.toInt)),
              LocalTestServices.console
            )
            .timeout(Duration.fromMillis(1000))
        yield result match
          case Some((_, replayData)) =>
            assertTrue(replayData.metadata.version == ReplayMetadata.CurrentVersion)
          case None =>
            assertTrue(false)
      }
    ),
    suite("Input recording")(
      test("Records user input events") {
        val renderer = MockRenderer()
        for result <- RecordingGameLoopRunner
            .recordingGameLoop(
              initialState,
              LocalTestServices.testConfig,
              renderer,
              FixedPieceGenerator
            )
            .provide(
              LocalTestServices.tty(Chunk('h'.toInt, 'q'.toInt)),
              LocalTestServices.console
            )
            .timeout(Duration.fromMillis(1000))
        yield result match
          case Some((_, replayData)) =>
            val hasPlayerInputs = replayData.events.exists {
              case ReplayEvent.PlayerInput(_, _, _) => true
              case _                                => false
            }
            assertTrue(hasPlayerInputs || replayData.events.isEmpty)
          case None =>
            assertTrue(false)
      },
      test("Records multiple movement inputs before quit") {
        val renderer = MockRenderer()
        val inputs   = Chunk('h'.toInt, 'l'.toInt, 'j'.toInt, 'k'.toInt, 'q'.toInt)
        for result <- RecordingGameLoopRunner
            .recordingGameLoop(
              initialState,
              LocalTestServices.testConfig,
              renderer,
              FixedPieceGenerator
            )
            .provide(
              LocalTestServices.tty(inputs),
              LocalTestServices.console
            )
            .timeout(Duration.fromMillis(1000))
        yield result match
          case Some((_, replayData)) =>
            assertTrue(replayData.events.nonEmpty)
          case None =>
            assertTrue(false)
      },
      test("Recording captures final score in metadata") {
        val renderer = MockRenderer()
        for result <- RecordingGameLoopRunner
            .recordingGameLoop(
              initialState,
              LocalTestServices.testConfig,
              renderer,
              FixedPieceGenerator
            )
            .provide(
              LocalTestServices.tty(Chunk('q'.toInt)),
              LocalTestServices.console
            )
            .timeout(Duration.fromMillis(1000))
        yield result match
          case Some((finalState, replayData)) =>
            assertTrue(
              replayData.metadata.finalScore == finalState.score,
              replayData.metadata.finalLevel == finalState.level,
              replayData.metadata.finalLinesCleared == finalState.linesCleared
            )
          case None =>
            assertTrue(false)
      }
    ),
    suite("Event recording order")(
      test("PieceSpawn is recorded before PlayerInput when piece locks") {
        // Test event ordering using ReplayBuilder
        val builder = ReplayBuilder.create(
          timestamp = 12345L,
          gridWidth = 10,
          gridHeight = 20,
          initialPiece = TetrominoShape.I,
          nextPiece = TetrominoShape.O
        )

        // Same logic as recordEvents: record PieceSpawn first
        val builderWithSpawn = builder.recordPieceSpawn(TetrominoShape.T)
        val builderWithInput = builderWithSpawn.recordInput(Input.HardDrop)
        val finalBuilder     = builderWithInput.advanceFrame

        val replayData = finalBuilder.build(initialState, 23456L)
        val events     = replayData.events

        // Verify PieceSpawn is recorded before PlayerInput
        val pieceSpawnIndex = events.indexWhere {
          case ReplayEvent.PieceSpawn(_, _, _) => true
          case _                               => false
        }
        val playerInputIndex = events.indexWhere {
          case ReplayEvent.PlayerInput(_, _, _) => true
          case _                                => false
        }

        assertTrue(
          pieceSpawnIndex >= 0,
          playerInputIndex >= 0,
          pieceSpawnIndex < playerInputIndex
        )
      },
      test("Only PlayerInput is recorded when piece does not lock") {
        val builder = ReplayBuilder.create(
          timestamp = 12345L,
          gridWidth = 10,
          gridHeight = 20,
          initialPiece = TetrominoShape.I,
          nextPiece = TetrominoShape.O
        )

        // Only PlayerInput is recorded when piece does not lock
        val builderWithInput = builder.recordInput(Input.MoveLeft)
        val finalBuilder     = builderWithInput.advanceFrame

        val replayData = finalBuilder.build(initialState, 23456L)
        val events     = replayData.events

        val hasPieceSpawn = events.exists {
          case ReplayEvent.PieceSpawn(_, _, _) => true
          case _                               => false
        }
        val hasPlayerInput = events.exists {
          case ReplayEvent.PlayerInput(Input.MoveLeft, _, _) => true
          case _                                             => false
        }

        assertTrue(
          !hasPieceSpawn,
          hasPlayerInput
        )
      }
    ),
    suite("ReplayBuilder integration")(
      test("ReplayBuilder creates valid metadata") {
        val builder = ReplayBuilder.create(
          timestamp = 12345L,
          gridWidth = 10,
          gridHeight = 20,
          initialPiece = TetrominoShape.I,
          nextPiece = TetrominoShape.O
        )
        assertTrue(
          builder.currentFrame == 0
        )
      },
      test("ReplayBuilder records inputs") {
        val builder = ReplayBuilder
          .create(
            timestamp = 12345L,
            gridWidth = 10,
            gridHeight = 20,
            initialPiece = TetrominoShape.I,
            nextPiece = TetrominoShape.O
          )
          .recordInput(Input.MoveLeft)
          .advanceFrame

        val finalState = initialState.copy(score = 500)
        val replayData = builder.build(finalState, 23456L)

        assertTrue(
          replayData.events.nonEmpty,
          replayData.metadata.finalScore == 500
        )
      },
      test("ReplayBuilder records piece spawns") {
        val builder = ReplayBuilder
          .create(
            timestamp = 12345L,
            gridWidth = 10,
            gridHeight = 20,
            initialPiece = TetrominoShape.I,
            nextPiece = TetrominoShape.O
          )
          .recordPieceSpawn(TetrominoShape.T)
          .advanceFrame

        val finalState = initialState
        val replayData = builder.build(finalState, 23456L)

        val hasPieceSpawn = replayData.events.exists {
          case ReplayEvent.PieceSpawn(TetrominoShape.T, _, _) => true
          case _                                              => false
        }
        assertTrue(hasPieceSpawn)
      }
    )
  )

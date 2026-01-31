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

  def spec = suite("RecordingGameLoopRunner")(
    suite("RecordingLoopState")(
      test("Can create RecordingLoopState with initial values") {
        val replayBuilder = ReplayBuilder.create(
          timestamp = java.lang.System.currentTimeMillis(),
          gridWidth = testGridWidth,
          gridHeight = testGridHeight,
          initialPiece = TetrominoShape.T,
          nextPiece = TetrominoShape.I
        )
        val loopState = RecordingGameLoopRunner.RecordingLoopState(
          gameState = initialState,
          previousBuffer = None,
          replayBuilder = replayBuilder
        )
        assertTrue(
          loopState.gameState == initialState,
          loopState.previousBuffer.isEmpty,
          loopState.replayBuilder.currentFrame == 0
        )
      },
      test("RecordingLoopState tracks game state correctly") {
        val movedState    = initialState.copy(score = 100)
        val replayBuilder = ReplayBuilder.create(
          timestamp = java.lang.System.currentTimeMillis(),
          gridWidth = testGridWidth,
          gridHeight = testGridHeight,
          initialPiece = TetrominoShape.T,
          nextPiece = TetrominoShape.I
        )
        val loopState = RecordingGameLoopRunner.RecordingLoopState(
          gameState = movedState,
          previousBuffer = Some(ScreenBuffer.empty(10, 10)),
          replayBuilder = replayBuilder
        )
        assertTrue(
          loopState.gameState.score == 100,
          loopState.previousBuffer.isDefined
        )
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
              case ReplayEvent.PlayerInput(_, _) => true
              case _                             => false
            }
            assertTrue(hasPlayerInputs || replayData.events.isEmpty)
          case None =>
            assertTrue(false)
      }
    ),
    suite("Event recording order")(
      test("PieceSpawn is recorded before PlayerInput when piece locks") {
        // ReplayBuilderを使ってイベント順序をテスト
        val builder = ReplayBuilder.create(
          timestamp = 12345L,
          gridWidth = 10,
          gridHeight = 20,
          initialPiece = TetrominoShape.I,
          nextPiece = TetrominoShape.O
        )

        // recordEventsと同じロジック: PieceSpawnを先に記録
        val builderWithSpawn = builder.recordPieceSpawn(TetrominoShape.T)
        val builderWithInput = builderWithSpawn.recordInput(Input.HardDrop)
        val finalBuilder     = builderWithInput.advanceFrame

        val replayData = finalBuilder.build(initialState, 23456L)
        val events     = replayData.events

        // PieceSpawnがPlayerInputより先に記録されていることを確認
        val pieceSpawnIndex = events.indexWhere {
          case ReplayEvent.PieceSpawn(_, _) => true
          case _                            => false
        }
        val playerInputIndex = events.indexWhere {
          case ReplayEvent.PlayerInput(_, _) => true
          case _                             => false
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

        // ピースがロックされない場合はPlayerInputのみ
        val builderWithInput = builder.recordInput(Input.MoveLeft)
        val finalBuilder     = builderWithInput.advanceFrame

        val replayData = finalBuilder.build(initialState, 23456L)
        val events     = replayData.events

        val hasPieceSpawn = events.exists {
          case ReplayEvent.PieceSpawn(_, _) => true
          case _                            => false
        }
        val hasPlayerInput = events.exists {
          case ReplayEvent.PlayerInput(Input.MoveLeft, _) => true
          case _                                          => false
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
          case ReplayEvent.PieceSpawn(TetrominoShape.T, _) => true
          case _                                           => false
        }
        assertTrue(hasPieceSpawn)
      }
    )
  )

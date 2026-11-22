package monadris.infrastructure.game

import zio.*
import zio.test.*

import monadris.domain.*
import monadris.infrastructure.terminal.Renderer
import monadris.infrastructure.terminal.TestServices as LocalTestServices
import monadris.replay.*

object RecordingGameLoopRunnerSpec extends ZIOSpecDefault:

  private val testGridWidth  = LocalTestServices.testConfig.grid.width
  private val testGridHeight = LocalTestServices.testConfig.grid.height

  private val initialState: GameState =
    GameState.initial(TetrominoShape.T, TetrominoShape.I, testGridWidth, testGridHeight)

  private val upcomingShapes = Vector(TetrominoShape.O, TetrominoShape.S, TetrominoShape.Z)
  private val suiteTimeout   = 10.seconds

  private object Keys:
    val Left: Int  = 'h'.toInt
    val Right: Int = 'l'.toInt
    val Down: Int  = 'j'.toInt
    val Drop: Int  = ' '.toInt
    val Quit: Int  = 'q'.toInt

  /** Plays the given keys with gravity switched off and returns the final state and the recording */
  private def record(keys: Int*): Task[(GameState, ReplayData)] =
    TestPieces
      .cycling(upcomingShapes)
      .flatMap(RecordingGameLoopRunner.recordingGameLoop(initialState, LocalTestServices.testConfig, Renderer.live, _))
      .provide(LocalTestServices.ttyWithoutGravity(Chunk.fromIterable(keys)), LocalTestServices.console)

  @annotation.tailrec
  private def replayToEnd(player: ReplayPlayerState): GameState =
    if player.isFinished then player.gameState
    else replayToEnd(ReplayPlayer.advanceFrame(player, LocalTestServices.testConfig.game))

  private def recordedInputs(replay: ReplayData): Vector[Input] =
    replay.events.collect { case ReplayEvent.PlayerInput(input, _, _) => input }

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
        val step =
          GameLoopRunner.Step(Input.MoveLeft, initialState, initialState, TetrominoShape.O, pieceLocked = false)
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
        val step = GameLoopRunner.Step(Input.HardDrop, initialState, lockedState, TetrominoShape.O, pieceLocked = true)
        val builder = RecordingGameLoopRunner.recordStep(newBuilder, step, Elapsed.First)
        assertTrue(
          builder.events == Vector(
            ReplayEvent.PieceSpawn(TetrominoShape.O, 0L, Elapsed.First),
            ReplayEvent.PlayerInput(Input.HardDrop, 0L, Elapsed.First)
          )
        )
      },
      test("consecutive steps keep their own elapsed times") {
        val step    = GameLoopRunner.Step(Input.Tick, initialState, initialState, TetrominoShape.O, pieceLocked = false)
        val builder = RecordingGameLoopRunner.recordStep(
          RecordingGameLoopRunner.recordStep(newBuilder, step, Elapsed.First),
          step,
          Elapsed.Second
        )
        assertTrue(builder.events.map(_.elapsedMs) == Vector(Elapsed.First, Elapsed.Second))
      }
    ),
    suite("Recording game loop")(
      test("the metadata describes the grid and the pieces the game started with") {
        record(Keys.Quit).map { (_, replay) =>
          assertTrue(
            replay.metadata.version == ReplayMetadata.CurrentVersion,
            replay.metadata.gridWidth == testGridWidth,
            replay.metadata.gridHeight == testGridHeight,
            replay.metadata.initialPiece == initialState.currentTetromino.shape,
            replay.metadata.nextPiece == initialState.nextTetromino
          )
        }
      },
      test("quitting immediately records no events") {
        record(Keys.Quit).map((_, replay) => assertTrue(replay.events.isEmpty))
      },
      test("every processed key is recorded in order, one frame each") {
        record(Keys.Left, Keys.Right, Keys.Down, Keys.Quit).map { (_, replay) =>
          assertTrue(
            recordedInputs(replay) == Vector(Input.MoveLeft, Input.MoveRight, Input.MoveDown),
            replay.events.map(_.frameNumber) == Vector.range(0L, replay.events.size.toLong)
          )
        }
      },
      test("a lock records the piece that entered the preview, before the input that caused it") {
        record(Keys.Drop, Keys.Quit).map { (finalState, replay) =>
          assertTrue(
            replay.events.map(_.frameNumber).distinct.size == 1,
            replay.events.headOption.contains(
              ReplayEvent.PieceSpawn(upcomingShapes.head, ReplayEvent.StartOfRecordingMs, replay.events.head.elapsedMs)
            ),
            recordedInputs(replay) == Vector(Input.HardDrop),
            finalState.nextTetromino == upcomingShapes.head
          )
        }
      },
      test("the metadata carries the final result of the game") {
        record(Keys.Drop, Keys.Quit).map { (finalState, replay) =>
          assertTrue(
            finalState.score > initialState.score,
            replay.metadata.finalScore == finalState.score,
            replay.metadata.finalLevel == finalState.level,
            replay.metadata.finalLinesCleared == finalState.linesCleared
          )
        }
      },
      test("replaying the recording reproduces the final state of the game") {
        record(Keys.Left, Keys.Drop, Keys.Right, Keys.Drop, Keys.Down, Keys.Quit).map { (finalState, replay) =>
          assertTrue(replayToEnd(ReplayPlayer.initialize(replay)) == finalState)
        }
      }
    )
  ) @@ TestAspect.timeout(suiteTimeout)

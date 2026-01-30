package monadris.infrastructure.game

import zio.*

import monadris.config.AppConfig
import monadris.domain.*
import monadris.game.GameLoop
import monadris.game.LineClearing
import monadris.infrastructure.game.Clock as GameClock
import monadris.infrastructure.terminal.ConsoleService
import monadris.infrastructure.terminal.Renderer
import monadris.infrastructure.terminal.TtyService
import monadris.input.GameCommand
import monadris.replay.*
import monadris.view.ScreenBuffer

object RecordingGameLoopRunner:

  final case class RecordingLoopState(
    gameState: GameState,
    previousBuffer: Option[ScreenBuffer],
    replayBuilder: ReplayBuilder
  )

  def recordingGameLoop(
    initialState: GameState,
    config: AppConfig,
    renderer: Renderer,
    randomPiece: GameRunner.RandomPiece
  ): ZIO[TtyService & ConsoleService, Throwable, (GameState, ReplayData)] =
    for
      startTime     <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
      commandQueue  <- Queue.bounded[GameCommand](EventLoop.CommandQueueCapacity)
      initialBuffer <- renderGame(renderer, initialState, config, None)
      initialInterval = LineClearing.dropInterval(initialState.level, config.speed)
      intervalRef <- Ref.make(initialInterval)
      initialBuilder = ReplayBuilder.create(
        timestamp = startTime,
        gridWidth = config.grid.width,
        gridHeight = config.grid.height,
        initialPiece = initialState.currentTetromino.shape,
        nextPiece = initialState.nextTetromino
      )
      inputFiber <- InputStream
        .run(commandQueue, config.terminal.inputPollIntervalMs)
        .provideSome[TtyService](ZLayer.succeed(config))
        .fork
      tickFiber      <- GameClock.run(commandQueue, intervalRef).fork
      finalLoopState <- eventLoop(
        commandQueue,
        RecordingLoopState(initialState, Some(initialBuffer), initialBuilder),
        config,
        intervalRef,
        renderer,
        randomPiece
      )
      _       <- inputFiber.interrupt
      _       <- tickFiber.interrupt
      endTime <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
      replayData = finalLoopState.replayBuilder.build(finalLoopState.gameState, endTime)
    yield (finalLoopState.gameState, replayData)

  private object EventLoop:
    val CommandQueueCapacity = 100

  private def eventLoop(
    queue: Queue[GameCommand],
    state: RecordingLoopState,
    config: AppConfig,
    intervalRef: Ref[Long],
    renderer: Renderer,
    randomPiece: GameRunner.RandomPiece
  ): ZIO[ConsoleService, Throwable, RecordingLoopState] =
    queue.take.flatMap {
      case GameCommand.Quit =>
        ZIO.succeed(state)
      case GameCommand.UserAction(input) =>
        handleCommand(input, state, config, intervalRef, queue, renderer, randomPiece, recordInput = true)
      case GameCommand.TimeTick =>
        handleCommand(Input.Tick, state, config, intervalRef, queue, renderer, randomPiece, recordInput = true)
    }

  private def handleCommand(
    input: Input,
    state: RecordingLoopState,
    config: AppConfig,
    intervalRef: Ref[Long],
    queue: Queue[GameCommand],
    renderer: Renderer,
    randomPiece: GameRunner.RandomPiece,
    recordInput: Boolean
  ): ZIO[ConsoleService, Throwable, RecordingLoopState] =
    for
      nextShape <- randomPiece.nextShape
      oldState = state.gameState
      outcome  = GameLoop.handleInput(oldState, input, nextShape, config)
      _ <- ZIO.when(outcome.state.isGameOver && !oldState.isGameOver) {
        ZIO.logInfo(
          s"Game Over - Score: ${outcome.state.score}, Lines: ${outcome.state.linesCleared}, Level: ${outcome.state.level}"
        )
      }
      _ <- outcome.nextIntervalMs match
        case Some(interval) => intervalRef.set(interval)
        case None           => ZIO.unit
      newBuffer <- renderGame(renderer, outcome.state, config, state.previousBuffer)
      updatedBuilder = recordEvents(state.replayBuilder, input, oldState, outcome.state, nextShape).advanceFrame
      newLoopState   = RecordingLoopState(outcome.state, Some(newBuffer), updatedBuilder)
      result <-
        if outcome.shouldContinue then eventLoop(queue, newLoopState, config, intervalRef, renderer, randomPiece)
        else ZIO.succeed(newLoopState)
    yield result

  private def recordEvents(
    builder: ReplayBuilder,
    input: Input,
    oldState: GameState,
    newState: GameState,
    nextShape: TetrominoShape
  ): ReplayBuilder =
    val builderWithInput = builder.recordInput(input)
    if pieceWasLocked(oldState, newState) then builderWithInput.recordPieceSpawn(nextShape)
    else builderWithInput

  private def pieceWasLocked(oldState: GameState, newState: GameState): Boolean =
    oldState.currentTetromino.shape != newState.currentTetromino.shape ||
      oldState.nextTetromino != newState.nextTetromino

  private def renderGame(
    renderer: Renderer,
    state: GameState,
    config: AppConfig,
    previousBuffer: Option[ScreenBuffer]
  ): ZIO[ConsoleService, Throwable, ScreenBuffer] =
    renderer.render(state, config, previousBuffer)

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
import monadris.view.ScreenBuffer

object GameLoopRunner:

  final case class LoopState(
    gameState: GameState,
    previousBuffer: Option[ScreenBuffer]
  )

  def interactiveGameLoop(
    initialState: GameState,
    config: AppConfig,
    renderer: Renderer,
    randomPiece: GameRunner.RandomPiece
  ): ZIO[TtyService & ConsoleService, Throwable, GameState] =
    for
      commandQueue  <- Queue.bounded[GameCommand](EventLoop.CommandQueueCapacity)
      initialBuffer <- renderGame(renderer, initialState, config, None)
      initialInterval = LineClearing.dropInterval(initialState.level, config.speed)
      intervalRef <- Ref.make(initialInterval)
      inputFiber  <- InputStream
        .run(commandQueue, config.terminal.inputPollIntervalMs)
        .provideSome[TtyService](ZLayer.succeed(config))
        .fork
      tickFiber      <- GameClock.run(commandQueue, intervalRef).fork
      finalLoopState <- eventLoop(
        commandQueue,
        LoopState(initialState, Some(initialBuffer)),
        config,
        intervalRef,
        renderer,
        randomPiece
      )
      _ <- inputFiber.interrupt
      _ <- tickFiber.interrupt
    yield finalLoopState.gameState

  private object EventLoop:
    val CommandQueueCapacity = 100

  def eventLoop(
    queue: Queue[GameCommand],
    state: LoopState,
    config: AppConfig,
    intervalRef: Ref[Long],
    renderer: Renderer,
    randomPiece: GameRunner.RandomPiece
  ): ZIO[ConsoleService, Throwable, LoopState] =
    queue.take.flatMap {
      case GameCommand.Quit =>
        ZIO.succeed(state)
      case GameCommand.UserAction(input) =>
        handleCommand(input, state, config, intervalRef, queue, renderer, randomPiece)
      case GameCommand.TimeTick =>
        handleCommand(Input.Tick, state, config, intervalRef, queue, renderer, randomPiece)
    }

  private def handleCommand(
    input: Input,
    state: LoopState,
    config: AppConfig,
    intervalRef: Ref[Long],
    queue: Queue[GameCommand],
    renderer: Renderer,
    randomPiece: GameRunner.RandomPiece
  ): ZIO[ConsoleService, Throwable, LoopState] =
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
      newLoopState = LoopState(outcome.state, Some(newBuffer))
      result <-
        if outcome.shouldContinue then eventLoop(queue, newLoopState, config, intervalRef, renderer, randomPiece)
        else ZIO.succeed(newLoopState)
    yield result

  private def renderGame(
    renderer: Renderer,
    state: GameState,
    config: AppConfig,
    previousBuffer: Option[ScreenBuffer]
  ): ZIO[ConsoleService, Throwable, ScreenBuffer] =
    renderer.render(state, config, previousBuffer)

package monadris.infrastructure.runtime

import zio.*

import monadris.domain.*
import monadris.domain.config.AppConfig
import monadris.infrastructure.io.ConsoleService
import monadris.infrastructure.io.TtyService
import monadris.infrastructure.render.Renderer
import monadris.infrastructure.runtime.Clock as GameClock
import monadris.logic.GameLoop
import monadris.logic.LineClearing
import monadris.view.ScreenBuffer

object GameRunner:

  export monadris.infrastructure.runtime.GameCommand
  export monadris.infrastructure.render.Renderer

  private val renderer: Renderer = Renderer.live

  trait RandomPiece:
    def nextShape: UIO[TetrominoShape]

  object RandomPieceGenerator extends RandomPiece:
    private val shapes = TetrominoShape.values.toVector

    def nextShape: UIO[TetrominoShape] =
      Random.nextIntBounded(shapes.size).map(shapes(_))

  def showTitle: ZIO[ConsoleService, Throwable, Unit] =
    renderer.renderTitle

  def renderGame(
    state: GameState,
    config: AppConfig,
    previousBuffer: Option[ScreenBuffer] = None
  ): ZIO[ConsoleService, Throwable, ScreenBuffer] =
    renderer.render(state, config, previousBuffer)

  def renderGameOver(state: GameState): ZIO[ConsoleService, Throwable, Unit] =
    renderer.renderGameOver(state)

  private object EventLoop:
    val CommandQueueCapacity = 100

  private[infrastructure] case class LoopState(
    gameState: GameState,
    previousBuffer: Option[ScreenBuffer]
  )

  def interactiveGameLoop(
    initialState: GameState
  ): ZIO[TtyService & ConsoleService & AppConfig, Throwable, GameState] =
    for
      config        <- ZIO.service[AppConfig]
      commandQueue  <- Queue.bounded[GameCommand](EventLoop.CommandQueueCapacity)
      initialBuffer <- renderGame(initialState, config, None)
      initialInterval = LineClearing.dropInterval(initialState.level, config.speed)
      intervalRef    <- Ref.make(initialInterval)
      inputFiber     <- InputStream.run(commandQueue, config.terminal.inputPollIntervalMs).fork
      tickFiber      <- GameClock.run(commandQueue, intervalRef).fork
      finalLoopState <- eventLoop(
        commandQueue,
        LoopState(initialState, Some(initialBuffer)),
        config,
        intervalRef
      )
      _ <- inputFiber.interrupt
      _ <- tickFiber.interrupt
    yield finalLoopState.gameState

  private[infrastructure] def eventLoop(
    queue: Queue[GameCommand],
    state: LoopState,
    config: AppConfig,
    intervalRef: Ref[Long]
  ): ZIO[ConsoleService, Throwable, LoopState] =
    queue.take.flatMap {
      case GameCommand.Quit =>
        ZIO.succeed(state)

      case GameCommand.UserAction(input) =>
        handleCommand(input, state, config, intervalRef, queue)

      case GameCommand.TimeTick =>
        handleCommand(Input.Tick, state, config, intervalRef, queue)
    }

  private def handleCommand(
    input: Input,
    state: LoopState,
    config: AppConfig,
    intervalRef: Ref[Long],
    queue: Queue[GameCommand]
  ): ZIO[ConsoleService, Throwable, LoopState] =
    for
      nextShape <- RandomPieceGenerator.nextShape
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
      newBuffer <- renderGame(outcome.state, config, state.previousBuffer)
      newLoopState = LoopState(outcome.state, Some(newBuffer))
      result <-
        if outcome.shouldContinue then eventLoop(queue, newLoopState, config, intervalRef)
        else ZIO.succeed(newLoopState)
    yield result

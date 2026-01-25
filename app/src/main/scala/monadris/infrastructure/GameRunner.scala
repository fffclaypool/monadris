package monadris.infrastructure

import zio.*

import monadris.domain.*
import monadris.domain.config.AppConfig
import monadris.logic.*
import monadris.view.GameView
import monadris.view.ScreenBuffer

object GameRunner:

  enum GameCommand:
    case UserAction(input: Input)
    case TimeTick
    case Quit

  trait Renderer:
    def render(state: GameState): UIO[Unit]
    def renderGameOver(state: GameState): UIO[Unit]

  trait RandomPiece:
    def nextShape: UIO[TetrominoShape]

  object RandomPieceGenerator extends RandomPiece:
    private val shapes = TetrominoShape.values.toVector

    def nextShape: UIO[TetrominoShape] =
      Random.nextIntBounded(shapes.size).map(shapes(_))

  def showTitle: ZIO[ConsoleService, Throwable, Unit] =
    val buffer = GameView.titleScreen
    ConsoleRenderer.renderWithoutClear(buffer)

  def renderGame(
    state: GameState,
    config: AppConfig,
    previousBuffer: Option[ScreenBuffer] = None
  ): ZIO[ConsoleService, Throwable, ScreenBuffer] =
    val buffer = GameView.toScreenBuffer(state, config)
    ConsoleRenderer.render(buffer, previousBuffer).as(buffer)

  def renderGameOver(state: GameState): ZIO[ConsoleService, Throwable, Unit] =
    val buffer = GameView.gameOverScreen(state)
    ConsoleRenderer.renderWithoutClear(buffer)

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
      inputFiber     <- inputProducer(commandQueue, config.terminal.inputPollIntervalMs).fork
      tickFiber      <- tickProducer(commandQueue, intervalRef).fork
      finalLoopState <- eventLoop(
        commandQueue,
        LoopState(initialState, Some(initialBuffer)),
        config,
        intervalRef
      )
      _ <- inputFiber.interrupt
      _ <- tickFiber.interrupt
    yield finalLoopState.gameState

  private def inputProducer(
    queue: Queue[GameCommand],
    pollIntervalMs: Int
  ): ZIO[TtyService & AppConfig, Throwable, Unit] =
    val readAndOffer = for
      parseResult <- TerminalInput.readKeyZIO
      _           <- parseResult match
        case TerminalInput.ParseResult.Timeout =>
          TtyService.sleep(pollIntervalMs)
        case TerminalInput.ParseResult.Regular(key) if TerminalInput.isQuitKey(key) =>
          ZIO.logInfo("Quit key pressed") *> queue.offer(GameCommand.Quit)
        case _ =>
          TerminalInput.toInput(parseResult) match
            case Some(input) => queue.offer(GameCommand.UserAction(input))
            case None        => ZIO.unit
    yield ()

    readAndOffer.forever

  private def tickProducer(
    queue: Queue[GameCommand],
    intervalRef: Ref[Long]
  ): ZIO[TtyService, Throwable, Unit] =
    val tickAndSleep = for
      interval <- intervalRef.get
      _        <- TtyService.sleep(interval.toInt)
      _        <- queue.offer(GameCommand.TimeTick)
    yield ()

    tickAndSleep.forever

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
        processCommand(input, state, config, intervalRef).flatMap { newState =>
          if newState.gameState.isGameOver then ZIO.succeed(newState)
          else eventLoop(queue, newState, config, intervalRef)
        }

      case GameCommand.TimeTick =>
        processCommand(Input.Tick, state, config, intervalRef).flatMap { newState =>
          if newState.gameState.isGameOver then ZIO.succeed(newState)
          else eventLoop(queue, newState, config, intervalRef)
        }
    }

  private def processCommand(
    input: Input,
    state: LoopState,
    config: AppConfig,
    intervalRef: Ref[Long]
  ): ZIO[ConsoleService, Throwable, LoopState] =
    for
      nextShape <- RandomPieceGenerator.nextShape
      oldGameState = state.gameState
      newGameState = GameLogic.update(oldGameState, input, () => nextShape, config)
      _ <- ZIO.when(newGameState.isGameOver && !oldGameState.isGameOver) {
        ZIO.logInfo(
          s"Game Over - Score: ${newGameState.score}, Lines: ${newGameState.linesCleared}, Level: ${newGameState.level}"
        )
      }
      newInterval = LineClearing.dropInterval(newGameState.level, config.speed)
      _         <- ZIO.unless(newGameState.isGameOver)(intervalRef.set(newInterval))
      newBuffer <- renderGame(newGameState, config, state.previousBuffer)
    yield LoopState(newGameState, Some(newBuffer))

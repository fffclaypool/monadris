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

  /** One processed command: what was applied and how the game state changed */
  final case class Step(
    input: Input,
    before: GameState,
    after: GameState,
    nextShape: TetrominoShape,
    pieceLocked: Boolean
  )

  /** Folds every processed command into an accumulator (e.g. a replay recording) */
  type Observer[A] = (A, Step) => UIO[A]

  private object EventLoop:
    val CommandQueueCapacity = 100

  private val ignoreSteps: Observer[Unit] = (_, _) => ZIO.unit

  def interactiveGameLoop(
    initialState: GameState,
    config: AppConfig,
    renderer: Renderer,
    randomPiece: GameRunner.RandomPiece
  ): ZIO[TtyService & ConsoleService, Throwable, GameState] =
    observedGameLoop(initialState, config, renderer, randomPiece, ())(ignoreSteps).map { case (finalState, _) =>
      finalState
    }

  /**
   * Everything the loop needs that stays the same from one command to the next.
   * `upcomingShape` is the piece that enters the preview at the next lock; it is
   * drawn once and replaced only when it has been consumed.
   */
  final private case class LoopContext(
    queue: Queue[GameCommand],
    config: AppConfig,
    intervalRef: Ref[Long],
    renderer: Renderer,
    randomPiece: GameRunner.RandomPiece,
    upcomingShape: Ref[TetrominoShape]
  )

  private def makeContext(
    queue: Queue[GameCommand],
    config: AppConfig,
    intervalRef: Ref[Long],
    renderer: Renderer,
    randomPiece: GameRunner.RandomPiece
  ): UIO[LoopContext] =
    randomPiece.nextShape
      .flatMap(Ref.make(_))
      .map(LoopContext(queue, config, intervalRef, renderer, randomPiece, _))

  /**
   * Runs the interactive game and returns the final state together with whatever
   * the observer accumulated. The input and tick fibers live only as long as the loop.
   */
  def observedGameLoop[A](
    initialState: GameState,
    config: AppConfig,
    renderer: Renderer,
    randomPiece: GameRunner.RandomPiece,
    initialObserved: A
  )(observe: Observer[A]): ZIO[TtyService & ConsoleService, Throwable, (GameState, A)] =
    ZIO
      .scoped {
        for
          commandQueue  <- Queue.bounded[GameCommand](EventLoop.CommandQueueCapacity)
          initialBuffer <- renderer.render(initialState, config, None)
          intervalRef   <- Ref.make(LineClearing.dropInterval(initialState.level, config.speed))
          _             <- InputStream
            .run(commandQueue, config.terminal.inputPollIntervalMs)
            .provideSome[TtyService](ZLayer.succeed(config))
            .forkScoped
          _       <- GameClock.run(commandQueue, intervalRef).forkScoped
          context <- makeContext(commandQueue, config, intervalRef, renderer, randomPiece)
          result  <- observedEventLoop(context, LoopState(initialState, Some(initialBuffer)), initialObserved)(observe)
        yield result
      }
      .map { case (loopState, observed) => (loopState.gameState, observed) }

  def eventLoop(
    queue: Queue[GameCommand],
    state: LoopState,
    config: AppConfig,
    intervalRef: Ref[Long],
    renderer: Renderer,
    randomPiece: GameRunner.RandomPiece
  ): ZIO[ConsoleService, Throwable, LoopState] =
    makeContext(queue, config, intervalRef, renderer, randomPiece)
      .flatMap(observedEventLoop(_, state, ())(ignoreSteps))
      .map { case (loopState, _) => loopState }

  private def observedEventLoop[A](
    context: LoopContext,
    state: LoopState,
    observed: A
  )(observe: Observer[A]): ZIO[ConsoleService, Throwable, (LoopState, A)] =
    def continueWith(input: Input): ZIO[ConsoleService, Throwable, (LoopState, A)] =
      for
        stepResult <- handleCommand(context, input, state)
        (newLoopState, step) = stepResult
        newObserved <- observe(observed, step)
        result      <-
          if newLoopState.gameState.isGameOver then ZIO.succeed((newLoopState, newObserved))
          else observedEventLoop(context, newLoopState, newObserved)(observe)
      yield result

    context.queue.take.flatMap {
      case GameCommand.Quit              => ZIO.succeed((state, observed))
      case GameCommand.UserAction(input) => continueWith(input)
      case GameCommand.TimeTick          => continueWith(Input.Tick)
    }

  private def handleCommand(
    context: LoopContext,
    input: Input,
    state: LoopState
  ): ZIO[ConsoleService, Throwable, (LoopState, Step)] =
    for
      nextShape <- context.upcomingShape.get
      oldState = state.gameState
      outcome  = GameLoop.handleInput(oldState, input, nextShape, context.config.game)
      _ <- ZIO.when(outcome.state.isGameOver && !oldState.isGameOver) {
        ZIO.logInfo(
          s"Game Over - Score: ${outcome.state.score}, Lines: ${outcome.state.linesCleared}, Level: ${outcome.state.level}"
        )
      }
      _         <- ZIO.when(outcome.pieceLocked)(context.randomPiece.nextShape.flatMap(context.upcomingShape.set))
      _         <- ZIO.foreachDiscard(outcome.nextIntervalMs)(context.intervalRef.set)
      newBuffer <- context.renderer.render(outcome.state, context.config, state.previousBuffer)
    yield (
      LoopState(outcome.state, Some(newBuffer)),
      Step(input, oldState, outcome.state, nextShape, outcome.pieceLocked)
    )

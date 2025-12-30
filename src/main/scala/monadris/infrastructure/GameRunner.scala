package monadris.infrastructure

import zio.*

import monadris.config.AppConfig
import monadris.domain.*
import monadris.logic.*
import monadris.view.GameView

/**
 * 副作用をZIOで管理するゲーム実行層
 * コアロジックは純粋関数のまま、入出力のみをエフェクトとして扱う
 */
object GameRunner:

  /**
   * 描画を抽象化するトレイト（依存性注入用）
   */
  trait Renderer:
    def render(state: GameState): UIO[Unit]
    def renderGameOver(state: GameState): UIO[Unit]

  /**
   * 乱数生成を抽象化
   */
  trait RandomPiece:
    def nextShape: UIO[TetrominoShape]

  /**
   * ランダムピース生成器
   */
  object RandomPieceGenerator extends RandomPiece:
    private val shapes = TetrominoShape.values.toVector

    def nextShape: UIO[TetrominoShape] =
      Random.nextIntBounded(shapes.size).map(shapes(_))

  /**
   * タイトル画面を表示
   */
  def showTitle: ZIO[ConsoleService, Throwable, Unit] =
    val buffer = GameView.titleScreen
    ConsoleRenderer.renderWithoutClear(buffer)

  /**
   * ゲーム画面を描画
   */
  def renderGame(state: GameState, config: AppConfig): ZIO[ConsoleService, Throwable, Unit] =
    val buffer = GameView.toScreenBuffer(state, config)
    ConsoleRenderer.render(buffer)

  /**
   * ゲームオーバー画面を描画
   */
  def renderGameOver(state: GameState): ZIO[ConsoleService, Throwable, Unit] =
    val buffer = GameView.gameOverScreen(state)
    ConsoleRenderer.renderWithoutClear(buffer)

  // ============================================================
  // インタラクティブゲームループ（サービス依存版）
  // ============================================================

  /**
   * インタラクティブなゲームループを実行（TtyService + ConsoleService版）
   */
  def interactiveGameLoop(
    initialState: GameState
  ): ZIO[TtyService & ConsoleService & AppConfig, Throwable, GameState] =
    for
      config   <- ZIO.service[AppConfig]
      stateRef <- Ref.make(initialState)
      quitRef  <- Ref.make(false)
      _        <- renderCurrentStateZIO(stateRef, config)
      tickFiber <- tickLoopZIO(stateRef, quitRef).fork
      _        <- inputLoopZIO(stateRef, quitRef)
      _        <- tickFiber.interrupt
      finalState <- stateRef.get
    yield finalState

  private def renderCurrentStateZIO(
    stateRef: Ref[GameState],
    config: AppConfig
  ): ZIO[ConsoleService, Throwable, Unit] =
    stateRef.get.flatMap(state => renderGame(state, config))

  private def tickLoopZIO(
    stateRef: Ref[GameState],
    quitRef: Ref[Boolean]
  ): ZIO[TtyService & ConsoleService & AppConfig, Throwable, Unit] =
    val shouldContinue = checkGameActiveZIO(stateRef, quitRef)
    val processTick = processTickUpdateZIO(stateRef)

    val tick = shouldContinue.flatMap {
      case false => ZIO.succeed(false)
      case true  => processTick
    }
    tick.repeatWhile(identity).unit

  private def checkGameActiveZIO(
    stateRef: Ref[GameState],
    quitRef: Ref[Boolean]
  ): UIO[Boolean] =
    for
      quit <- quitRef.get
      state <- stateRef.get
    yield !quit && !state.isGameOver

  private def processTickUpdateZIO(
    stateRef: Ref[GameState]
  ): ZIO[TtyService & ConsoleService & AppConfig, Throwable, Boolean] =
    for
      config <- ZIO.service[AppConfig]
      nextShape <- RandomPieceGenerator.nextShape
      _ <- stateRef.update(s => GameLogic.update(s, Input.Tick, () => nextShape, config))
      newState <- stateRef.get
      _ <- renderGame(newState, config)
      interval = LineClearing.dropInterval(newState.level, config.speed)
      _ <- TtyService.sleep(interval)
    yield !newState.isGameOver

  private def inputLoopZIO(
    stateRef: Ref[GameState],
    quitRef: Ref[Boolean]
  ): ZIO[TtyService & ConsoleService & AppConfig, Throwable, Unit] =
    processInputLoopZIO(stateRef, quitRef)

  private def processInputLoopZIO(
    stateRef: Ref[GameState],
    quitRef: Ref[Boolean]
  ): ZIO[TtyService & ConsoleService & AppConfig, Throwable, Unit] =
    val step = checkGameActiveZIO(stateRef, quitRef).flatMap {
      case false => ZIO.succeed(false)
      case true  => readAndHandleKeyZIO(stateRef, quitRef)
    }
    step.repeatWhile(identity).unit

  private def readAndHandleKeyZIO(
    stateRef: Ref[GameState],
    quitRef: Ref[Boolean]
  ): ZIO[TtyService & ConsoleService & AppConfig, Throwable, Boolean] =
    for
      config      <- ZIO.service[AppConfig]
      parseResult <- TerminalInput.readKeyZIO
      result <- parseResult match
        case TerminalInput.ParseResult.Timeout =>
          TtyService.sleep(config.terminal.inputPollIntervalMs).as(true)
        case TerminalInput.ParseResult.Regular(key) if TerminalInput.isQuitKey(key) =>
          ZIO.logInfo("Quit key pressed") *> quitRef.set(true).as(false)
        case _ =>
          handleParsedInput(parseResult, stateRef, config)
    yield result

  private def handleParsedInput(
    parseResult: TerminalInput.ParseResult,
    stateRef: Ref[GameState],
    config: AppConfig
  ): ZIO[ConsoleService, Throwable, Boolean] =
    TerminalInput.toInput(parseResult) match
      case None        => ZIO.succeed(true)
      case Some(input) => applyInputZIO(input, stateRef, config)

  private def applyInputZIO(
    input: Input,
    stateRef: Ref[GameState],
    config: AppConfig
  ): ZIO[ConsoleService, Throwable, Boolean] =
    for
      _ <- ZIO.logDebug(s"Input received: $input")
      nextShape <- RandomPieceGenerator.nextShape
      oldState <- stateRef.get
      _ <- stateRef.update(s => GameLogic.update(s, input, () => nextShape, config))
      newState <- stateRef.get
      _ <- ZIO.when(newState.isGameOver && !oldState.isGameOver) {
        ZIO.logInfo(s"Game Over - Score: ${newState.score}, Lines: ${newState.linesCleared}, Level: ${newState.level}")
      }
      _ <- renderGame(newState, config)
    yield !newState.isGameOver

package monadris.effect

import zio.*

import monadris.config.AppConfig
import monadris.domain.*
import monadris.logic.*

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

  // raw modeでは \r\n が必要
  private val NL = "\r\n"

  /**
   * ConsoleService依存のレンダラー（テスト可能版）
   */
  object ServiceRenderer:
    // ANSI color codes
    private val ANSI_RESET = "\u001b[0m"
    private val ANSI_CYAN = "\u001b[36m"
    private val ANSI_YELLOW = "\u001b[33m"
    private val ANSI_MAGENTA = "\u001b[35m"
    private val ANSI_GREEN = "\u001b[32m"
    private val ANSI_RED = "\u001b[31m"
    private val ANSI_BLUE = "\u001b[34m"
    private val ANSI_WHITE = "\u001b[37m"

    private def getColor(shape: TetrominoShape): String = shape match
      case TetrominoShape.I => ANSI_CYAN
      case TetrominoShape.O => ANSI_YELLOW
      case TetrominoShape.T => ANSI_MAGENTA
      case TetrominoShape.S => ANSI_GREEN
      case TetrominoShape.Z => ANSI_RED
      case TetrominoShape.J => ANSI_BLUE
      case TetrominoShape.L => ANSI_WHITE
    /**
     * タイトル画面を表示
     */
    def showTitle: ZIO[ConsoleService, Throwable, Unit] =
      val lines = List(
        "╔════════════════════════════════════╗",
        "║    🎮 Functional Tetris            ║",
        "║    Scala 3 + ZIO                   ║",
        "╠════════════════════════════════════╣",
        "║  Controls:                         ║",
        "║    ← → or H L : Move left/right    ║",
        "║    ↓ or J     : Soft drop          ║",
        "║    ↑ or K     : Rotate             ║",
        "║    Z          : Rotate CCW         ║",
        "║    Space      : Hard drop          ║",
        "║    P          : Pause              ║",
        "║    Q          : Quit               ║",
        "╚════════════════════════════════════╝",
        ""
      )
      ZIO.foreachDiscard(lines)(line => ConsoleService.print(line + NL))

    def render(state: GameState): ZIO[ConsoleService, Throwable, Unit] =
      for
        _ <- ConsoleService.print("\u001b[H\u001b[2J\u001b[3J")
        gridDisplay = renderGrid(state)
        info = List(
          s"Score: ${state.score}",
          s"Level: ${state.level}",
          s"Lines: ${state.linesCleared}",
          s"Next: ${state.nextTetromino}",
          "",
          "H/L or ←/→: Move  K or ↑: Rotate",
          "J or ↓: Drop  Space: Hard drop",
          "P: Pause  Q: Quit"
        ).mkString(NL)
        _ <- ConsoleService.print(gridDisplay)
        _ <- ConsoleService.print(NL)
        _ <- ConsoleService.print(info)
        _ <- ConsoleService.print(NL)
        _ <- ConsoleService.flush()
      yield ()

    def renderGameOver(state: GameState): ZIO[ConsoleService, Throwable, Unit] =
      val msg = List(
        "",
        "╔═══════════════════════╗",
        "║      GAME OVER!       ║",
        "╠═══════════════════════╣",
        s"║  Score: ${"%6d".format(state.score)}        ║",
        s"║  Lines: ${"%6d".format(state.linesCleared)}        ║",
        s"║  Level: ${"%6d".format(state.level)}        ║",
        "╚═══════════════════════╝"
      ).mkString(NL)
      for
        _ <- ConsoleService.print(msg)
        _ <- ConsoleService.print(NL)
        _ <- ConsoleService.flush()
      yield ()

    private def renderGrid(state: GameState): String =
      val grid = state.grid
      val width = grid.width
      val fallingBlocks = state.currentTetromino.currentBlocks.toSet
      val fallingColor  = getColor(state.currentTetromino.shape)

      def renderCell(x: Int, y: Int): String =
        val pos = Position(x, y)
        if fallingBlocks.contains(pos) then
          s"$fallingColor█$ANSI_RESET"
        else grid.get(pos) match
          case Some(Cell.Filled(shape)) => s"${getColor(shape)}▓$ANSI_RESET"
          case _                        => "·"

      val rows = (0 until grid.height).map { y =>
        val rowContent = (0 until width).map(x => renderCell(x, y)).mkString
        s"│$rowContent│"
      }
      val border = "─" * width
      val top    = s"┌$border┐"
      val bottom = s"└$border┘"

      (top +: rows :+ bottom).mkString(NL)

  /**
   * ランダムピース生成器
   */
  object RandomPieceGenerator extends RandomPiece:
    private val shapes = TetrominoShape.values.toVector

    def nextShape: UIO[TetrominoShape] =
      Random.nextIntBounded(shapes.size).map(shapes(_))

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
      stateRef <- Ref.make(initialState)
      quitRef <- Ref.make(false)
      _ <- renderCurrentStateZIO(stateRef)
      tickFiber <- tickLoopZIO(stateRef, quitRef).fork
      _ <- inputLoopZIO(stateRef, quitRef)
      _ <- tickFiber.interrupt
      finalState <- stateRef.get
    yield finalState

  private def renderCurrentStateZIO(
    stateRef: Ref[GameState]
  ): ZIO[ConsoleService, Throwable, Unit] =
    stateRef.get.flatMap(ServiceRenderer.render)

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
      _ <- ServiceRenderer.render(newState)
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
      _ <- ServiceRenderer.render(newState)
    yield !newState.isGameOver

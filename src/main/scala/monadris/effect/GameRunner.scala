package monadris.effect

import zio.*
import zio.stream.UStream
import zio.stream.ZStream

import monadris.domain.*
import monadris.logic.*

/**
 * 副作用をZIOで管理するゲーム実行層
 * コアロジックは純粋関数のまま、入出力のみをエフェクトとして扱う
 */
object GameRunner:

  private case object GameEnded extends RuntimeException

  /**
   * 描画を抽象化するトレイト（依存性注入用）
   */
  trait Renderer:
    def render(state: GameState): UIO[Unit]
    def renderGameOver(state: GameState): UIO[Unit]

  /**
   * 入力を抽象化するトレイト
   */
  trait InputHandler:
    def nextInput: UIO[Option[Input]]

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
      val currentBlocks = state.currentTetromino.currentBlocks.toSet
      val currentColor = getColor(state.currentTetromino.shape)

      val rows = for y <- 0 until grid.height yield
        val cells = for x <- 0 until grid.width yield
          val pos = Position(x, y)
          if currentBlocks.contains(pos) then
            currentColor + "█" + ANSI_RESET
          else grid.get(pos) match
            case Some(Cell.Filled(shape)) =>
              getColor(shape) + "▓" + ANSI_RESET
            case _ => "·"
        "│" + cells.mkString + "│"

      val top = "┌" + "─" * grid.width + "┐"
      val bottom = "└" + "─" * grid.width + "┘"
      (top +: rows :+ bottom).mkString(NL)

  /**
   * ランダムピース生成器
   */
  object RandomPieceGenerator extends RandomPiece:
    private val shapes = TetrominoShape.values.toVector

    def nextShape: UIO[TetrominoShape] =
      Random.nextIntBounded(shapes.size).map(shapes(_))

  /**
   * ゲームループの構造
   * 純粋なコアロジックをZIOストリームでラップ
   */
  def gameLoop(
    initialState: GameState,
    inputStream: UStream[Input],
    renderer: Renderer,
    randomPiece: RandomPiece
  ): UIO[GameState] =

    // Refを使って状態を管理（内部的には可変だが、外部からは不変）
    for
      stateRef <- Ref.make(initialState)

      // Tick生成（レベルに応じて間隔が変化）
      tickFiber <- createTickStream(stateRef)
        .foreach(_ => processInput(stateRef, Input.Tick, randomPiece, renderer))
        .fork

      // 入力処理
      _ <- inputStream
        .takeWhile(_ => true) // 無限ストリーム
        .foreach { input =>
          for
            state <- stateRef.get
            _ <- (
              if state.isGameOver
              then renderer.renderGameOver(state) *> ZIO.fail(GameEnded)
              else if input == Input.Quit
              then ZIO.fail(GameEnded)
              else processInput(stateRef, input, randomPiece, renderer)
            )
          yield ()
        }
        .catchAll { case GameEnded => ZIO.unit }
        .race(tickFiber.join) // どちらかが終了したら終了

      finalState <- stateRef.get
    yield finalState

  /**
   * 入力を処理して状態を更新
   */
  private def processInput(
    stateRef: Ref[GameState],
    input: Input,
    randomPiece: RandomPiece,
    renderer: Renderer
  ): UIO[Unit] =
    for
      nextShape <- randomPiece.nextShape
      _ <- stateRef.update { state =>
        GameLogic.update(state, input, () => nextShape)
      }
      state <- stateRef.get
      _ <- renderer.render(state)
    yield ()

  /**
   * レベルに応じたTick間隔でストリームを生成
   */
  private def createTickStream(
    stateRef: Ref[GameState]
  ): UStream[Unit] =
    ZStream.repeatZIOWithSchedule(
      stateRef.get.map(s => LineClearing.dropInterval(s.level)),
      Schedule.fixed(100.millis) // 基本間隔
    ).mapZIO { interval =>
      ZIO.sleep(Duration.fromMillis(interval - 100)) // 動的間隔調整
    }

  // ============================================================
  // インタラクティブゲームループ（サービス依存版）
  // ============================================================

  /** 入力ポーリング待機時間（ミリ秒） */
  private final val InputPollIntervalMs: Int = 20

  /**
   * インタラクティブなゲームループを実行（TtyService + ConsoleService版）
   */
  def interactiveGameLoop(
    initialState: GameState
  ): ZIO[TtyService & ConsoleService, Throwable, GameState] =
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
  ): ZIO[TtyService & ConsoleService, Throwable, Unit] =
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
  ): ZIO[TtyService & ConsoleService, Throwable, Boolean] =
    for
      nextShape <- RandomPieceGenerator.nextShape
      _ <- stateRef.update(s => GameLogic.update(s, Input.Tick, () => nextShape))
      newState <- stateRef.get
      _ <- ServiceRenderer.render(newState)
      interval = LineClearing.dropInterval(newState.level)
      _ <- TtyService.sleep(interval)
    yield !newState.isGameOver

  private def inputLoopZIO(
    stateRef: Ref[GameState],
    quitRef: Ref[Boolean]
  ): ZIO[TtyService & ConsoleService, Throwable, Unit] =
    processInputLoopZIO(stateRef, quitRef)

  private def processInputLoopZIO(
    stateRef: Ref[GameState],
    quitRef: Ref[Boolean]
  ): ZIO[TtyService & ConsoleService, Throwable, Unit] =
    val step = checkGameActiveZIO(stateRef, quitRef).flatMap {
      case false => ZIO.succeed(false)
      case true  => readAndHandleKeyZIO(stateRef, quitRef)
    }
    step.repeatWhile(identity).unit

  private def readAndHandleKeyZIO(
    stateRef: Ref[GameState],
    quitRef: Ref[Boolean]
  ): ZIO[TtyService & ConsoleService, Throwable, Boolean] =
    for
      parseResult <- TerminalInput.readKeyZIO
      result <- parseResult match
        case TerminalInput.ParseResult.Timeout =>
          TtyService.sleep(InputPollIntervalMs).as(true)
        case TerminalInput.ParseResult.Regular(key) if TerminalInput.isQuitKey(key) =>
          ZIO.logInfo("Quit key pressed") *> quitRef.set(true).as(false)
        case _ =>
          handleParsedInput(parseResult, stateRef)
    yield result

  private def handleParsedInput(
    parseResult: TerminalInput.ParseResult,
    stateRef: Ref[GameState]
  ): ZIO[ConsoleService, Throwable, Boolean] =
    TerminalInput.toInput(parseResult) match
      case None        => ZIO.succeed(true)
      case Some(input) => applyInputZIO(input, stateRef)

  private def applyInputZIO(
    input: Input,
    stateRef: Ref[GameState]
  ): ZIO[ConsoleService, Throwable, Boolean] =
    for
      _ <- ZIO.logDebug(s"Input received: $input")
      nextShape <- RandomPieceGenerator.nextShape
      oldState <- stateRef.get
      _ <- stateRef.update(s => GameLogic.update(s, input, () => nextShape))
      newState <- stateRef.get
      _ <- ZIO.when(newState.isGameOver && !oldState.isGameOver) {
        ZIO.logInfo(s"Game Over - Score: ${newState.score}, Lines: ${newState.linesCleared}, Level: ${newState.level}")
      }
      _ <- ServiceRenderer.render(newState)
    yield !newState.isGameOver

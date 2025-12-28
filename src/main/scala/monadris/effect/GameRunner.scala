package monadris.effect

import java.io.FileInputStream

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

  /**
   * コンソール用の簡易レンダラー
   */
  object ConsoleRenderer extends Renderer:
    // raw modeでは \r\n が必要
    private val NL = "\r\n"

    /**
     * タイトル画面を表示
     */
    def showTitle: Task[Unit] =
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
      ZIO.foreachDiscard(lines)(Console.printLine(_))

    def render(state: GameState): UIO[Unit] = ZIO.succeed {
      // ANSI エスケープでカーソルを先頭に移動して画面クリア
      print("\u001b[H\u001b[2J\u001b[3J")

      val gridDisplay = renderGrid(state)
      val info = List(
        s"Score: ${state.score}",
        s"Level: ${state.level}",
        s"Lines: ${state.linesCleared}",
        s"Next: ${state.nextTetromino}",
        "",
        "H/L or ←/→: Move  K or ↑: Rotate",
        "J or ↓: Drop  Space: Hard drop",
        "P: Pause  Q: Quit"
      ).mkString(NL)

      print(gridDisplay)
      print(NL)
      print(info)
      print(NL)
      java.lang.System.out.flush()
    }

    def renderGameOver(state: GameState): UIO[Unit] = ZIO.succeed {
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
      print(msg)
      print(NL)
      java.lang.System.out.flush()
    }

    private def renderGrid(state: GameState): String =
      val grid = state.grid
      val currentBlocks = state.currentTetromino.currentBlocks.toSet

      val rows = for y <- 0 until grid.height yield
        val cells = for x <- 0 until grid.width yield
          val pos = Position(x, y)
          if currentBlocks.contains(pos) then "█"
          else grid.get(pos) match
            case Some(Cell.Filled(_)) => "▓"
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
  // インタラクティブゲームループ（Main.scalaから移動）
  // ============================================================

  /** 入力ポーリング待機時間（ミリ秒） */
  private final val InputPollIntervalMs: Int = 20

  /**
   * インタラクティブなゲームループを実行
   */
  def interactiveGameLoop(initialState: GameState): Task[GameState] =
    for
      stateRef <- Ref.make(initialState)
      quitRef <- Ref.make(false)
      _ <- renderCurrentState(stateRef)
      tickFiber <- tickLoop(stateRef, quitRef).fork
      _ <- inputLoop(stateRef, quitRef)
      _ <- tickFiber.interrupt
      finalState <- stateRef.get
    yield finalState

  private def renderCurrentState(stateRef: Ref[GameState]): UIO[Unit] =
    stateRef.get.flatMap(ConsoleRenderer.render)

  private def tickLoop(stateRef: Ref[GameState], quitRef: Ref[Boolean]): UIO[Unit] =
    val shouldContinue = checkGameActive(stateRef, quitRef)
    val processTick = processTickUpdate(stateRef)

    val tick = shouldContinue.flatMap {
      case false => ZIO.succeed(false)
      case true  => processTick
    }
    tick.repeatWhile(identity).unit

  private def checkGameActive(
    stateRef: Ref[GameState],
    quitRef: Ref[Boolean]
  ): UIO[Boolean] =
    for
      quit <- quitRef.get
      state <- stateRef.get
    yield !quit && !state.isGameOver

  private def processTickUpdate(stateRef: Ref[GameState]): UIO[Boolean] =
    for
      nextShape <- RandomPieceGenerator.nextShape
      _ <- stateRef.update(s => GameLogic.update(s, Input.Tick, () => nextShape))
      newState <- stateRef.get
      _ <- ConsoleRenderer.render(newState)
      interval = LineClearing.dropInterval(newState.level)
      _ <- ZIO.sleep(Duration.fromMillis(interval))
    yield !newState.isGameOver

  private def inputLoop(stateRef: Ref[GameState], quitRef: Ref[Boolean]): Task[Unit] =
    ZIO.acquireReleaseWith(
      ZIO.attempt(new FileInputStream("/dev/tty"))
    )(fis => ZIO.succeed(fis.close())) { ttyIn =>
      processInputLoop(ttyIn, stateRef, quitRef)
    }

  private def processInputLoop(
    ttyIn: FileInputStream,
    stateRef: Ref[GameState],
    quitRef: Ref[Boolean]
  ): Task[Unit] =
    val step = checkGameActive(stateRef, quitRef).flatMap {
      case false => ZIO.succeed(false)
      case true  => readAndHandleKey(ttyIn, stateRef, quitRef)
    }
    step.repeatWhile(identity).unit

  private def readAndHandleKey(
    ttyIn: FileInputStream,
    stateRef: Ref[GameState],
    quitRef: Ref[Boolean]
  ): Task[Boolean] =
    for
      keyOpt <- ZIO.attemptBlocking(readKeyFromTty(ttyIn))
      result <- keyOpt match
        case None      => ZIO.sleep(InputPollIntervalMs.millis).as(true)
        case Some(key) => handleKey(key, ttyIn, stateRef, quitRef)
    yield result

  private def readKeyFromTty(ttyIn: FileInputStream): Option[Int] =
    if ttyIn.available() > 0 then Some(ttyIn.read()) else None

  private def handleKey(
    key: Int,
    ttyIn: FileInputStream,
    stateRef: Ref[GameState],
    quitRef: Ref[Boolean]
  ): Task[Boolean] =
    if TerminalInput.isQuitKey(key) then
      quitRef.set(true).as(false)
    else
      parseAndApplyInput(key, ttyIn, stateRef)

  private def parseAndApplyInput(
    key: Int,
    ttyIn: FileInputStream,
    stateRef: Ref[GameState]
  ): Task[Boolean] =
    for
      inputOpt <- ZIO.attemptBlocking(parseKeyToInput(key, ttyIn))
      result <- inputOpt match
        case None        => ZIO.succeed(true)
        case Some(input) => applyInput(input, stateRef)
    yield result

  private def parseKeyToInput(key: Int, ttyIn: FileInputStream): Option[Input] =
    if key == TerminalInput.EscapeKeyCode then TerminalInput.parseEscapeSequence(ttyIn)
    else TerminalInput.keyToInput(key)

  private def applyInput(input: Input, stateRef: Ref[GameState]): UIO[Boolean] =
    for
      nextShape <- RandomPieceGenerator.nextShape
      _ <- stateRef.update(s => GameLogic.update(s, input, () => nextShape))
      newState <- stateRef.get
      _ <- ConsoleRenderer.render(newState)
    yield !newState.isGameOver

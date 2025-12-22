package monadris.effect

import zio.*
import zio.stream.*
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
    inputStream: ZStream[Any, Nothing, Input],
    renderer: Renderer,
    randomPiece: RandomPiece
  ): ZIO[Any, Nothing, GameState] =

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
              then renderer.renderGameOver(state) *> ZIO.interrupt
              else processInput(stateRef, input, randomPiece, renderer)
            )
          yield ()
        }
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
  ): ZStream[Any, Nothing, Unit] =
    ZStream.repeatZIOWithSchedule(
      stateRef.get.map(s => LineClearing.dropInterval(s.level)),
      Schedule.fixed(100.millis) // 基本間隔
    ).mapZIO { interval =>
      ZIO.sleep(Duration.fromMillis(interval - 100)) // 動的間隔調整
    }

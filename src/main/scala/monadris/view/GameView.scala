package monadris.view

import scala.util.chaining.*

import monadris.config.AppConfig
import monadris.domain.*

/**
 * GameState を ScreenBuffer に変換する純粋関数群
 * ANSIコードに依存せず、色は UiColor で表現
 */
object GameView:

  // raw modeでは \r\n が必要だが、ScreenBufferは行単位なので不要
  private val FilledBlock = '█'
  private val LockedBlock = '▓'
  private val EmptyCell = '·'

  /**
   * テトリミノの形状から色を取得
   */
  def shapeToColor(shape: TetrominoShape): UiColor = shape match
    case TetrominoShape.I => UiColor.Cyan
    case TetrominoShape.O => UiColor.Yellow
    case TetrominoShape.T => UiColor.Magenta
    case TetrominoShape.S => UiColor.Green
    case TetrominoShape.Z => UiColor.Red
    case TetrominoShape.J => UiColor.Blue
    case TetrominoShape.L => UiColor.White

  /**
   * ゲーム状態を画面バッファに変換
   */
  def toScreenBuffer(state: GameState, config: AppConfig): ScreenBuffer =
    val gridWidth = config.grid.width
    val gridHeight = config.grid.height

    // グリッド + 枠線 + 情報欄用の幅と高さを計算
    // 枠線: 左右に1文字ずつ、上下に1行ずつ
    // 情報欄: 右側に表示（幅30程度）
    val totalWidth = gridWidth + 2 + 30
    val totalHeight = gridHeight + 2 + 4  // 操作説明用に追加行

    ScreenBuffer.empty(totalWidth, totalHeight)
      .pipe(renderGrid(_, state, gridWidth, gridHeight))
      .pipe(renderInfo(_, state, gridWidth + 4))
      .pipe(renderControls(_, gridHeight + 2))

  /**
   * グリッドと枠線を描画
   */
  private def renderGrid(
    buffer: ScreenBuffer,
    state: GameState,
    gridWidth: Int,
    gridHeight: Int
  ): ScreenBuffer =
    val grid = state.grid
    val fallingBlocks = state.currentTetromino.currentBlocks.toSet
    val fallingColor = shapeToColor(state.currentTetromino.shape)

    // 上枠
    val topBorder = "┌" + "─" * gridWidth + "┐"
    val withTopBorder = buffer.drawText(0, 0, topBorder)

    // グリッド本体（行ごとに一括描画）
    val withGrid = (0 until gridHeight).foldLeft(withTopBorder) { (buf, y) =>
      // その行のグリッド部分のピクセル列を一括生成
      val rowPixels = (0 until gridWidth).map { x =>
        val pos = Position(x, y)
        if fallingBlocks.contains(pos) then
          Pixel(FilledBlock, fallingColor)
        else grid.get(pos) match
          case Some(Cell.Filled(shape)) => Pixel(LockedBlock, shapeToColor(shape))
          case _ => Pixel(EmptyCell, UiColor.Default)
      }.toVector

      // 左枠、グリッド一括描画、右枠をチェーンして更新
      buf
        .drawChar(0, y + 1, '│')
        .drawPixels(1, y + 1, rowPixels)
        .drawChar(gridWidth + 1, y + 1, '│')
    }

    // 下枠
    val bottomBorder = "└" + "─" * gridWidth + "┘"
    withGrid.drawText(0, gridHeight + 1, bottomBorder)

  /**
   * 情報欄を描画
   */
  private def renderInfo(buffer: ScreenBuffer, state: GameState, startX: Int): ScreenBuffer =
    buffer
      .drawText(startX, 1, s"Score: ${state.score}")
      .drawText(startX, 2, s"Level: ${state.level}")
      .drawText(startX, 3, s"Lines: ${state.linesCleared}")
      .drawText(startX, 5, s"Next: ${state.nextTetromino}")
      .drawText(startX, 7, if state.status == GameStatus.Paused then "** PAUSED **" else "")

  /**
   * 操作説明を描画
   */
  private def renderControls(buffer: ScreenBuffer, startY: Int): ScreenBuffer =
    buffer
      .drawText(0, startY, "H/L or ←/→: Move  K or ↑: Rotate")
      .drawText(0, startY + 1, "J or ↓: Drop  Space: Hard drop")
      .drawText(0, startY + 2, "P: Pause  Q: Quit")

  /**
   * タイトル画面用のバッファを生成
   */
  def titleScreen: ScreenBuffer =
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
      "╚════════════════════════════════════╝"
    )
    val width = lines.map(_.length).maxOption.getOrElse(40)
    val height = lines.length + 1

    lines.zipWithIndex.foldLeft(ScreenBuffer.empty(width, height)) { case (buf, (line, y)) =>
      buf.drawText(0, y, line)
    }

  /**
   * ゲームオーバー画面用のバッファを生成
   */
  def gameOverScreen(state: GameState): ScreenBuffer =
    val lines = List(
      "",
      "╔═══════════════════════╗",
      "║      GAME OVER!       ║",
      "╠═══════════════════════╣",
      s"║  Score: ${"%6d".format(state.score)}        ║",
      s"║  Lines: ${"%6d".format(state.linesCleared)}        ║",
      s"║  Level: ${"%6d".format(state.level)}        ║",
      "╚═══════════════════════╝"
    )
    val width = lines.map(_.length).maxOption.getOrElse(30)
    val height = lines.length + 1

    lines.zipWithIndex.foldLeft(ScreenBuffer.empty(width, height)) { case (buf, (line, y)) =>
      buf.drawText(0, y, line)
    }

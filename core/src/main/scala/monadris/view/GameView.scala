package monadris.view

import scala.util.chaining.*

import monadris.domain.*
import monadris.domain.config.AppConfig

object GameView:

  private object Layout:
    val BorderThickness      = 1
    val HorizontalBorder     = BorderThickness * 2
    val VerticalBorder       = BorderThickness * 2
    val InfoPanelWidth       = 30
    val InfoPanelLeftPadding = HorizontalBorder + 2
    val ControlsAreaHeight   = 4
    val DefaultTitleWidth    = 40
    val DefaultGameOverWidth = 30

  private val FilledBlock = '█'
  private val LockedBlock = '▓'
  private val EmptyCell   = '·'

  def shapeToColor(shape: TetrominoShape): UiColor = shape match
    case TetrominoShape.I => UiColor.Cyan
    case TetrominoShape.O => UiColor.Yellow
    case TetrominoShape.T => UiColor.Magenta
    case TetrominoShape.S => UiColor.Green
    case TetrominoShape.Z => UiColor.Red
    case TetrominoShape.J => UiColor.Blue
    case TetrominoShape.L => UiColor.White

  def toScreenBuffer(state: GameState, config: AppConfig): ScreenBuffer =
    val gridWidth  = config.grid.width
    val gridHeight = config.grid.height

    // グリッド + 枠線 + 情報欄用の幅と高さを計算
    val totalWidth  = gridWidth + Layout.HorizontalBorder + Layout.InfoPanelWidth
    val totalHeight = gridHeight + Layout.VerticalBorder + Layout.ControlsAreaHeight

    ScreenBuffer
      .empty(totalWidth, totalHeight)
      .pipe(renderGrid(_, state, gridWidth, gridHeight))
      .pipe(renderInfo(_, state, gridWidth + Layout.InfoPanelLeftPadding))
      .pipe(renderControls(_, gridHeight + Layout.VerticalBorder))

  private def renderGrid(
    buffer: ScreenBuffer,
    state: GameState,
    gridWidth: Int,
    gridHeight: Int
  ): ScreenBuffer =
    val grid          = state.grid
    val fallingBlocks = state.currentTetromino.currentBlocks.toSet
    val fallingColor  = shapeToColor(state.currentTetromino.shape)

    val topBorder     = "┌" + "─" * gridWidth + "┐"
    val withTopBorder = buffer.drawText(0, 0, topBorder)

    val withGrid = (0 until gridHeight).foldLeft(withTopBorder) { (buf, y) =>
      val rowPixels = (0 until gridWidth).map { x =>
        val pos = Position(x, y)
        if fallingBlocks.contains(pos) then Pixel(FilledBlock, fallingColor)
        else
          grid.get(pos) match
            case Some(Cell.Filled(shape)) => Pixel(LockedBlock, shapeToColor(shape))
            case _                        => Pixel(EmptyCell, UiColor.Default)
      }.toVector

      buf
        .drawChar(0, y + 1, '│')
        .drawPixels(1, y + 1, rowPixels)
        .drawChar(gridWidth + 1, y + 1, '│')
    }

    val bottomBorder = "└" + "─" * gridWidth + "┘"
    withGrid.drawText(0, gridHeight + 1, bottomBorder)

  private def renderInfo(buffer: ScreenBuffer, state: GameState, startX: Int): ScreenBuffer =
    buffer
      .drawText(startX, 1, s"Score: ${state.score}")
      .drawText(startX, 2, s"Level: ${state.level}")
      .drawText(startX, 3, s"Lines: ${state.linesCleared}")
      .drawText(startX, 5, s"Next: ${state.nextTetromino}")
      .drawText(startX, 7, if state.status == GameStatus.Paused then "** PAUSED **" else "")

  private def renderControls(buffer: ScreenBuffer, startY: Int): ScreenBuffer =
    buffer
      .drawText(0, startY, "H/L or ←/→: Move  K or ↑: Rotate")
      .drawText(0, startY + 1, "J or ↓: Drop  Space: Hard drop")
      .drawText(0, startY + 2, "P: Pause  Q: Quit")

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
    val width  = lines.map(_.length).maxOption.getOrElse(Layout.DefaultTitleWidth)
    val height = lines.length + 1

    lines.zipWithIndex.foldLeft(ScreenBuffer.empty(width, height)) { case (buf, (line, y)) =>
      buf.drawText(0, y, line)
    }

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
    val width  = lines.map(_.length).maxOption.getOrElse(Layout.DefaultGameOverWidth)
    val height = lines.length + 1

    lines.zipWithIndex.foldLeft(ScreenBuffer.empty(width, height)) { case (buf, (line, y)) =>
      buf.drawText(0, y, line)
    }

  final case class MenuItem(key: Char, label: String)

  object MenuItems:
    val PlayGame: MenuItem      = MenuItem('1', "Play Game")
    val PlayAndRecord: MenuItem = MenuItem('2', "Play & Record")
    val WatchReplay: MenuItem   = MenuItem('3', "Watch Replay")
    val ListReplays: MenuItem   = MenuItem('4', "List Replays")
    val Quit: MenuItem          = MenuItem('Q', "Exit")

    val all: Vector[MenuItem] = Vector(PlayGame, PlayAndRecord, WatchReplay, ListReplays, Quit)

  def menuScreen(selectedIndex: Int): ScreenBuffer =
    val menuWidth = 70

    def menuItem(item: MenuItem, index: Int): String =
      val isSelected = index == selectedIndex
      val prefix     = if isSelected then "▶ " else "  "
      val suffix     = if isSelected then " ◀" else "  "
      val content    = s"$prefix[${item.key}] ${item.label}$suffix"
      val padding    = menuWidth - 2 - content.length
      s"║$content${" " * padding}║"

    val lines = List(
      "╔════════════════════════════════════════════════════════════════════╗",
      "║                                                                    ║",
      "║  ███╗   ███╗ ██████╗ ███╗   ██╗ █████╗ ██████╗ ██████╗ ██╗███████╗ ║",
      "║  ████╗ ████║██╔═══██╗████╗  ██║██╔══██╗██╔══██╗██╔══██╗██║██╔════╝ ║",
      "║  ██╔████╔██║██║   ██║██╔██╗ ██║███████║██║  ██║██████╔╝██║███████╗ ║",
      "║  ██║╚██╔╝██║██║   ██║██║╚██╗██║██╔══██║██║  ██║██╔══██╗██║╚════██║ ║",
      "║  ██║ ╚═╝ ██║╚██████╔╝██║ ╚████║██║  ██║██████╔╝██║  ██║██║███████║ ║",
      "║  ╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚═╝  ╚═╝╚═════╝ ╚═╝  ╚═╝╚═╝╚══════╝ ║",
      "║                                                                    ║",
      "║                      Functional Tetris in Scala                    ║",
      "║                                                                    ║",
      "╠════════════════════════════════════════════════════════════════════╣",
      "║                                                                    ║"
    ) ++ MenuItems.all.zipWithIndex.map { case (item, idx) =>
      menuItem(item, idx)
    } ++ List(
      "║                                                                    ║",
      "╠════════════════════════════════════════════════════════════════════╣",
      "║            ↑/↓ or K/J: Navigate    Enter: Select    Q: Quit        ║",
      "╚════════════════════════════════════════════════════════════════════╝"
    )

    val width  = lines.map(_.length).maxOption.getOrElse(menuWidth)
    val height = lines.length + 1

    lines.zipWithIndex.foldLeft(ScreenBuffer.empty(width, height)) { case (buf, (line, y)) =>
      val color = if line.contains("▶") then UiColor.Cyan else UiColor.Default
      buf.drawText(0, y, line, color)
    }

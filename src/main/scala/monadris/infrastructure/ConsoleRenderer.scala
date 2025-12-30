package monadris.infrastructure

import zio.*

import monadris.view.{ScreenBuffer, Pixel, UiColor}

/**
 * ScreenBuffer をコンソールに出力するレンダラー
 * UiColor を ANSI エスケープシーケンスに変換
 */
object ConsoleRenderer:

  // ANSI color codes
  private val ANSI_RESET = "\u001b[0m"
  private val ANSI_CYAN = "\u001b[36m"
  private val ANSI_YELLOW = "\u001b[33m"
  private val ANSI_MAGENTA = "\u001b[35m"
  private val ANSI_GREEN = "\u001b[32m"
  private val ANSI_RED = "\u001b[31m"
  private val ANSI_BLUE = "\u001b[34m"
  private val ANSI_WHITE = "\u001b[37m"

  // raw modeでは \r\n が必要
  private val NL = "\r\n"

  /**
   * UiColor を ANSI カラーコードに変換
   */
  private def colorToAnsi(color: UiColor): String = color match
    case UiColor.Cyan    => ANSI_CYAN
    case UiColor.Yellow  => ANSI_YELLOW
    case UiColor.Magenta => ANSI_MAGENTA
    case UiColor.Green   => ANSI_GREEN
    case UiColor.Red     => ANSI_RED
    case UiColor.Blue    => ANSI_BLUE
    case UiColor.White   => ANSI_WHITE
    case UiColor.Default => ANSI_RESET

  /**
   * ピクセルを ANSI 文字列に変換
   */
  private def pixelToString(pixel: Pixel): String =
    if pixel.color == UiColor.Default then
      pixel.char.toString
    else
      s"${colorToAnsi(pixel.color)}${pixel.char}$ANSI_RESET"

  /**
   * 行を ANSI 文字列に変換（色の切り替えを最適化）
   */
  private def rowToString(row: Vector[Pixel]): String =
    val (result, lastColor) = row.foldLeft((new StringBuilder, UiColor.Default)) {
      case ((sb, currentColor), pixel) =>
        if pixel.color != currentColor then
          sb.append(colorToAnsi(pixel.color))
        sb.append(pixel.char)
        (sb, pixel.color)
    }

    if lastColor != UiColor.Default then
      result.append(ANSI_RESET)

    result.toString

  /**
   * ScreenBuffer 全体を文字列に変換
   */
  def bufferToString(buffer: ScreenBuffer): String =
    buffer.pixels.map(rowToString).mkString(NL)

  /**
   * 画面クリア + バッファ描画
   */
  def render(buffer: ScreenBuffer): ZIO[ConsoleService, Throwable, Unit] =
    for
      _ <- ConsoleService.print("\u001b[H\u001b[2J\u001b[3J")
      _ <- ConsoleService.print(bufferToString(buffer))
      _ <- ConsoleService.print(NL)
      _ <- ConsoleService.flush()
    yield ()

  /**
   * バッファのみ描画（画面クリアなし）
   */
  def renderWithoutClear(buffer: ScreenBuffer): ZIO[ConsoleService, Throwable, Unit] =
    for
      _ <- ConsoleService.print(bufferToString(buffer))
      _ <- ConsoleService.print(NL)
      _ <- ConsoleService.flush()
    yield ()

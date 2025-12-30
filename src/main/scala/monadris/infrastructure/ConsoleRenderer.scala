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
   * 差分描画対応のrender
   * previousがNoneの場合は全描画、Someの場合は差分のみ描画
   */
  def render(current: ScreenBuffer, previous: Option[ScreenBuffer]): ZIO[ConsoleService, Throwable, Unit] =
    previous match
      case None => render(current)
      case Some(prev) => renderDiff(current, prev)

  /**
   * 2つのバッファの差分を計算し、変更点のみを描画
   */
  private def renderDiff(current: ScreenBuffer, previous: ScreenBuffer): ZIO[ConsoleService, Throwable, Unit] =
    val diffString = computeDiffString(current, previous)
    if diffString.isEmpty then
      ZIO.unit
    else
      for
        _ <- ConsoleService.print(diffString)
        _ <- ConsoleService.flush()
      yield ()

  /**
   * 2つのバッファの差分を ANSI エスケープシーケンス付き文字列に変換
   * 変更があった座標のみカーソル移動 + 描画
   */
  private def computeDiffString(current: ScreenBuffer, previous: ScreenBuffer): String =
    val coordinates = for
      y <- 0 until current.height
      x <- 0 until current.width
    yield (x, y)

    val (sb, lastColor) = coordinates.foldLeft((new StringBuilder, UiColor.Default)) {
      case ((accSb, accColor), (x, y)) =>
        val currentPixel = current.pixels(y)(x)
        val prevPixel =
          if y < previous.height && x < previous.width then
            previous.pixels(y)(x)
          else
            Pixel(' ', UiColor.Default)

        if currentPixel != prevPixel then
          // カーソル移動（1-based座標）
          accSb.append(s"\u001b[${y + 1};${x + 1}H")

          // 色の変更が必要な場合のみ色コードを出力
          val newColor =
            if currentPixel.color != accColor then
              accSb.append(colorToAnsi(currentPixel.color))
              currentPixel.color
            else
              accColor

          accSb.append(currentPixel.char)
          (accSb, newColor)
        else
          (accSb, accColor)
    }

    // 最後に色をリセット
    if lastColor != UiColor.Default then
      sb.append(ANSI_RESET)

    sb.toString

  /**
   * バッファのみ描画（画面クリアなし）
   */
  def renderWithoutClear(buffer: ScreenBuffer): ZIO[ConsoleService, Throwable, Unit] =
    for
      _ <- ConsoleService.print(bufferToString(buffer))
      _ <- ConsoleService.print(NL)
      _ <- ConsoleService.flush()
    yield ()

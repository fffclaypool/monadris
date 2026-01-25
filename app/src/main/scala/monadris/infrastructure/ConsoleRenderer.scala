package monadris.infrastructure

import zio.*

import monadris.view.Pixel
import monadris.view.ScreenBuffer
import monadris.view.UiColor

object ConsoleRenderer:

  private val ANSI_RESET   = "\u001b[0m"
  private val ANSI_CYAN    = "\u001b[36m"
  private val ANSI_YELLOW  = "\u001b[33m"
  private val ANSI_MAGENTA = "\u001b[35m"
  private val ANSI_GREEN   = "\u001b[32m"
  private val ANSI_RED     = "\u001b[31m"
  private val ANSI_BLUE    = "\u001b[34m"
  private val ANSI_WHITE   = "\u001b[37m"

  private val HIDE_CURSOR  = "\u001b[?25l"
  private val SHOW_CURSOR  = "\u001b[?25h"
  private val HOME         = "\u001b[H"
  private val CLEAR_SCREEN = "\u001b[2J\u001b[3J"
  private val NL           = "\r\n"

  private def colorToAnsi(color: UiColor): String = color match
    case UiColor.Cyan    => ANSI_CYAN
    case UiColor.Yellow  => ANSI_YELLOW
    case UiColor.Magenta => ANSI_MAGENTA
    case UiColor.Green   => ANSI_GREEN
    case UiColor.Red     => ANSI_RED
    case UiColor.Blue    => ANSI_BLUE
    case UiColor.White   => ANSI_WHITE
    case UiColor.Default => ANSI_RESET

  private def rowToString(row: Vector[Pixel]): String =
    val (result, lastColor) = row.foldLeft((new StringBuilder, UiColor.Default)) { case ((sb, currentColor), pixel) =>
      if pixel.color != currentColor then sb.append(colorToAnsi(pixel.color))
      sb.append(pixel.char)
      (sb, pixel.color)
    }
    if lastColor != UiColor.Default then result.append(ANSI_RESET)
    result.toString

  def bufferToString(buffer: ScreenBuffer): String =
    buffer.pixels.map(rowToString).mkString(NL)

  def render(buffer: ScreenBuffer): ZIO[ConsoleService, Throwable, Unit] =
    for
      _ <- ConsoleService.print(s"$HIDE_CURSOR$HOME$CLEAR_SCREEN")
      _ <- ConsoleService.print(bufferToString(buffer))
      _ <- ConsoleService.print(NL)
      _ <- ConsoleService.flush()
    yield ()

  def render(current: ScreenBuffer, previous: Option[ScreenBuffer]): ZIO[ConsoleService, Throwable, Unit] =
    previous match
      case None       => render(current)
      case Some(prev) => renderDiff(current, prev)

  private def renderDiff(current: ScreenBuffer, previous: ScreenBuffer): ZIO[ConsoleService, Throwable, Unit] =
    val diffString = computeDiffString(current, previous)
    if diffString.isEmpty then ZIO.unit
    else
      for
        _ <- ConsoleService.print(HIDE_CURSOR + diffString)
        _ <- ConsoleService.flush()
      yield ()

  private def computeDiffString(current: ScreenBuffer, previous: ScreenBuffer): String =
    val coordinates = for
      y <- 0 until current.height
      x <- 0 until current.width
    yield (x, y)

    val (sb, lastColor) = coordinates.foldLeft((new StringBuilder, UiColor.Default)) {
      case ((accSb, accColor), (x, y)) =>
        val currentPixel = current.pixels(y)(x)
        val prevPixel    =
          if y < previous.height && x < previous.width then previous.pixels(y)(x)
          else Pixel(' ', UiColor.Default)

        if currentPixel != prevPixel then
          // カーソル移動（1-based座標）
          accSb.append(s"\u001b[${y + 1};${x + 1}H")

          val newColor =
            if currentPixel.color != accColor then
              accSb.append(colorToAnsi(currentPixel.color))
              currentPixel.color
            else accColor

          accSb.append(currentPixel.char)
          (accSb, newColor)
        else (accSb, accColor)
    }

    if lastColor != UiColor.Default then sb.append(ANSI_RESET)

    sb.toString

  def renderWithoutClear(buffer: ScreenBuffer): ZIO[ConsoleService, Throwable, Unit] =
    for
      _ <- ConsoleService.print(HIDE_CURSOR)
      _ <- ConsoleService.print(bufferToString(buffer))
      _ <- ConsoleService.print(NL)
      _ <- ConsoleService.flush()
    yield ()

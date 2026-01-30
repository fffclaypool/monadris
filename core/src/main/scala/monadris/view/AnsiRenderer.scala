package monadris.view

object AnsiRenderer:

  object Codes:
    val Reset: String   = "\u001b[0m"
    val Cyan: String    = "\u001b[36m"
    val Yellow: String  = "\u001b[33m"
    val Magenta: String = "\u001b[35m"
    val Green: String   = "\u001b[32m"
    val Red: String     = "\u001b[31m"
    val Blue: String    = "\u001b[34m"
    val White: String   = "\u001b[37m"

    val HideCursor: String  = "\u001b[?25l"
    val ShowCursor: String  = "\u001b[?25h"
    val Home: String        = "\u001b[H"
    val ClearScreen: String = "\u001b[2J\u001b[3J"
    val ClearLine: String   = "\u001b[2K"
    val Newline: String     = "\r\n"

    def moveTo(row: Int, col: Int): String = s"\u001b[${row};${col}H"

  def colorToAnsi(color: UiColor): String = color match
    case UiColor.Cyan    => Codes.Cyan
    case UiColor.Yellow  => Codes.Yellow
    case UiColor.Magenta => Codes.Magenta
    case UiColor.Green   => Codes.Green
    case UiColor.Red     => Codes.Red
    case UiColor.Blue    => Codes.Blue
    case UiColor.White   => Codes.White
    case UiColor.Default => Codes.Reset

  def rowToString(row: Vector[Pixel]): String =
    val (result, lastColor) = row.foldLeft((new StringBuilder, UiColor.Default)) { case ((sb, currentColor), pixel) =>
      if pixel.color != currentColor then sb.append(colorToAnsi(pixel.color))
      sb.append(pixel.char)
      (sb, pixel.color)
    }
    if lastColor != UiColor.Default then result.append(Codes.Reset)
    result.toString

  def bufferToString(buffer: ScreenBuffer): String =
    buffer.pixels.map(rowToString).mkString(Codes.Newline)

  def computeDiffString(current: ScreenBuffer, previous: ScreenBuffer): String =
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
          accSb.append(Codes.moveTo(y + 1, x + 1))

          val newColor =
            if currentPixel.color != accColor then
              accSb.append(colorToAnsi(currentPixel.color))
              currentPixel.color
            else accColor

          accSb.append(currentPixel.char)
          (accSb, newColor)
        else (accSb, accColor)
    }

    if lastColor != UiColor.Default then sb.append(Codes.Reset)

    sb.toString

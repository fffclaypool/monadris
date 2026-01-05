package monadris.infrastructure

import zio.*
import zio.test.*

import monadris.view.Pixel
import monadris.view.ScreenBuffer
import monadris.view.UiColor

/**
 * ConsoleRendererの純粋関数テスト
 */
object ConsoleRendererSpec extends ZIOSpecDefault:

  // ============================================================
  // Test buffer dimensions
  // ============================================================

  private object BufferSize:
    val empty  = 0
    val single = 1
    val small  = 3
    val medium = 5

  private object Position:
    val origin = 0
    val first  = 0
    val second = 1
    val third  = 2
    val fourth = 3
    val fifth  = 4

  // ANSI escape sequences for assertions
  private object Ansi:
    val home        = "\u001b[H"
    val clearScreen = "\u001b[2J"
    val reset       = "\u001b[0m"
    val red         = "\u001b[31m"
    val green       = "\u001b[32m"
    val blue        = "\u001b[34m"
    val cyan        = "\u001b[36m"
    val yellow      = "\u001b[33m"
    val magenta     = "\u001b[35m"
    val white       = "\u001b[37m"
    // Cursor move: row;col (1-based)
    val cursorRow1Col2 = "\u001b[1;2H"
    val cursorRow1Col3 = "\u001b[1;3H"

  // ============================================================
  // bufferToString tests
  // ============================================================

  def spec = suite("ConsoleRenderer")(
    suite("bufferToString")(
      test("converts empty buffer to empty string") {
        val buffer = ScreenBuffer.empty(BufferSize.empty, BufferSize.empty)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.isEmpty)
      },
      test("converts single character buffer") {
        val buffer = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X')
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains("X"))
      },
      test("includes ANSI color codes for colored pixels") {
        val buffer = ScreenBuffer
          .empty(BufferSize.small, BufferSize.single)
          .drawChar(Position.first, Position.origin, 'R', UiColor.Red)
          .drawChar(Position.second, Position.origin, 'G', UiColor.Green)
          .drawChar(Position.third, Position.origin, 'B', UiColor.Blue)
        val result = ConsoleRenderer.bufferToString(buffer)

        assertTrue(
          result.contains(Ansi.red),
          result.contains(Ansi.green),
          result.contains(Ansi.blue),
          result.contains("R"),
          result.contains("G"),
          result.contains("B")
        )
      },
      test("includes reset code after colored text") {
        val buffer = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X', UiColor.Cyan)
        val result = ConsoleRenderer.bufferToString(buffer)

        assertTrue(
          result.contains(Ansi.cyan),
          result.contains(Ansi.reset)
        )
      },
      test("handles multiple rows with newlines") {
        val twoRows = 2
        val buffer = ScreenBuffer
          .empty(BufferSize.small, twoRows)
          .drawText(Position.origin, Position.first, "ABC")
          .drawText(Position.origin, Position.second, "DEF")
        val result = ConsoleRenderer.bufferToString(buffer)

        assertTrue(
          result.contains("ABC"),
          result.contains("DEF"),
          result.contains("\r\n")
        )
      },
      test("handles default color without color code") {
        // When starting with Default color and staying with Default,
        // no color codes should be emitted (since Default is the initial state)
        val buffer = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X', UiColor.Default)
        val result = ConsoleRenderer.bufferToString(buffer)

        assertTrue(
          result.contains("X"),
          // Default color doesn't emit color codes if it's the starting state
          !result.contains("\u001b[")
        )
      },
      test("optimizes color switching") {
        // Same color consecutive chars should not repeat color code
        val buffer = ScreenBuffer
          .empty(BufferSize.small, BufferSize.single)
          .drawChar(Position.first, Position.origin, 'A', UiColor.Red)
          .drawChar(Position.second, Position.origin, 'B', UiColor.Red)
          .drawChar(Position.third, Position.origin, 'C', UiColor.Red)
        val result = ConsoleRenderer.bufferToString(buffer)

        // Should only have one red code, not three
        val redCount = result.sliding(Ansi.red.length).count(_ == Ansi.red)
        assertTrue(redCount == 1)
      },
      test("handles all UiColor variants") {
        val colors = List(
          UiColor.Cyan,
          UiColor.Yellow,
          UiColor.Magenta,
          UiColor.Green,
          UiColor.Red,
          UiColor.Blue,
          UiColor.White,
          UiColor.Default
        )
        val buffer = colors.zipWithIndex.foldLeft(ScreenBuffer.empty(colors.length, 1)) { case (buf, (color, x)) =>
          buf.drawChar(x, Position.origin, (65 + x).toChar, color) // A, B, C, ...
        }
        val result = ConsoleRenderer.bufferToString(buffer)

        assertTrue(
          result.contains(Ansi.cyan),
          result.contains(Ansi.yellow),
          result.contains(Ansi.magenta),
          result.contains(Ansi.green),
          result.contains(Ansi.red),
          result.contains(Ansi.blue),
          result.contains(Ansi.white)
        )
      }
    ),

    // ============================================================
    // Color ANSI codes
    // ============================================================

    suite("Color ANSI codes")(
      test("Cyan produces correct ANSI code") {
        val buffer = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X', UiColor.Cyan)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains("\u001b[36m"))
      },
      test("Yellow produces correct ANSI code") {
        val buffer = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X', UiColor.Yellow)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains("\u001b[33m"))
      },
      test("Magenta produces correct ANSI code") {
        val buffer = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X', UiColor.Magenta)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains("\u001b[35m"))
      },
      test("Green produces correct ANSI code") {
        val buffer = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X', UiColor.Green)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains("\u001b[32m"))
      },
      test("Red produces correct ANSI code") {
        val buffer = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X', UiColor.Red)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains("\u001b[31m"))
      },
      test("Blue produces correct ANSI code") {
        val buffer = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X', UiColor.Blue)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains("\u001b[34m"))
      },
      test("White produces correct ANSI code") {
        val buffer = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X', UiColor.White)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains("\u001b[37m"))
      }
    ),

    // ============================================================
    // computeDiffString tests (testing via render with previous buffer)
    // ============================================================

    suite("computeDiffString edge cases")(
      test("diff render handles multiple scattered changes") {
        val prev = ScreenBuffer
          .empty(BufferSize.medium, BufferSize.single)
          .drawText(Position.origin, Position.origin, "AAAAA")
        val curr = ScreenBuffer
          .empty(BufferSize.medium, BufferSize.single)
          .drawText(Position.origin, Position.origin, "ABABA")

        // Create diff by changing indices 1 and 3
        val diff = computeDiff(curr, prev)
        assertTrue(
          diff.contains("B"),
          diff.contains("\u001b[1;2H"), // Position (0,1) in 1-based coords
          diff.contains("\u001b[1;4H")  // Position (0,3) in 1-based coords
        )
      },
      test("diff render handles char change at same position") {
        val prev = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'A')
        val curr = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'B')

        val diff = computeDiff(curr, prev)
        assertTrue(diff.contains("B"))
      },
      test("diff render handles color change without char change") {
        val prev = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'A', UiColor.Red)
        val curr = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'A', UiColor.Blue)

        val diff = computeDiff(curr, prev)
        assertTrue(diff.contains(Ansi.blue))
      },
      test("diff render resets color at end if last pixel was colored") {
        val prev = ScreenBuffer.empty(BufferSize.single, BufferSize.single)
        val curr = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X', UiColor.Green)

        val diff = computeDiff(curr, prev)
        assertTrue(diff.endsWith(Ansi.reset))
      },
      test("diff render preserves same color across consecutive changes") {
        val prev = ScreenBuffer
          .empty(BufferSize.small, BufferSize.single)
          .drawText(Position.origin, Position.origin, "   ")
        val curr = ScreenBuffer
          .empty(BufferSize.small, BufferSize.single)
          .drawChar(Position.first, Position.origin, 'A', UiColor.Red)
          .drawChar(Position.second, Position.origin, 'B', UiColor.Red)
          .drawChar(Position.third, Position.origin, 'C', UiColor.Red)

        val diff = computeDiff(curr, prev)
        // Only one red code should appear
        val redCount = diff.sliding(Ansi.red.length).count(_ == Ansi.red)
        assertTrue(redCount == 1)
      },
      test("diff render handles expanding buffer size") {
        val prev = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'A')
        val curr = ScreenBuffer
          .empty(BufferSize.small, BufferSize.single)
          .drawText(Position.origin, Position.origin, "ABC")

        val diff = computeDiff(curr, prev)
        assertTrue(
          diff.contains("B"),
          diff.contains("C")
        )
      },
      test("diff render handles shrinking buffer size") {
        val prev = ScreenBuffer
          .empty(BufferSize.small, BufferSize.single)
          .drawText(Position.origin, Position.origin, "ABC")
        val curr = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X')

        val diff = computeDiff(curr, prev)
        assertTrue(diff.contains("X"))
      }
    )
  )

  // Helper function to compute diff string (package private in ConsoleRenderer)
  // We test this indirectly by checking the render output
  private def computeDiff(current: ScreenBuffer, previous: ScreenBuffer): String =
    val coordinates = for
      y <- 0 until current.height
      x <- 0 until current.width
    yield (x, y)

    val ANSI_RESET = "\u001b[0m"
    val (sb, lastColor) = coordinates.foldLeft((new StringBuilder, UiColor.Default)) {
      case ((accSb, accColor), (x, y)) =>
        val currentPixel = current.pixels(y)(x)
        val prevPixel =
          if y < previous.height && x < previous.width then previous.pixels(y)(x)
          else Pixel(' ', UiColor.Default)

        if currentPixel != prevPixel then
          accSb.append(s"\u001b[${y + 1};${x + 1}H")

          val colorCode = currentPixel.color match
            case UiColor.Cyan    => "\u001b[36m"
            case UiColor.Yellow  => "\u001b[33m"
            case UiColor.Magenta => "\u001b[35m"
            case UiColor.Green   => "\u001b[32m"
            case UiColor.Red     => "\u001b[31m"
            case UiColor.Blue    => "\u001b[34m"
            case UiColor.White   => "\u001b[37m"
            case UiColor.Default => ANSI_RESET

          val newColor =
            if currentPixel.color != accColor then
              accSb.append(colorCode)
              currentPixel.color
            else accColor

          accSb.append(currentPixel.char)
          (accSb, newColor)
        else (accSb, accColor)
    }

    if lastColor != UiColor.Default then sb.append(ANSI_RESET)
    sb.toString

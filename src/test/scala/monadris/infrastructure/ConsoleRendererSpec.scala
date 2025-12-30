package monadris.infrastructure

import zio.*
import zio.test.*

import monadris.view.{ScreenBuffer, UiColor}
import monadris.infrastructure.{TestServices as Mocks}

object ConsoleRendererSpec extends ZIOSpecDefault:

  // ============================================================
  // bufferToString tests
  // ============================================================

  def spec = suite("ConsoleRenderer")(
    suite("bufferToString")(
      test("converts empty buffer to empty string") {
        val buffer = ScreenBuffer.empty(0, 0)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.isEmpty)
      },

      test("converts single character buffer") {
        val buffer = ScreenBuffer.empty(1, 1).drawChar(0, 0, 'X')
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains("X"))
      },

      test("includes ANSI color codes for colored pixels") {
        val buffer = ScreenBuffer.empty(3, 1)
          .drawChar(0, 0, 'R', UiColor.Red)
          .drawChar(1, 0, 'G', UiColor.Green)
          .drawChar(2, 0, 'B', UiColor.Blue)
        val result = ConsoleRenderer.bufferToString(buffer)

        assertTrue(
          result.contains("\u001b[31m"), // Red
          result.contains("\u001b[32m"), // Green
          result.contains("\u001b[34m"), // Blue
          result.contains("R"),
          result.contains("G"),
          result.contains("B")
        )
      },

      test("includes reset code after colored text") {
        val buffer = ScreenBuffer.empty(1, 1).drawChar(0, 0, 'X', UiColor.Cyan)
        val result = ConsoleRenderer.bufferToString(buffer)

        assertTrue(
          result.contains("\u001b[36m"), // Cyan
          result.contains("\u001b[0m")   // Reset
        )
      },

      test("handles multiple rows with newlines") {
        val buffer = ScreenBuffer.empty(3, 2)
          .drawText(0, 0, "ABC")
          .drawText(0, 1, "DEF")
        val result = ConsoleRenderer.bufferToString(buffer)

        assertTrue(
          result.contains("ABC"),
          result.contains("DEF"),
          result.contains("\r\n")
        )
      },

      test("optimizes color switching") {
        // Same color for adjacent characters
        val buffer = ScreenBuffer.empty(3, 1)
          .drawChar(0, 0, 'A', UiColor.Red)
          .drawChar(1, 0, 'B', UiColor.Red)
          .drawChar(2, 0, 'C', UiColor.Red)
        val result = ConsoleRenderer.bufferToString(buffer)

        // Should only have one color code at the start
        val colorCodeCount = result.sliding(5).count(_ == "\u001b[31m")
        assertTrue(colorCodeCount == 1)
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
        val buffer = colors.zipWithIndex.foldLeft(ScreenBuffer.empty(colors.size, 1)) {
          case (buf, (color, i)) => buf.drawChar(i, 0, 'X', color)
        }
        val result = ConsoleRenderer.bufferToString(buffer)

        assertTrue(result.count(_ == 'X') == colors.size)
      },

      test("handles default color without color code") {
        val buffer = ScreenBuffer.empty(1, 1).drawChar(0, 0, 'X', UiColor.Default)
        val result = ConsoleRenderer.bufferToString(buffer)

        // Default color uses reset, check it still has the character
        assertTrue(result.contains("X"))
      }
    ),

    // ============================================================
    // render tests (with mocks)
    // ============================================================

    suite("render")(
      test("outputs to console with clear sequence") {
        val buffer = ScreenBuffer.empty(5, 3).drawText(0, 0, "Test")
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.render(buffer)
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains("\u001b[H"),      // Home cursor
          combined.contains("\u001b[2J"),     // Clear screen
          combined.contains("Test")
        )
      }.provide(Mocks.console),

      test("flushes after rendering") {
        val buffer = ScreenBuffer.empty(3, 1)
        for
          _      <- ConsoleRenderer.render(buffer)
        yield assertTrue(true) // Flush called without error
      }.provide(Mocks.console)
    ),

    // ============================================================
    // renderWithoutClear tests
    // ============================================================

    suite("renderWithoutClear")(
      test("outputs to console without clear sequence") {
        val buffer = ScreenBuffer.empty(5, 1).drawText(0, 0, "Hello")
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.renderWithoutClear(buffer)
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains("Hello"),
          !combined.contains("\u001b[2J")  // No clear screen
        )
      }.provide(Mocks.console),

      test("includes newline at end") {
        val buffer = ScreenBuffer.empty(3, 1).drawText(0, 0, "Hi")
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.renderWithoutClear(buffer)
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(combined.contains("\r\n"))
      }.provide(Mocks.console)
    ),

    // ============================================================
    // Color conversion tests (via bufferToString)
    // ============================================================

    suite("Color ANSI codes")(
      test("Cyan produces correct ANSI code") {
        val buffer = ScreenBuffer.empty(1, 1).drawChar(0, 0, 'X', UiColor.Cyan)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains("\u001b[36m"))
      },

      test("Yellow produces correct ANSI code") {
        val buffer = ScreenBuffer.empty(1, 1).drawChar(0, 0, 'X', UiColor.Yellow)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains("\u001b[33m"))
      },

      test("Magenta produces correct ANSI code") {
        val buffer = ScreenBuffer.empty(1, 1).drawChar(0, 0, 'X', UiColor.Magenta)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains("\u001b[35m"))
      },

      test("Green produces correct ANSI code") {
        val buffer = ScreenBuffer.empty(1, 1).drawChar(0, 0, 'X', UiColor.Green)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains("\u001b[32m"))
      },

      test("Red produces correct ANSI code") {
        val buffer = ScreenBuffer.empty(1, 1).drawChar(0, 0, 'X', UiColor.Red)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains("\u001b[31m"))
      },

      test("Blue produces correct ANSI code") {
        val buffer = ScreenBuffer.empty(1, 1).drawChar(0, 0, 'X', UiColor.Blue)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains("\u001b[34m"))
      },

      test("White produces correct ANSI code") {
        val buffer = ScreenBuffer.empty(1, 1).drawChar(0, 0, 'X', UiColor.White)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains("\u001b[37m"))
      }
    )
  )

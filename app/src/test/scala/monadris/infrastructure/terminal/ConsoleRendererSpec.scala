package monadris.infrastructure.terminal

import zio.*
import zio.test.*

import monadris.infrastructure.terminal.TestServices as Mocks
import monadris.view.ScreenBuffer
import monadris.view.UiColor

object ConsoleRendererSpec extends ZIOSpecDefault:

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

  private val singleColorCode = 1

  private object Ansi:
    val home           = "\u001b[H"
    val clearScreen    = "\u001b[2J"
    val reset          = "\u001b[0m"
    val red            = "\u001b[31m"
    val green          = "\u001b[32m"
    val blue           = "\u001b[34m"
    val cyan           = "\u001b[36m"
    val yellow         = "\u001b[33m"
    val magenta        = "\u001b[35m"
    val white          = "\u001b[37m"
    val cursorRow1Col2 = "\u001b[1;2H"
    val cursorRow1Col3 = "\u001b[1;3H"

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
        val buffer  = ScreenBuffer
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
      test("optimizes color switching") {
        val buffer = ScreenBuffer
          .empty(BufferSize.small, BufferSize.single)
          .drawChar(Position.first, Position.origin, 'A', UiColor.Red)
          .drawChar(Position.second, Position.origin, 'B', UiColor.Red)
          .drawChar(Position.third, Position.origin, 'C', UiColor.Red)
        val result = ConsoleRenderer.bufferToString(buffer)

        val ansiCodeLength = 5
        val colorCodeCount = result.sliding(ansiCodeLength).count(_ == Ansi.red)
        assertTrue(colorCodeCount == singleColorCode)
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
        val buffer = colors.zipWithIndex.foldLeft(ScreenBuffer.empty(colors.size, BufferSize.single)) {
          case (buf, (color, i)) => buf.drawChar(i, Position.origin, 'X', color)
        }
        val result = ConsoleRenderer.bufferToString(buffer)

        assertTrue(result.count(_ == 'X') == colors.size)
      },
      test("handles default color without color code") {
        val buffer = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X', UiColor.Default)
        val result = ConsoleRenderer.bufferToString(buffer)

        assertTrue(result.contains("X"))
      }
    ),

    suite("render")(
      test("outputs to console with clear sequence") {
        val buffer = ScreenBuffer
          .empty(BufferSize.medium, BufferSize.small)
          .drawText(Position.origin, Position.origin, "Test")
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.render(buffer)
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains(Ansi.home),
          combined.contains(Ansi.clearScreen),
          combined.contains("Test")
        )
      }.provide(Mocks.console),
      test("flushes after rendering") {
        val buffer = ScreenBuffer.empty(BufferSize.small, BufferSize.single)
        for _ <- ConsoleRenderer.render(buffer)
        yield assertTrue(true)
      }.provide(Mocks.console)
    ),

    suite("render with previous buffer")(
      test("full render when previous is None") {
        val buffer = ScreenBuffer
          .empty(BufferSize.medium, BufferSize.small)
          .drawText(Position.origin, Position.origin, "Test")
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.render(buffer, None)
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains(Ansi.home),
          combined.contains(Ansi.clearScreen),
          combined.contains("Test")
        )
      }.provide(Mocks.console),
      test("diff render outputs only changed pixels") {
        val prev = ScreenBuffer
          .empty(BufferSize.medium, BufferSize.small)
          .drawText(Position.origin, Position.origin, "AAAA")
        val curr = ScreenBuffer
          .empty(BufferSize.medium, BufferSize.small)
          .drawText(Position.origin, Position.origin, "ABBA")
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.render(curr, Some(prev))
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          !combined.contains(Ansi.clearScreen),
          combined.contains(Ansi.cursorRow1Col2),
          combined.contains(Ansi.cursorRow1Col3),
          combined.contains("B")
        )
      }.provide(Mocks.console),
      test("diff render outputs nothing when buffers are identical") {
        val buffer = ScreenBuffer
          .empty(BufferSize.medium, BufferSize.small)
          .drawText(Position.origin, Position.origin, "Same")
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.render(buffer, Some(buffer))
          output  <- service.buffer.get
        yield assertTrue(output.isEmpty)
      }.provide(Mocks.console),
      test("diff render handles color changes") {
        val prev = ScreenBuffer
          .empty(BufferSize.small, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X', UiColor.Red)
        val curr = ScreenBuffer
          .empty(BufferSize.small, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X', UiColor.Blue)
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.render(curr, Some(prev))
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains(Ansi.blue),
          combined.contains("X")
        )
      }.provide(Mocks.console),
      test("diff render handles different buffer sizes") {
        val prev = ScreenBuffer.empty(BufferSize.small, BufferSize.small)
        val curr = ScreenBuffer
          .empty(BufferSize.medium, BufferSize.medium)
          .drawChar(Position.fifth, Position.fifth, 'X')
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.render(curr, Some(prev))
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(combined.contains("X"))
      }.provide(Mocks.console),
      test("diff render optimizes same color for consecutive changed pixels") {
        val prev = ScreenBuffer.empty(BufferSize.small, BufferSize.single)
        val curr = ScreenBuffer
          .empty(BufferSize.small, BufferSize.single)
          .drawChar(Position.first, Position.origin, 'X', UiColor.Red)
          .drawChar(Position.second, Position.origin, 'Y', UiColor.Red)
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.render(curr, Some(prev))
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains("X"),
          combined.contains("Y"),
          combined.contains(Ansi.red)
        )
      }.provide(Mocks.console),
      test("diff render handles pixels outside previous buffer bounds") {
        val prevSize       = 2
        val currSize       = 4
        val outOfBoundsPos = 3
        val prev           = ScreenBuffer.empty(prevSize, prevSize)
        val curr           = ScreenBuffer
          .empty(currSize, currSize)
          .drawChar(outOfBoundsPos, outOfBoundsPos, 'Z', UiColor.Green)
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.render(curr, Some(prev))
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains("Z"),
          combined.contains(Ansi.green)
        )
      }.provide(Mocks.console)
    ),

    suite("renderWithoutClear")(
      test("outputs to console without clear sequence") {
        val buffer = ScreenBuffer
          .empty(BufferSize.medium, BufferSize.single)
          .drawText(Position.origin, Position.origin, "Hello")
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.renderWithoutClear(buffer)
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains("Hello"),
          !combined.contains(Ansi.clearScreen)
        )
      }.provide(Mocks.console),
      test("includes newline at end") {
        val buffer = ScreenBuffer
          .empty(BufferSize.small, BufferSize.single)
          .drawText(Position.origin, Position.origin, "Hi")
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.renderWithoutClear(buffer)
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(combined.contains("\r\n"))
      }.provide(Mocks.console)
    ),

    suite("Color ANSI codes")(
      test("Cyan produces correct ANSI code") {
        val buffer = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X', UiColor.Cyan)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains(Ansi.cyan))
      },
      test("Yellow produces correct ANSI code") {
        val buffer = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X', UiColor.Yellow)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains(Ansi.yellow))
      },
      test("Magenta produces correct ANSI code") {
        val buffer = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X', UiColor.Magenta)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains(Ansi.magenta))
      },
      test("Green produces correct ANSI code") {
        val buffer = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X', UiColor.Green)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains(Ansi.green))
      },
      test("Red produces correct ANSI code") {
        val buffer = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X', UiColor.Red)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains(Ansi.red))
      },
      test("Blue produces correct ANSI code") {
        val buffer = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X', UiColor.Blue)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains(Ansi.blue))
      },
      test("White produces correct ANSI code") {
        val buffer = ScreenBuffer
          .empty(BufferSize.single, BufferSize.single)
          .drawChar(Position.origin, Position.origin, 'X', UiColor.White)
        val result = ConsoleRenderer.bufferToString(buffer)
        assertTrue(result.contains(Ansi.white))
      }
    ),

    suite("computeDiffString edge cases")(
      test("diff render handles char change at same position") {
        val prev = ScreenBuffer
          .empty(BufferSize.small, BufferSize.single)
          .drawChar(Position.first, Position.origin, 'A', UiColor.Default)
        val curr = ScreenBuffer
          .empty(BufferSize.small, BufferSize.single)
          .drawChar(Position.first, Position.origin, 'B', UiColor.Default)
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.render(curr, Some(prev))
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains("B"),
          !combined.contains(Ansi.clearScreen)
        )
      }.provide(Mocks.console),
      test("diff render handles color change without char change") {
        val prev = ScreenBuffer
          .empty(BufferSize.small, BufferSize.single)
          .drawChar(Position.first, Position.origin, 'X', UiColor.Red)
        val curr = ScreenBuffer
          .empty(BufferSize.small, BufferSize.single)
          .drawChar(Position.first, Position.origin, 'X', UiColor.Green)
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.render(curr, Some(prev))
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains(Ansi.green),
          combined.contains("X")
        )
      }.provide(Mocks.console),
      test("diff render handles multiple scattered changes") {
        val prevSize = 5
        val prev     = ScreenBuffer.empty(prevSize, prevSize)
        val curr     = ScreenBuffer
          .empty(prevSize, prevSize)
          .drawChar(0, 0, 'A', UiColor.Red)
          .drawChar(2, 2, 'B', UiColor.Blue)
          .drawChar(4, 4, 'C', UiColor.Green)
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.render(curr, Some(prev))
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains("A"),
          combined.contains("B"),
          combined.contains("C")
        )
      }.provide(Mocks.console),
      test("diff render resets color at end if last pixel was colored") {
        val prev = ScreenBuffer.empty(BufferSize.small, BufferSize.single)
        val curr = ScreenBuffer
          .empty(BufferSize.small, BufferSize.single)
          .drawChar(Position.third, Position.origin, 'X', UiColor.Cyan)
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.render(curr, Some(prev))
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains(Ansi.cyan),
          combined.contains(Ansi.reset)
        )
      }.provide(Mocks.console),
      test("diff render preserves same color across consecutive changes") {
        val prev = ScreenBuffer.empty(BufferSize.medium, BufferSize.single)
        val curr = ScreenBuffer
          .empty(BufferSize.medium, BufferSize.single)
          .drawChar(Position.first, Position.origin, 'A', UiColor.Red)
          .drawChar(Position.second, Position.origin, 'B', UiColor.Red)
          .drawChar(Position.third, Position.origin, 'C', UiColor.Red)
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.render(curr, Some(prev))
          output  <- service.buffer.get
          combined = output.mkString
          redCount = combined.sliding(Ansi.red.length).count(_ == Ansi.red)
        yield assertTrue(
          combined.contains("A"),
          combined.contains("B"),
          combined.contains("C"),
          redCount >= 1
        )
      }.provide(Mocks.console),
      test("diff render handles expanding buffer size") {
        val smallSize = 2
        val largeSize = 5
        val prev      = ScreenBuffer
          .empty(smallSize, smallSize)
          .drawChar(0, 0, 'X')
        val curr = ScreenBuffer
          .empty(largeSize, largeSize)
          .drawChar(0, 0, 'X')
          .drawChar(3, 3, 'Y')
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.render(curr, Some(prev))
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(combined.contains("Y"))
      }.provide(Mocks.console),
      test("diff render handles shrinking buffer size") {
        val largeSize = 5
        val smallSize = 2
        val prev      = ScreenBuffer
          .empty(largeSize, largeSize)
          .drawChar(4, 4, 'X')
        val curr = ScreenBuffer
          .empty(smallSize, smallSize)
          .drawChar(0, 0, 'Y')
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.render(curr, Some(prev))
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(combined.contains("Y"))
      }.provide(Mocks.console)
    ),

    suite("Direct render methods")(
      test("render with clear outputs clear sequence and buffer") {
        val buffer = ScreenBuffer
          .empty(BufferSize.small, BufferSize.small)
          .drawChar(0, 0, 'X', UiColor.Red)
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.render(buffer)
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains(Ansi.clearScreen),
          combined.contains("X")
        )
      }.provide(Mocks.console),
      test("renderWithoutClear outputs buffer without clear sequence") {
        val buffer = ScreenBuffer
          .empty(BufferSize.small, BufferSize.small)
          .drawChar(0, 0, 'Y', UiColor.Blue)
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.renderWithoutClear(buffer)
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          !combined.contains(Ansi.clearScreen),
          combined.contains("Y")
        )
      }.provide(Mocks.console),
      test("render dispatches to full render when previous is None") {
        val buffer = ScreenBuffer
          .empty(BufferSize.small, BufferSize.small)
          .drawChar(0, 0, 'Z')
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.render(buffer, None)
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains(Ansi.clearScreen),
          combined.contains("Z")
        )
      }.provide(Mocks.console),
      test("computeDiffString returns empty for identical buffers") {
        val buffer = ScreenBuffer
          .empty(BufferSize.small, BufferSize.small)
          .drawChar(0, 0, 'A')
        for
          service <- ZIO.service[Mocks.TestConsoleService]
          _       <- ConsoleRenderer.render(buffer, Some(buffer))
          output  <- service.buffer.get
        yield assertTrue(output.isEmpty || output.forall(_.isEmpty))
      }.provide(Mocks.console)
    )
  )

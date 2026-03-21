package monadris.infrastructure.terminal

import zio.*
import zio.test.*

import monadris.infrastructure.terminal.TestServices as Mocks

object LineReaderSpec extends ZIOSpecDefault:

  private object TestKeys:
    val Enter: Int       = '\r'.toInt
    val LineFeed: Int    = '\n'.toInt
    val Backspace: Int   = 127
    val BackspaceCR: Int = 8

  def spec = suite("LineReader")(
    test("Enter confirms input") {
      val inputs = Chunk('a'.toInt, 'b'.toInt, 'c'.toInt, TestKeys.Enter)
      for result <- LineReader.readLine
          .provide(
            Mocks.tty(inputs),
            Mocks.console
          )
      yield assertTrue(result == "abc")
    },
    test("LineFeed confirms input") {
      val inputs = Chunk('x'.toInt, 'y'.toInt, TestKeys.LineFeed)
      for result <- LineReader.readLine
          .provide(
            Mocks.tty(inputs),
            Mocks.console
          )
      yield assertTrue(result == "xy")
    },
    test("Backspace deletes last character") {
      val inputs = Chunk('a'.toInt, 'b'.toInt, TestKeys.Backspace, TestKeys.Enter)
      for result <- LineReader.readLine
          .provide(
            Mocks.tty(inputs),
            Mocks.console
          )
      yield assertTrue(result == "a")
    },
    test("BackspaceCR (code 8) also deletes last character") {
      val inputs = Chunk('a'.toInt, 'b'.toInt, TestKeys.BackspaceCR, TestKeys.Enter)
      for result <- LineReader.readLine
          .provide(
            Mocks.tty(inputs),
            Mocks.console
          )
      yield assertTrue(result == "a")
    },
    test("Backspace on empty buffer is ignored") {
      val inputs = Chunk(TestKeys.Backspace, 'a'.toInt, TestKeys.Enter)
      for result <- LineReader.readLine
          .provide(
            Mocks.tty(inputs),
            Mocks.console
          )
      yield assertTrue(result == "a")
    },
    test("Normal characters are echoed to console") {
      val inputs = Chunk('h'.toInt, 'i'.toInt, TestKeys.Enter)
      for
        service <- ZIO.service[Mocks.TestConsoleService]
        _       <- LineReader.readLine
          .provide(
            Mocks.tty(inputs),
            ZLayer.succeed(service)
          )
        output <- service.buffer.get
        combined = output.mkString
      yield assertTrue(
        combined.contains("h"),
        combined.contains("i")
      )
    }.provide(Mocks.console),
    test("Backspace emits erase sequence") {
      val inputs = Chunk('a'.toInt, TestKeys.Backspace, TestKeys.Enter)
      for
        service <- ZIO.service[Mocks.TestConsoleService]
        _       <- LineReader.readLine
          .provide(
            Mocks.tty(inputs),
            ZLayer.succeed(service)
          )
        output <- service.buffer.get
        combined = output.mkString
      yield assertTrue(combined.contains("\b \b"))
    }.provide(Mocks.console),
    test("Multiple characters with backspace and enter") {
      val inputs = Chunk(
        'h'.toInt,
        'e'.toInt,
        'l'.toInt,
        'p'.toInt,
        TestKeys.Backspace,
        TestKeys.Backspace,
        'l'.toInt,
        'o'.toInt,
        TestKeys.Enter
      )
      for result <- LineReader.readLine
          .provide(
            Mocks.tty(inputs),
            Mocks.console
          )
      yield assertTrue(result == "helo")
    },
    test("Empty input with immediate Enter") {
      val inputs = Chunk(TestKeys.Enter)
      for result <- LineReader.readLine
          .provide(
            Mocks.tty(inputs),
            Mocks.console
          )
      yield assertTrue(result == "")
    }
  )

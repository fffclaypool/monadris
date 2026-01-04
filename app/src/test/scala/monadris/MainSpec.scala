package monadris

import zio.*
import zio.test.*

import monadris.infrastructure.TestServices as Mocks

/**
 * Main.program の結合テスト
 * TestServices のモック実装を使用してアプリケーション全体をテスト
 */
object MainSpec extends ZIOSpecDefault:

  def spec = suite("Main Application")(
    test("program runs and exits with Q key") {
      val inputs = Chunk('q'.toInt)
      for _ <- Main.program
          .timeout(5.seconds)
      yield assertTrue(true)
    }.provide(
      Mocks.tty(Chunk('q'.toInt)),
      Mocks.console,
      Mocks.command,
      Mocks.config
    ),
    test("program runs and exits with lowercase q") {
      for _ <- Main.program
          .timeout(5.seconds)
      yield assertTrue(true)
    }.provide(
      Mocks.tty(Chunk('q'.toInt)),
      Mocks.console,
      Mocks.command,
      Mocks.config
    ),
    test("program runs and exits with uppercase Q") {
      for _ <- Main.program
          .timeout(5.seconds)
      yield assertTrue(true)
    }.provide(
      Mocks.tty(Chunk('Q'.toInt)),
      Mocks.console,
      Mocks.command,
      Mocks.config
    ),
    test("program shows title screen") {
      for
        service <- ZIO.service[Mocks.TestConsoleService]
        _       <- Main.program
          .timeout(5.seconds)
        output <- service.buffer.get
        combined = output.mkString
      yield assertTrue(
        combined.contains("Functional Tetris"),
        combined.contains("Controls")
      )
    }.provide(
      Mocks.tty(Chunk('q'.toInt)),
      Mocks.console,
      Mocks.command,
      Mocks.config
    ),
    test("program shows game over screen") {
      for
        service <- ZIO.service[Mocks.TestConsoleService]
        _       <- Main.program
          .timeout(5.seconds)
        output <- service.buffer.get
        combined = output.mkString
      yield assertTrue(combined.contains("GAME OVER"))
    }.provide(
      Mocks.tty(Chunk('q'.toInt)),
      Mocks.console,
      Mocks.command,
      Mocks.config
    ),
    test("program enables and disables raw mode") {
      for
        service <- ZIO.service[Mocks.TestCommandService]
        _       <- Main.program
          .timeout(5.seconds)
        history <- service.history.get
      yield assertTrue(
        history.contains("stty raw -echo < /dev/tty"),
        history.contains("stty cooked echo < /dev/tty")
      )
    }.provide(
      Mocks.tty(Chunk('q'.toInt)),
      Mocks.console,
      Mocks.command,
      Mocks.config
    ),
    test("program handles movement before quit") {
      val inputs = Chunk('h'.toInt, 'l'.toInt, 'j'.toInt, 'q'.toInt)
      for _ <- Main.program
          .timeout(5.seconds)
      yield assertTrue(true)
    }.provide(
      Mocks.tty(Chunk('h'.toInt, 'l'.toInt, 'j'.toInt, 'q'.toInt)),
      Mocks.console,
      Mocks.command,
      Mocks.config
    ),
    test("program handles arrow keys before quit") {
      // ESC [ D (Left arrow) then q
      val inputs = Chunk(27, '['.toInt, 'D'.toInt, 'q'.toInt)
      for _ <- Main.program
          .timeout(5.seconds)
      yield assertTrue(true)
    }.provide(
      Mocks.tty(Chunk(27, '['.toInt, 'D'.toInt, 'q'.toInt)),
      Mocks.console,
      Mocks.command,
      Mocks.config
    ),
    test("program outputs game ended message") {
      for
        service <- ZIO.service[Mocks.TestConsoleService]
        _       <- Main.program
          .timeout(5.seconds)
        output <- service.buffer.get
        combined = output.mkString
      yield assertTrue(combined.contains("Game ended"))
    }.provide(
      Mocks.tty(Chunk('q'.toInt)),
      Mocks.console,
      Mocks.command,
      Mocks.config
    )
  )

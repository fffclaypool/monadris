package monadris.infrastructure.terminal

import zio.*
import zio.test.*

import monadris.infrastructure.terminal.TestServices as Mocks

/** Checks that the test doubles behave the way the other specs rely on. */
object TestServicesSpec extends ZIOSpecDefault:

  private val typedKeys = Chunk('h'.toInt, 'j'.toInt, 'k'.toInt)
  private val printed   = List("Hello", "World")
  private val commands  = List("echo hello", "ls -la")
  private val anySleep  = 1000L

  def spec = suite("TestServices")(
    test("tty hands out the queued keys in order and reports how many are left") {
      for
        before <- TtyService.available()
        keys   <- ZIO.replicateZIO(typedKeys.size)(TtyService.read())
        after  <- TtyService.available()
      yield assertTrue(before == typedKeys.size, Chunk.fromIterable(keys) == typedKeys, after == 0)
    }.provide(Mocks.tty(typedKeys)),
    test("tty sleep returns immediately") {
      TtyService.sleep(anySleep).as(assertCompletes)
    }.provide(Mocks.tty(Chunk.empty)),
    test("console accumulates what was printed") {
      for
        console <- ZIO.service[Mocks.TestConsoleService]
        _       <- ZIO.foreachDiscard(printed)(ConsoleService.print)
        _       <- ConsoleService.flush()
        output  <- console.buffer.get
      yield assertTrue(output == printed)
    }.provide(Mocks.console),
    test("command service records what was executed") {
      for
        service <- ZIO.service[Mocks.TestCommandService]
        _       <- ZIO.foreachDiscard(commands)(CommandService.exec)
        history <- service.history.get
      yield assertTrue(history == commands)
    }.provide(Mocks.command)
  )

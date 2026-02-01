package monadris.infrastructure.terminal

import zio.*
import zio.test.*

object TerminalSessionSpec extends ZIOSpecDefault:

  case class TestCommandService(history: Ref[List[String]]) extends CommandService:
    def exec(cmd: String): Task[Unit] = history.update(_ :+ cmd)

  case class TestConsoleService(buffer: Ref[List[String]]) extends ConsoleService:
    def print(text: String): Task[Unit] = buffer.update(_ :+ text)
    def flush(): Task[Unit]             = ZIO.unit

  final case class TestEnv(
    session: TerminalSession,
    cmdHistory: Ref[List[String]],
    consoleBuffer: Ref[List[String]]
  )

  def testEnv(inputs: Chunk[Int]): ZIO[Any, Nothing, TestEnv] =
    for
      cmdHistory    <- Ref.make(List.empty[String])
      consoleBuffer <- Ref.make(List.empty[String])
      queue         <- Queue.unbounded[Int]
      _             <- queue.offerAll(inputs)
      tty = new TtyService:
        def available(): Task[Int]                                 = queue.size
        def read(): Task[Int]                                      = queue.take
        def sleep(ms: Long): Task[Unit]                            = ZIO.unit
        def readByteWithTimeout(timeoutMs: Int): Task[Option[Int]] = queue.poll.map(_.map(_.toInt))
      console = TestConsoleService(consoleBuffer)
      cmd     = TestCommandService(cmdHistory)
      layer   = ZLayer.succeed(tty) ++ ZLayer.succeed[ConsoleService](console) ++ ZLayer.succeed[CommandService](cmd)
      session <- ZIO.service[TerminalSession].provideLayer(layer >>> TerminalSession.live)
    yield TestEnv(session, cmdHistory, consoleBuffer)

  def spec = suite("TerminalSession")(
    suite("TerminalSession.live")(
      test("showMessage prints message and toggles raw mode") {
        for
          env    <- testEnv(Chunk.empty)
          _      <- env.session.showMessage("Hello World")
          cmds   <- env.cmdHistory.get
          output <- env.consoleBuffer.get
        yield assertTrue(
          cmds.contains("stty cooked echo < /dev/tty"),
          cmds.contains("stty raw -echo < /dev/tty"),
          output.contains("Hello World")
        )
      },
      test("prompt prints message and reads input") {
        val inputs = Chunk('t'.toInt, 'e'.toInt, 's'.toInt, 't'.toInt, '\r'.toInt)
        for
          env    <- testEnv(inputs)
          result <- env.session.prompt("Enter: ")
          cmds   <- env.cmdHistory.get
          output <- env.consoleBuffer.get
        yield assertTrue(
          result == "test",
          cmds.contains("stty cooked echo < /dev/tty"),
          cmds.contains("stty raw -echo < /dev/tty"),
          output.contains("Enter: ")
        )
      },
      test("prompt handles backspace") {
        val inputs = Chunk('a'.toInt, 'b'.toInt, 127, 'c'.toInt, '\r'.toInt)
        for
          env    <- testEnv(inputs)
          result <- env.session.prompt("? ")
          output <- env.consoleBuffer.get
        yield assertTrue(
          result == "ac",
          output.contains("\b \b")
        )
      },
      test("prompt handles alternate backspace code") {
        val inputs = Chunk('x'.toInt, 8, 'y'.toInt, '\r'.toInt)
        for
          env    <- testEnv(inputs)
          result <- env.session.prompt("? ")
        yield assertTrue(result == "y")
      },
      test("prompt handles line feed as enter") {
        val inputs = Chunk('o'.toInt, 'k'.toInt, '\n'.toInt)
        for
          env    <- testEnv(inputs)
          result <- env.session.prompt("? ")
        yield assertTrue(result == "ok")
      },
      test("prompt ignores backspace on empty buffer") {
        val inputs = Chunk(127, 'a'.toInt, '\r'.toInt)
        for
          env    <- testEnv(inputs)
          result <- env.session.prompt("? ")
        yield assertTrue(result == "a")
      },
      test("waitForKeypress reads single key") {
        for
          env <- testEnv(Chunk('x'.toInt))
          key <- env.session.waitForKeypress()
        yield assertTrue(key == 'x'.toInt)
      },
      test("showMessageAndWait prints message and waits for keypress") {
        for
          env    <- testEnv(Chunk(' '.toInt))
          _      <- env.session.showMessageAndWait("Done!")
          cmds   <- env.cmdHistory.get
          output <- env.consoleBuffer.get
        yield assertTrue(
          cmds.contains("stty cooked echo < /dev/tty"),
          cmds.contains("stty raw -echo < /dev/tty"),
          output.contains("Done!"),
          output.contains("Press any key to continue...")
        )
      },
      test("enableRawMode executes stty command") {
        for
          env  <- testEnv(Chunk.empty)
          _    <- env.session.enableRawMode
          cmds <- env.cmdHistory.get
        yield assertTrue(cmds.contains("stty raw -echo < /dev/tty"))
      },
      test("disableRawMode executes stty command") {
        for
          env  <- testEnv(Chunk.empty)
          _    <- env.session.disableRawMode
          cmds <- env.cmdHistory.get
        yield assertTrue(cmds.contains("stty cooked echo < /dev/tty"))
      },
      test("withRawMode enables and disables raw mode around effect") {
        for
          env  <- testEnv(Chunk.empty)
          _    <- env.session.withRawMode(ZIO.unit)
          cmds <- env.cmdHistory.get
        yield assertTrue(
          cmds.head == "stty raw -echo < /dev/tty",
          cmds.last == "stty cooked echo < /dev/tty"
        )
      },
      test("withCooked enables and disables cooked mode around effect") {
        for
          env  <- testEnv(Chunk.empty)
          _    <- env.session.withCooked(ZIO.unit)
          cmds <- env.cmdHistory.get
        yield assertTrue(
          cmds.head == "stty cooked echo < /dev/tty",
          cmds.last == "stty raw -echo < /dev/tty"
        )
      }
    ),
    suite("TerminalSession companion object")(
      test("showMessage delegates to service") {
        for
          env    <- testEnv(Chunk.empty)
          _      <- TerminalSession.showMessage("Test").provideLayer(ZLayer.succeed(env.session))
          output <- env.consoleBuffer.get
        yield assertTrue(output.contains("Test"))
      },
      test("prompt delegates to service") {
        val inputs = Chunk('h'.toInt, 'i'.toInt, '\r'.toInt)
        for
          env    <- testEnv(inputs)
          result <- TerminalSession.prompt("? ").provideLayer(ZLayer.succeed(env.session))
        yield assertTrue(result == "hi")
      },
      test("waitForKeypress delegates to service") {
        for
          env <- testEnv(Chunk('k'.toInt))
          key <- TerminalSession.waitForKeypress().provideLayer(ZLayer.succeed(env.session))
        yield assertTrue(key == 'k'.toInt)
      },
      test("showMessageAndWait delegates to service") {
        for
          env    <- testEnv(Chunk(' '.toInt))
          _      <- TerminalSession.showMessageAndWait("Msg").provideLayer(ZLayer.succeed(env.session))
          output <- env.consoleBuffer.get
        yield assertTrue(output.contains("Msg"))
      },
      test("enableRawMode delegates to service") {
        for
          env  <- testEnv(Chunk.empty)
          _    <- TerminalSession.enableRawMode.provideLayer(ZLayer.succeed(env.session))
          cmds <- env.cmdHistory.get
        yield assertTrue(cmds.contains("stty raw -echo < /dev/tty"))
      },
      test("disableRawMode delegates to service") {
        for
          env  <- testEnv(Chunk.empty)
          _    <- TerminalSession.disableRawMode.provideLayer(ZLayer.succeed(env.session))
          cmds <- env.cmdHistory.get
        yield assertTrue(cmds.contains("stty cooked echo < /dev/tty"))
      },
      test("withRawMode wraps effect with raw mode") {
        for
          env  <- testEnv(Chunk.empty)
          _    <- TerminalSession.withRawMode(ZIO.unit).provideLayer(ZLayer.succeed(env.session))
          cmds <- env.cmdHistory.get
        yield assertTrue(
          cmds.head == "stty raw -echo < /dev/tty",
          cmds.last == "stty cooked echo < /dev/tty"
        )
      },
      test("withCooked wraps effect with cooked mode") {
        for
          env  <- testEnv(Chunk.empty)
          _    <- TerminalSession.withCooked(ZIO.unit).provideLayer(ZLayer.succeed(env.session))
          cmds <- env.cmdHistory.get
        yield assertTrue(
          cmds.head == "stty cooked echo < /dev/tty",
          cmds.last == "stty raw -echo < /dev/tty"
        )
      }
    )
  )

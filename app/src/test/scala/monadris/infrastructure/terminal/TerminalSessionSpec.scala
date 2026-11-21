package monadris.infrastructure.terminal

import java.nio.charset.StandardCharsets

import zio.*
import zio.test.*

object TerminalSessionSpec extends ZIOSpecDefault:

  private object Commands:
    val SaveState  = "stty -g < /dev/tty"
    val Raw        = "stty raw -echo < /dev/tty"
    val Cooked     = "stty cooked echo < /dev/tty"
    val SavedState = "4500:5:bf:8a3b:3:1c:7f:15:4:0:1:0"
    val Restore    = s"stty $SavedState < /dev/tty"

  private object Ansi:
    val ShowCursor = "\u001b[?25h"

  private val EndOfStream = -1

  case class TestCommandService(history: Ref[List[String]], sttyState: String) extends CommandService:
    def exec(cmd: String): Task[Unit]          = history.update(_ :+ cmd)
    def execCapture(cmd: String): Task[String] = history.update(_ :+ cmd).as(sttyState)

  case class TestConsoleService(buffer: Ref[List[String]]) extends ConsoleService:
    def print(text: String): Task[Unit] = buffer.update(_ :+ text)
    def flush(): Task[Unit]             = ZIO.unit

  final case class TestEnv(
    session: TerminalSession,
    cmdHistory: Ref[List[String]],
    consoleBuffer: Ref[List[String]]
  )

  def testEnv(inputs: Chunk[Int], sttyState: String = ""): ZIO[Any, Nothing, TestEnv] =
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
      cmd     = TestCommandService(cmdHistory, sttyState)
      layer   = ZLayer.succeed(tty) ++ ZLayer.succeed[ConsoleService](console) ++ ZLayer.succeed[CommandService](cmd)
      session <- ZIO.service[TerminalSession].provideLayer(layer >>> TerminalSession.live)
    yield TestEnv(session, cmdHistory, consoleBuffer)

  private def utf8Bytes(text: String): Chunk[Int] =
    Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8)).map(_ & 0xff)

  def spec = suite("TerminalSession")(
    suite("TerminalSession.live")(
      test("showMessage prints message without touching the terminal mode") {
        for
          env    <- testEnv(Chunk.empty)
          _      <- env.session.showMessage("Hello World")
          cmds   <- env.cmdHistory.get
          output <- env.consoleBuffer.get
        yield assertTrue(
          cmds.isEmpty,
          output.contains("Hello World")
        )
      },
      test("prompt reads a line in line mode and returns to raw mode") {
        val inputs = utf8Bytes("test") :+ '\r'.toInt
        for
          env    <- testEnv(inputs)
          result <- env.session.prompt("Enter: ")
          cmds   <- env.cmdHistory.get
          output <- env.consoleBuffer.get
        yield assertTrue(
          result == "test",
          cmds.head == Commands.Cooked,
          cmds.last == Commands.Raw,
          output.contains("Enter: ")
        )
      },
      test("prompt does not echo typed characters itself") {
        val inputs = utf8Bytes("abc") :+ '\n'.toInt
        for
          env    <- testEnv(inputs)
          _      <- env.session.prompt("? ")
          output <- env.consoleBuffer.get
        yield assertTrue(!output.exists(text => text == "a" || text == "b" || text == "c"))
      },
      test("prompt decodes multi-byte UTF-8 input") {
        val name   = "テスト再生"
        val inputs = utf8Bytes(name) :+ '\n'.toInt
        for
          env    <- testEnv(inputs)
          result <- env.session.prompt("? ")
        yield assertTrue(result == name)
      },
      test("prompt handles line feed as enter") {
        val inputs = Chunk('o'.toInt, 'k'.toInt, '\n'.toInt)
        for
          env    <- testEnv(inputs)
          result <- env.session.prompt("? ")
        yield assertTrue(result == "ok")
      },
      test("prompt ends the line at end of stream") {
        val inputs = Chunk('o'.toInt, 'k'.toInt, EndOfStream)
        for
          env    <- testEnv(inputs)
          result <- env.session.prompt("? ")
        yield assertTrue(result == "ok")
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
          cmds.isEmpty,
          output.mkString.contains("Done!"),
          output.mkString.contains("Press any key to continue...")
        )
      },
      test("enableRawMode captures the current settings and switches to raw mode") {
        for
          env  <- testEnv(Chunk.empty)
          _    <- env.session.enableRawMode
          cmds <- env.cmdHistory.get
        yield assertTrue(cmds == List(Commands.SaveState, Commands.Raw))
      },
      test("disableRawMode restores the settings captured by enableRawMode") {
        for
          env  <- testEnv(Chunk.empty, Commands.SavedState)
          _    <- env.session.enableRawMode
          _    <- env.session.disableRawMode
          cmds <- env.cmdHistory.get
        yield assertTrue(cmds.last == Commands.Restore)
      },
      test("disableRawMode falls back to cooked mode when no settings were captured") {
        for
          env  <- testEnv(Chunk.empty)
          _    <- env.session.disableRawMode
          cmds <- env.cmdHistory.get
        yield assertTrue(cmds == List(Commands.Cooked))
      },
      test("disableRawMode ignores captured settings that are not a valid stty state") {
        for
          env  <- testEnv(Chunk.empty, "x; rm -rf /")
          _    <- env.session.enableRawMode
          _    <- env.session.disableRawMode
          cmds <- env.cmdHistory.get
        yield assertTrue(cmds.last == Commands.Cooked)
      },
      test("disableRawMode keeps the settings from the first enableRawMode") {
        for
          env  <- testEnv(Chunk('\n'.toInt), Commands.SavedState)
          _    <- env.session.enableRawMode
          _    <- env.session.prompt("? ")
          _    <- env.session.disableRawMode
          cmds <- env.cmdHistory.get
        yield assertTrue(cmds.count(_ == Commands.Restore) == 2)
      },
      test("disableRawMode shows the cursor again") {
        for
          env    <- testEnv(Chunk.empty)
          _      <- env.session.disableRawMode
          output <- env.consoleBuffer.get
        yield assertTrue(output.contains(Ansi.ShowCursor))
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
        yield assertTrue(output.mkString.contains("Msg"))
      },
      test("enableRawMode delegates to service") {
        for
          env  <- testEnv(Chunk.empty)
          _    <- TerminalSession.enableRawMode.provideLayer(ZLayer.succeed(env.session))
          cmds <- env.cmdHistory.get
        yield assertTrue(cmds.contains(Commands.Raw))
      },
      test("disableRawMode delegates to service") {
        for
          env  <- testEnv(Chunk.empty)
          _    <- TerminalSession.disableRawMode.provideLayer(ZLayer.succeed(env.session))
          cmds <- env.cmdHistory.get
        yield assertTrue(cmds.contains(Commands.Cooked))
      },
      test("withRawMode wraps effect with raw mode and restores afterwards") {
        for
          env  <- testEnv(Chunk.empty, Commands.SavedState)
          _    <- TerminalSession.withRawMode(ZIO.unit).provideLayer(ZLayer.succeed(env.session))
          cmds <- env.cmdHistory.get
        yield assertTrue(cmds == List(Commands.SaveState, Commands.Raw, Commands.Restore))
      },
      test("withRawMode restores the terminal when the effect fails") {
        for
          env <- testEnv(Chunk.empty, Commands.SavedState)
          _   <- TerminalSession
            .withRawMode(ZIO.fail(new RuntimeException("boom")))
            .provideLayer(ZLayer.succeed(env.session))
            .either
          cmds <- env.cmdHistory.get
        yield assertTrue(cmds.last == Commands.Restore)
      }
    )
  )

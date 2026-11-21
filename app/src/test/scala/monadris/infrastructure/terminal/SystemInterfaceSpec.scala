package monadris.infrastructure.terminal

import java.io.ByteArrayInputStream
import java.nio.file.Files

import zio.*
import zio.test.*

import monadris.infrastructure.terminal.TestServices as LocalTestServices

object SystemInterfaceSpec extends ZIOSpecDefault:

  private object Input:
    val Bytes: Array[Byte] = Array('a'.toByte, 'b'.toByte)
    val EndOfStream        = -1
    val TimeoutMs          = 50
    val MissingPath        = "/nonexistent/monadris-tty"

  private object Shell:
    val Succeeding      = "true"
    val FailingExitCode = "3"
    val Failing         = s"exit $FailingExitCode"
    val EchoedText      = "monadris"
    val Echo            = s"echo $EchoedText"
    val EchoToStderr    = s"echo $EchoedText 1>&2"

  def spec = suite("SystemInterface")(
    suite("GameEnv type alias")(
      test("GameEnv includes TerminalSession") {
        val layer = LocalTestServices.tty(Chunk.empty) ++
          LocalTestServices.console ++
          LocalTestServices.command ++
          LocalTestServices.config ++
          LocalTestServices.terminalSession(Chunk.empty)

        val effect = for
          _       <- ZIO.service[TtyService]
          _       <- ZIO.service[ConsoleService]
          _       <- ZIO.service[CommandService]
          session <- ZIO.service[TerminalSession]
        yield assertTrue(session != null)

        effect.provideLayer(layer)
      }
    ),
    suite("TtyService")(
      test("reads the bytes of the underlying stream in order") {
        val tty = TtyService.fromInputStream(new ByteArrayInputStream(Input.Bytes))
        for
          available <- tty.available()
          first     <- tty.read()
          second    <- tty.read()
          atEnd     <- tty.read()
        yield assertTrue(
          available == Input.Bytes.length,
          first == Input.Bytes(0).toInt,
          second == Input.Bytes(1).toInt,
          atEnd == Input.EndOfStream
        )
      },
      test("readByteWithTimeout returns a waiting byte immediately") {
        val tty = TtyService.fromInputStream(new ByteArrayInputStream(Input.Bytes))
        for byte <- tty.readByteWithTimeout(Input.TimeoutMs)
        yield assertTrue(byte == Some(Input.Bytes(0).toInt))
      },
      test("readByteWithTimeout gives up after the timeout when nothing arrives") {
        val tty = TtyService.fromInputStream(new ByteArrayInputStream(Array.emptyByteArray))
        for
          fiber  <- tty.readByteWithTimeout(Input.TimeoutMs).fork
          _      <- TestClock.adjust(Input.TimeoutMs.millis)
          result <- fiber.join
        yield assertTrue(result.isEmpty)
      },
      test("readByteWithTimeout does not wait at all for a non-positive timeout") {
        val tty = TtyService.fromInputStream(new ByteArrayInputStream(Array.emptyByteArray))
        for result <- tty.readByteWithTimeout(0)
        yield assertTrue(result.isEmpty)
      },
      test("sleep suspends for the given time") {
        val tty = TtyService.fromInputStream(new ByteArrayInputStream(Array.emptyByteArray))
        for
          fiber <- tty.sleep(Input.TimeoutMs.toLong).fork
          _     <- TestClock.adjust(Input.TimeoutMs.millis)
          _     <- fiber.join
        yield assertTrue(true)
      },
      test("open reads from the given path and closes it with the scope") {
        ZIO.scoped {
          for
            path  <- ZIO.attemptBlocking(Files.createTempFile("monadris-tty", ".bin"))
            _     <- ZIO.addFinalizer(ZIO.attemptBlocking(Files.deleteIfExists(path)).ignore)
            _     <- ZIO.attemptBlocking(Files.write(path, Input.Bytes))
            tty   <- TtyService.open(path.toString)
            first <- tty.read()
          yield assertTrue(first == Input.Bytes(0).toInt)
        }
      },
      test("open fails for a path that does not exist") {
        for result <- ZIO.scoped(TtyService.open(Input.MissingPath)).either
        yield assertTrue(result.isLeft)
      }
    ),
    suite("CommandService.live")(
      test("exec succeeds for a command that exits with zero") {
        for result <- CommandService.exec(Shell.Succeeding).either
        yield assertTrue(result.isRight)
      }.provide(CommandService.live),
      test("exec fails for a command that exits with a non-zero status") {
        for result <- CommandService.exec(Shell.Failing).either
        yield assertTrue(result.left.exists(_.getMessage.contains(Shell.FailingExitCode)))
      }.provide(CommandService.live),
      test("execCapture returns the standard output of the command") {
        for output <- CommandService.execCapture(Shell.Echo)
        yield assertTrue(output.trim == Shell.EchoedText)
      }.provide(CommandService.live),
      test("execCapture does not include standard error") {
        for output <- CommandService.execCapture(Shell.EchoToStderr)
        yield assertTrue(output.isEmpty)
      }.provide(CommandService.live)
    ),
    suite("TerminalSession.live")(
      test("can be constructed from terminal services") {
        val terminalServices = LocalTestServices.tty(Chunk.empty) ++
          LocalTestServices.console ++
          LocalTestServices.command

        val effect = ZIO.service[TerminalSession]

        effect
          .provideLayer(terminalServices >>> TerminalSession.live)
          .map(session => assertTrue(session != null))
      }
    )
  )

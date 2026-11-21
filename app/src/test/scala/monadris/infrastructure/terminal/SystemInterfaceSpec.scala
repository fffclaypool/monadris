package monadris.infrastructure.terminal

import zio.*
import zio.test.*

import monadris.infrastructure.terminal.TestServices as LocalTestServices

object SystemInterfaceSpec extends ZIOSpecDefault:

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

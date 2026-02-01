package monadris.infrastructure.terminal

import zio.*
import zio.test.*

import monadris.infrastructure.terminal.TestServices as LocalTestServices

object SystemInterfaceSpec extends ZIOSpecDefault:

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

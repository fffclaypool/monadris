package monadris.infrastructure.game

import zio.*

import monadris.config.AppConfig
import monadris.infrastructure.terminal.TerminalInput
import monadris.infrastructure.terminal.TtyService
import monadris.input.GameCommand

object InputStream:

  def run(
    queue: Queue[GameCommand],
    pollIntervalMs: Int
  ): ZIO[TtyService & AppConfig, Throwable, Unit] =
    val readAndOffer = for
      parseResult <- TerminalInput.readKeyZIO
      _           <- parseResult match
        case TerminalInput.ParseResult.Timeout =>
          TtyService.sleep(pollIntervalMs)
        case TerminalInput.ParseResult.Regular(key) if TerminalInput.isQuitKey(key) =>
          ZIO.logInfo("Quit key pressed") *> queue.offer(GameCommand.Quit)
        case _ =>
          TerminalInput.toInput(parseResult) match
            case Some(input) => queue.offer(GameCommand.UserAction(input))
            case None        => ZIO.unit
    yield ()

    readAndOffer.forever

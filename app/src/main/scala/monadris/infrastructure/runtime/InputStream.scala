package monadris.infrastructure.runtime

import zio.*

import monadris.domain.config.AppConfig
import monadris.infrastructure.io.TerminalInput
import monadris.infrastructure.io.TtyService

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

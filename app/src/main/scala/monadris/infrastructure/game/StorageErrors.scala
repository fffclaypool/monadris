package monadris.infrastructure.game

import zio.*

import monadris.infrastructure.persistence.ReplayError
import monadris.infrastructure.terminal.TerminalSession

/** Reports replay storage failures to the player instead of aborting the application. */
private[game] object StorageErrors:

  def report(action: String, error: ReplayError): ZIO[TerminalSession, Throwable, Unit] =
    for
      _ <- error match
        case ReplayError.StorageFailure(cause) => ZIO.logErrorCause(s"Failed to $action", Cause.fail(cause))
        case other                             => ZIO.logWarning(s"Failed to $action: ${other.message}")
      _ <- TerminalSession.showMessageAndWait(s"\r\nFailed to $action: ${singleLine(error.message)}\r\n")
    yield ()

  /** The terminal is in raw mode, so embedded line breaks would garble the screen. */
  private def singleLine(text: String): String =
    text.linesIterator.map(_.trim).filter(_.nonEmpty).mkString(" ")

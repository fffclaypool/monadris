package monadris.infrastructure.game

import zio.*

import monadris.infrastructure.terminal.TerminalSession

/** Reports replay storage failures to the player instead of aborting the application. */
private[game] object StorageErrors:

  def report(action: String, error: Throwable): ZIO[TerminalSession, Throwable, Unit] =
    for
      _ <- ZIO.logErrorCause(s"Failed to $action", Cause.fail(error))
      _ <- TerminalSession.showMessageAndWait(s"\r\nFailed to $action: ${singleLine(describe(error))}\r\n")
    yield ()

  private def describe(error: Throwable): String =
    Option(error.getMessage).getOrElse(error.getClass.getSimpleName)

  /** The terminal is in raw mode, so embedded line breaks would garble the screen. */
  private def singleLine(text: String): String =
    text.linesIterator.map(_.trim).filter(_.nonEmpty).mkString(" ")

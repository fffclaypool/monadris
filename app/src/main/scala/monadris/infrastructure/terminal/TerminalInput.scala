package monadris.infrastructure.terminal

import zio.*

import monadris.config.AppConfig
import monadris.domain.Input
import monadris.input.KeyMapping

object TerminalInput:

  export KeyMapping.EscapeKeyCode
  export KeyMapping.ParseResult
  export KeyMapping.keyToInput
  export KeyMapping.isQuitKey
  export KeyMapping.arrowToInput
  export KeyMapping.toInput

  def parseEscapeSequenceZIO: ZIO[TtyService & AppConfig, Throwable, Option[Input]] =
    for
      config    <- ZIO.service[AppConfig]
      _         <- TtyService.sleep(config.terminal.escapeSequenceWaitMs)
      available <- TtyService.available()
      result    <-
        if available <= 0 then ZIO.succeed(None)
        else parseEscapeBody
    yield result

  private def parseEscapeBody: ZIO[TtyService & AppConfig, Throwable, Option[Input]] =
    for
      second <- TtyService.read()
      result <-
        if second != '[' then ZIO.succeed(None)
        else parseArrowKey
    yield result

  private def parseArrowKey: ZIO[TtyService & AppConfig, Throwable, Option[Input]] =
    for
      config    <- ZIO.service[AppConfig]
      _         <- TtyService.sleep(config.terminal.escapeSequenceSecondWaitMs)
      available <- TtyService.available()
      result    <-
        if available <= 0 then ZIO.succeed(None)
        else TtyService.read().map(key => KeyMapping.arrowToInput(key))
    yield result

  def readKeyZIO: ZIO[TtyService & AppConfig, Throwable, KeyMapping.ParseResult] =
    for
      available <- TtyService.available()
      result    <-
        if available <= 0 then ZIO.succeed(KeyMapping.ParseResult.Timeout)
        else readKeyBody
    yield result

  private def readKeyBody: ZIO[TtyService & AppConfig, Throwable, KeyMapping.ParseResult] =
    for
      key    <- TtyService.read()
      result <- parseKeyResult(key)
    yield result

  private def parseKeyResult(key: Int): ZIO[TtyService & AppConfig, Throwable, KeyMapping.ParseResult] =
    if key == KeyMapping.EscapeKeyCode then
      parseEscapeSequenceZIO.map {
        case Some(input) => KeyMapping.ParseResult.Arrow(input)
        case None        => KeyMapping.ParseResult.Unknown
      }
    else ZIO.succeed(KeyMapping.ParseResult.Regular(key))

object TerminalControl:

  def enableRawMode: ZIO[CommandService, Throwable, Unit] =
    CommandService.exec("stty raw -echo < /dev/tty")

  def disableRawMode: ZIO[CommandService, Throwable, Unit] =
    CommandService.exec("stty cooked echo < /dev/tty")

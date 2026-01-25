package monadris.infrastructure

import zio.*

import monadris.domain.Input
import monadris.domain.config.AppConfig

object TerminalInput:

  final val EscapeKeyCode: Int = 27

  enum ParseResult:
    case Arrow(input: Input)
    case Regular(key: Int)
    case Timeout
    case Unknown

  private val arrowKeyMap: Map[Char, Input] = Map(
    'A' -> Input.RotateClockwise,
    'B' -> Input.MoveDown,
    'C' -> Input.MoveRight,
    'D' -> Input.MoveLeft
  )

  private val regularKeyMap: Map[Char, Input] = Map(
    'h' -> Input.MoveLeft,
    'H' -> Input.MoveLeft,
    'l' -> Input.MoveRight,
    'L' -> Input.MoveRight,
    'j' -> Input.MoveDown,
    'J' -> Input.MoveDown,
    'k' -> Input.RotateClockwise,
    'K' -> Input.RotateClockwise,
    'z' -> Input.RotateCounterClockwise,
    'Z' -> Input.RotateCounterClockwise,
    ' ' -> Input.HardDrop,
    'p' -> Input.Pause,
    'P' -> Input.Pause
  )

  def keyToInput(key: Int): Option[Input] =
    regularKeyMap.get(key.toChar)

  def isQuitKey(key: Int): Boolean =
    key == 'q' || key == 'Q'

  def arrowToInput(key: Int): Option[Input] =
    arrowKeyMap.get(key.toChar)

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
        else TtyService.read().map(key => arrowToInput(key))
    yield result

  def readKeyZIO: ZIO[TtyService & AppConfig, Throwable, ParseResult] =
    for
      available <- TtyService.available()
      result    <-
        if available <= 0 then ZIO.succeed(ParseResult.Timeout)
        else readKeyBody
    yield result

  private def readKeyBody: ZIO[TtyService & AppConfig, Throwable, ParseResult] =
    for
      key    <- TtyService.read()
      result <- parseKeyResult(key)
    yield result

  private def parseKeyResult(key: Int): ZIO[TtyService & AppConfig, Throwable, ParseResult] =
    if key == EscapeKeyCode then
      parseEscapeSequenceZIO.map {
        case Some(input) => ParseResult.Arrow(input)
        case None        => ParseResult.Unknown
      }
    else ZIO.succeed(ParseResult.Regular(key))

  def toInput(result: ParseResult): Option[Input] =
    result match
      case ParseResult.Arrow(input) => Some(input)
      case ParseResult.Regular(key) => keyToInput(key)
      case ParseResult.Timeout      => None
      case ParseResult.Unknown      => None

object TerminalControl:

  def enableRawMode: ZIO[CommandService, Throwable, Unit] =
    CommandService.exec("stty raw -echo < /dev/tty")

  def disableRawMode: ZIO[CommandService, Throwable, Unit] =
    CommandService.exec("stty cooked echo < /dev/tty")

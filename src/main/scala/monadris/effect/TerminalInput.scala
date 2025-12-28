package monadris.effect

import zio.*

import monadris.domain.Input

/**
 * ターミナル入力のパース処理を集約
 * エスケープシーケンス解析を共通化し、ネストを削減
 */
object TerminalInput:

  /** キーコード定数 */
  final val EscapeKeyCode: Int = 27

  /** エスケープシーケンス解析用の待機時間（ミリ秒） */
  private final val EscapeSequenceWaitMs: Int = 20
  private final val EscapeSequenceSecondWaitMs: Int = 5

  /** エスケープシーケンスの解析結果 */
  enum ParseResult:
    case Arrow(input: Input)
    case Regular(key: Int)
    case Timeout
    case Unknown

  /** 矢印キーのマッピング */
  private val arrowKeyMap: Map[Char, Input] = Map(
    'A' -> Input.RotateClockwise,
    'B' -> Input.MoveDown,
    'C' -> Input.MoveRight,
    'D' -> Input.MoveLeft
  )

  /** 通常キーのマッピング */
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

  /**
   * キーコードをInputに変換（純粋関数）
   */
  def keyToInput(key: Int): Option[Input] =
    regularKeyMap.get(key.toChar)

  /**
   * 終了キーかどうか判定（純粋関数）
   */
  def isQuitKey(key: Int): Boolean =
    key == 'q' || key == 'Q'

  /**
   * 矢印キーコードをInputに変換（純粋関数）
   */
  def arrowToInput(key: Int): Option[Input] =
    arrowKeyMap.get(key.toChar)

  /**
   * エスケープシーケンスを解析（TtyService版）
   */
  def parseEscapeSequenceZIO(
    waitMs: Int = EscapeSequenceWaitMs
  ): ZIO[TtyService, Throwable, Option[Input]] =
    for
      _         <- TtyService.sleep(waitMs)
      available <- TtyService.available()
      result    <- if available <= 0 then ZIO.succeed(None)
                   else parseEscapeBody
    yield result

  private def parseEscapeBody: ZIO[TtyService, Throwable, Option[Input]] =
    for
      second    <- TtyService.read()
      result    <- if second != '[' then ZIO.succeed(None)
                   else parseArrowKey
    yield result

  private def parseArrowKey: ZIO[TtyService, Throwable, Option[Input]] =
    for
      _         <- TtyService.sleep(EscapeSequenceSecondWaitMs)
      available <- TtyService.available()
      result    <- if available <= 0 then ZIO.succeed(None)
                   else TtyService.read().map(key => arrowToInput(key))
    yield result

  /**
   * TtyServiceから1キーを読み取り（ZIO版）
   */
  def readKeyZIO: ZIO[TtyService, Throwable, ParseResult] =
    for
      available <- TtyService.available()
      result    <- if available <= 0 then ZIO.succeed(ParseResult.Timeout)
                   else readKeyBody
    yield result

  private def readKeyBody: ZIO[TtyService, Throwable, ParseResult] =
    for
      key    <- TtyService.read()
      result <- parseKeyResult(key)
    yield result

  private def parseKeyResult(key: Int): ZIO[TtyService, Throwable, ParseResult] =
    if key == EscapeKeyCode then
      parseEscapeSequenceZIO().map {
        case Some(input) => ParseResult.Arrow(input)
        case None        => ParseResult.Unknown
      }
    else
      ZIO.succeed(ParseResult.Regular(key))

  /**
   * ParseResultをOption[Input]に変換（純粋関数）
   */
  def toInput(result: ParseResult): Option[Input] =
    result match
      case ParseResult.Arrow(input) => Some(input)
      case ParseResult.Regular(key) => keyToInput(key)
      case ParseResult.Timeout      => None
      case ParseResult.Unknown      => None

/**
 * ターミナルのraw/cookedモード制御
 */
object TerminalControl:

  /**
   * ターミナルをrawモードに設定（CommandService版）
   */
  def enableRawMode: ZIO[CommandService, Throwable, Unit] =
    CommandService.exec("stty raw -echo < /dev/tty")

  /**
   * ターミナルをcookedモードに戻す（CommandService版）
   */
  def disableRawMode: ZIO[CommandService, Throwable, Unit] =
    CommandService.exec("stty cooked echo < /dev/tty")

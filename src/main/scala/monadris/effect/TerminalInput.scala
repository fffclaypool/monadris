package monadris.effect

import monadris.domain.Input
import java.io.{FileInputStream, InputStream}

/**
 * ターミナル入力のパース処理を集約
 * エスケープシーケンス解析を共通化し、ネストを削減
 */
object TerminalInput:

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
   * キーコードをInputに変換
   */
  def keyToInput(key: Int): Option[Input] =
    regularKeyMap.get(key.toChar)

  /**
   * 終了キーかどうか判定
   */
  def isQuitKey(key: Int): Boolean =
    key == 'q' || key == 'Q'

  /**
   * エスケープシーケンスを解析（InputStream版）
   * 矢印キー: ESC [ A/B/C/D
   */
  def parseEscapeSequence(in: InputStream, waitMs: Int = 20): Option[Input] =
    Thread.sleep(waitMs)
    val hasData = in.available() > 0
    if !hasData then return None

    val second = in.read()
    if second != '[' then return None

    Thread.sleep(5)
    if in.available() <= 0 then return None

    arrowKeyMap.get(in.read().toChar)

  /**
   * FileInputStreamから1キーを読み取り、Inputに変換
   */
  def readKey(in: FileInputStream): Option[ParseResult] =
    if in.available() <= 0 then return Some(ParseResult.Timeout)

    val key = in.read()
    if key == 27 then
      parseEscapeSequence(in) match
        case Some(input) => Some(ParseResult.Arrow(input))
        case None        => Some(ParseResult.Unknown)
    else
      Some(ParseResult.Regular(key))

  /**
   * ParseResultをOption[Input]に変換
   */
  def toInput(result: ParseResult): Option[Input] =
    result match
      case ParseResult.Arrow(input) => Some(input)
      case ParseResult.Regular(key) => keyToInput(key)
      case ParseResult.Timeout      => None
      case ParseResult.Unknown      => None

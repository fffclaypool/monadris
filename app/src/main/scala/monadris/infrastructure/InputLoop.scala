package monadris.infrastructure

import zio.*

import monadris.domain.Input
import monadris.domain.config.AppConfig
import monadris.domain.model.game.GameCommand

/**
 * キーボード入力を監視し、GameCommandをQueueにプッシュするコンポーネント
 * ブロッキングな入力読み取りをZIO Fiberで非同期化
 *
 * 責務:
 * - ターミナルからのキー入力を読み取り
 * - エスケープシーケンス（矢印キー等）の解析
 * - Input → GameCommand への変換
 * - Queueへのコマンドプッシュ
 */
object InputLoop:

  // ============================================================
  // キーコード定数
  // ============================================================

  /** Escapeキーコード（テスト用にパッケージ公開） */
  private[infrastructure] val EscapeKeyCode: Int = 27

  private val QuitKeys: Set[Int] = Set('q'.toInt, 'Q'.toInt)

  // ============================================================
  // キーマッピング
  // ============================================================

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

  // ============================================================
  // Input → GameCommand 変換 (Anti-Corruption Layer)
  // テスト用にパッケージ公開
  // ============================================================

  private[infrastructure] def translateToCommand(input: Input): Option[GameCommand] = input match
    case Input.MoveLeft               => Some(GameCommand.MoveLeft)
    case Input.MoveRight              => Some(GameCommand.MoveRight)
    case Input.MoveDown               => Some(GameCommand.SoftDrop)
    case Input.RotateClockwise        => Some(GameCommand.RotateCW)
    case Input.RotateCounterClockwise => Some(GameCommand.RotateCCW)
    case Input.HardDrop               => Some(GameCommand.HardDrop)
    case Input.Pause                  => Some(GameCommand.TogglePause)
    case Input.Tick                   => Some(GameCommand.Tick)
    case Input.Quit                   => None

  // ============================================================
  // キー解析（純粋関数）
  // テスト用にパッケージ公開
  // ============================================================

  private[infrastructure] def keyToInput(key: Int): Option[Input] =
    regularKeyMap.get(key.toChar)

  private[infrastructure] def isQuitKey(key: Int): Boolean =
    QuitKeys.contains(key)

  private[infrastructure] def arrowToInput(key: Int): Option[Input] =
    arrowKeyMap.get(key.toChar)

  // ============================================================
  // エスケープシーケンス解析
  // テスト用にパッケージ公開
  // ============================================================

  private[infrastructure] def parseEscapeSequenceZIO: ZIO[Terminal & AppConfig, Throwable, Option[Input]] =
    for
      config    <- ZIO.service[AppConfig]
      _         <- ZIO.sleep(config.terminal.escapeSequenceWaitMs.millis)
      available <- Terminal.available
      result <-
        if available <= 0 then ZIO.succeed(None)
        else parseEscapeBody
    yield result

  private def parseEscapeBody: ZIO[Terminal & AppConfig, Throwable, Option[Input]] =
    for
      second <- Terminal.read
      result <-
        if second != '[' then ZIO.succeed(None)
        else parseArrowKey
    yield result

  private def parseArrowKey: ZIO[Terminal & AppConfig, Throwable, Option[Input]] =
    for
      config    <- ZIO.service[AppConfig]
      _         <- ZIO.sleep(config.terminal.escapeSequenceSecondWaitMs.millis)
      available <- Terminal.available
      result <-
        if available <= 0 then ZIO.succeed(None)
        else Terminal.read.map(arrowToInput)
    yield result

  // ============================================================
  // キー読み取り
  // テスト用にパッケージ公開
  // ============================================================

  /** エスケープシーケンスの解析結果 */
  private[infrastructure] enum ParseResult:
    case Arrow(input: Input)
    case Regular(key: Int)
    case Timeout
    case Unknown

  private[infrastructure] def readKeyZIO: ZIO[Terminal & AppConfig, Throwable, ParseResult] =
    for
      available <- Terminal.available
      result <-
        if available <= 0 then ZIO.succeed(ParseResult.Timeout)
        else readKeyBody
    yield result

  private def readKeyBody: ZIO[Terminal & AppConfig, Throwable, ParseResult] =
    for
      key    <- Terminal.read
      result <- parseKeyResult(key)
    yield result

  private def parseKeyResult(key: Int): ZIO[Terminal & AppConfig, Throwable, ParseResult] =
    if key == EscapeKeyCode then
      parseEscapeSequenceZIO.map {
        case Some(input) => ParseResult.Arrow(input)
        case None        => ParseResult.Unknown
      }
    else ZIO.succeed(ParseResult.Regular(key))

  private[infrastructure] def toInput(result: ParseResult): Option[Input] =
    result match
      case ParseResult.Arrow(input) => Some(input)
      case ParseResult.Regular(key) => keyToInput(key)
      case ParseResult.Timeout      => None
      case ParseResult.Unknown      => None

  // ============================================================
  // プロデューサーループ
  // ============================================================

  /**
   * 入力を監視し続けるプロデューサー
   * Quitキーが押されるとQuitシグナルを返して終了
   *
   * @param queue コマンドを送信するQueue
   * @param quitSignal Quit要求を通知するPromise
   * @param pollIntervalMs ポーリング間隔（入力がない場合のsleep時間）
   */
  def producer(
    queue: Queue[GameCommand],
    quitSignal: Promise[Nothing, Unit],
    pollIntervalMs: Int
  ): ZIO[Terminal & AppConfig, Throwable, Unit] =
    val readAndOffer = for
      parseResult <- readKeyZIO
      shouldQuit <- parseResult match
        case ParseResult.Timeout =>
          ZIO.sleep(pollIntervalMs.millis).as(false)

        case ParseResult.Regular(key) if isQuitKey(key) =>
          ZIO.logInfo("Quit key pressed") *>
            quitSignal.succeed(()).as(true)

        case _ =>
          toInput(parseResult) match
            case Some(input) =>
              translateToCommand(input) match
                case Some(cmd) => queue.offer(cmd).as(false)
                case None      => quitSignal.succeed(()).as(true)
            case None => ZIO.succeed(false)
    yield shouldQuit

    readAndOffer.repeatUntil(identity).unit

  /**
   * 入力プロデューサーファイバーを起動
   * 返されたFiberはゲーム終了時にinterruptする必要がある
   */
  def start(
    queue: Queue[GameCommand],
    quitSignal: Promise[Nothing, Unit],
    pollIntervalMs: Int
  ): ZIO[Terminal & AppConfig, Nothing, Fiber.Runtime[Throwable, Unit]] =
    producer(queue, quitSignal, pollIntervalMs).fork

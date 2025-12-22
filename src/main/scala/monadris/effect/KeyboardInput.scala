package monadris.effect

import zio.*
import zio.stream.*
import org.jline.terminal.{Terminal, TerminalBuilder}
import org.jline.utils.NonBlockingReader
import monadris.domain.Input

/**
 * JLineを使用したキーボード入力ハンドラー
 * 副作用をZIOでラップし、コアロジックから分離
 */
object KeyboardInput:

  /**
   * ターミナルリソースを管理するスコープ
   */
  def terminalResource: ZIO[Scope, Throwable, Terminal] =
    ZIO.acquireRelease(
      ZIO.attempt {
        TerminalBuilder.builder()
          .system(true)
          .build()
      }
    )(terminal =>
      ZIO.succeed {
        terminal.close()
      }
    )

  /**
   * キー入力をInputに変換
   */
  def keyToInput(key: Int): Option[Input] =
    key match
      // 矢印キー（エスケープシーケンス処理済み）
      case 'h' | 'H' | 4   => Some(Input.MoveLeft)   // h or ←
      case 'l' | 'L' | 5   => Some(Input.MoveRight)  // l or →
      case 'j' | 'J' | 2   => Some(Input.MoveDown)   // j or ↓
      case 'k' | 'K' | 3   => Some(Input.RotateClockwise) // k or ↑
      case 'z' | 'Z'       => Some(Input.RotateCounterClockwise)
      case ' '             => Some(Input.HardDrop)
      case 'p' | 'P'       => Some(Input.Pause)
      case 'q' | 'Q'       => None  // 終了シグナル
      case _               => Some(Input.Tick)  // その他は無視（Tickとして扱う）

  /**
   * エスケープシーケンスを処理して矢印キーを検出
   */
  def readArrowKey(reader: NonBlockingReader): Int =
    val first = reader.read(50)  // タイムアウト付き読み取り
    if first == 27 then  // ESC
      val second = reader.read(50)
      if second == '[' then
        reader.read(50) match
          case 'A' => 3  // ↑
          case 'B' => 2  // ↓
          case 'C' => 5  // →
          case 'D' => 4  // ←
          case _   => first
      else first
    else first

  /**
   * キーボード入力のZStreamを生成
   * 終了時（q押下）はストリームを終了
   */
  def inputStream(terminal: Terminal): ZStream[Any, Nothing, Input] =
    ZStream.unwrapScoped {
      for
        _ <- ZIO.succeed(terminal.enterRawMode())
        reader = terminal.reader().asInstanceOf[NonBlockingReader]
      yield ZStream
        .repeatZIO(
          ZIO.attemptBlocking(readArrowKey(reader)).orDie
        )
        .takeWhile(key => key != 'q' && key != 'Q')
        .collect { case key if keyToInput(key).isDefined => keyToInput(key).get }
        .filter(_ != Input.Tick)  // 無効な入力は除外
    }

  /**
   * タイムアウト付きのキー入力読み取り
   * 入力がない場合はNoneを返す
   */
  def readInputWithTimeout(
    reader: NonBlockingReader,
    timeoutMs: Long
  ): ZIO[Any, Nothing, Option[Input]] =
    ZIO.attemptBlocking {
      val key = reader.read(timeoutMs)
      if key == -2 then None  // タイムアウト
      else if key == 27 then
        // エスケープシーケンス処理
        val second = reader.read(50)
        if second == '[' then
          reader.read(50) match
            case 'A' => keyToInput(3)   // ↑
            case 'B' => keyToInput(2)   // ↓
            case 'C' => keyToInput(5)   // →
            case 'D' => keyToInput(4)   // ←
            case _   => None
        else None
      else keyToInput(key)
    }.orDie


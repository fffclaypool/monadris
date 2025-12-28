package monadris.effect

import zio.*

/**
 * テスト専用のモック実装
 * 本番コードには含まれない
 */
object TestServices:

  // ============================================================
  // TtyService テスト実装 - キューベースの入力シミュレーション
  // ============================================================

  def tty(inputs: Chunk[Int]): ZLayer[Any, Nothing, TtyService] =
    ZLayer.fromZIO {
      for
        queue <- Queue.unbounded[Int]
        _     <- queue.offerAll(inputs)
      yield new TtyService:
        def available(): Task[Int] = queue.size
        def read(): Task[Int] = queue.take
        def sleep(ms: Long): Task[Unit] = ZIO.unit
    }

  // ============================================================
  // ConsoleService テスト実装 - バッファに蓄積
  // ============================================================

  case class TestConsoleService(buffer: Ref[List[String]]) extends ConsoleService:
    def print(text: String): Task[Unit] = buffer.update(_ :+ text)
    def flush(): Task[Unit] = ZIO.unit

  val console: ULayer[TestConsoleService] = ZLayer.fromZIO {
    for
      buffer <- Ref.make(List.empty[String])
    yield TestConsoleService(buffer)
  }

  // ============================================================
  // CommandService テスト実装 - コマンド履歴を記録
  // ============================================================

  case class TestCommandService(history: Ref[List[String]]) extends CommandService:
    def exec(cmd: String): Task[Unit] = history.update(_ :+ cmd)

  val command: ULayer[TestCommandService] = ZLayer.fromZIO {
    for
      history <- Ref.make(List.empty[String])
    yield TestCommandService(history)
  }

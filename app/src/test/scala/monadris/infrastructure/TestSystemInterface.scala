package monadris.infrastructure

import zio.*

import monadris.domain.config.*

/**
 * テスト専用のモック実装
 * 本番コードには含まれない
 */
object TestServices:

  // ============================================================
  // Terminal テスト実装 - キューベースの入力シミュレーション
  // ============================================================

  def terminal(inputs: Chunk[Int]): ZLayer[Any, Nothing, Terminal] =
    ZLayer.fromZIO {
      for
        queue <- Queue.unbounded[Int]
        _     <- queue.offerAll(inputs)
      yield new Terminal:
        def available: Task[Int] = queue.size
        def read: Task[Int]      = queue.take
    }

  // ============================================================
  // Console出力モック - バッファに蓄積（テスト検証用）
  // ============================================================

  case class TestConsoleOutput(buffer: Ref[List[String]]):
    def print(text: String): Task[Unit] = buffer.update(_ :+ text)
    def flush(): Task[Unit]             = ZIO.unit

  def consoleOutput: ULayer[TestConsoleOutput] = ZLayer.fromZIO {
    for buffer <- Ref.make(List.empty[String])
    yield TestConsoleOutput(buffer)
  }

  // ============================================================
  // Command実行モック - コマンド履歴を記録（テスト検証用）
  // ============================================================

  case class TestCommandHistory(history: Ref[List[String]]):
    def exec(cmd: String): Task[Unit] = history.update(_ :+ cmd)

  def commandHistory: ULayer[TestCommandHistory] = ZLayer.fromZIO {
    for history <- Ref.make(List.empty[String])
    yield TestCommandHistory(history)
  }

  // ============================================================
  // AppConfig テスト実装 - デフォルト値を提供
  // ============================================================

  val testConfig: AppConfig = AppConfig(
    grid = GridConfig(width = 10, height = 20),
    score = ScoreConfig(singleLine = 100, doubleLine = 300, tripleLine = 500, tetris = 800),
    level = LevelConfig(linesPerLevel = 10),
    speed = SpeedConfig(baseDropIntervalMs = 1000, minDropIntervalMs = 100, decreasePerLevelMs = 50),
    terminal = TerminalConfig(escapeSequenceWaitMs = 20, escapeSequenceSecondWaitMs = 5, inputPollIntervalMs = 20),
    timing = TimingConfig(titleDelayMs = 1000, outroDelayMs = 2000)
  )

  val config: ULayer[AppConfig] = ZLayer.succeed(testConfig)

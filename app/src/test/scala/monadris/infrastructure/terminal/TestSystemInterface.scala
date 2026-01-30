package monadris.infrastructure.terminal

import zio.*

import monadris.config.*

/**
 * テスト専用のモック実装
 * 本番コードには含まれない
 */
object TestServices:

  def tty(inputs: Chunk[Int]): ZLayer[Any, Nothing, TtyService] =
    ZLayer.fromZIO {
      for
        queue <- Queue.unbounded[Int]
        _     <- queue.offerAll(inputs)
      yield new TtyService:
        def available(): Task[Int]                                 = queue.size
        def read(): Task[Int]                                      = queue.take
        def sleep(ms: Long): Task[Unit]                            = ZIO.unit
        def readByteWithTimeout(timeoutMs: Int): Task[Option[Int]] =
          queue.poll.map(_.map(_.toInt))
    }

  case class TestConsoleService(buffer: Ref[List[String]]) extends ConsoleService:
    def print(text: String): Task[Unit] = buffer.update(_ :+ text)
    def flush(): Task[Unit]             = ZIO.unit

  val console: ULayer[TestConsoleService] = ZLayer.fromZIO {
    for buffer <- Ref.make(List.empty[String])
    yield TestConsoleService(buffer)
  }

  case class TestCommandService(history: Ref[List[String]]) extends CommandService:
    def exec(cmd: String): Task[Unit] = history.update(_ :+ cmd)

  val command: ULayer[TestCommandService] = ZLayer.fromZIO {
    for history <- Ref.make(List.empty[String])
    yield TestCommandService(history)
  }

  val testConfig: AppConfig = AppConfig(
    grid = GridConfig(width = 10, height = 20),
    score = ScoreConfig(singleLine = 100, doubleLine = 300, tripleLine = 500, tetris = 800),
    level = LevelConfig(linesPerLevel = 10),
    speed = SpeedConfig(baseDropIntervalMs = 1000, minDropIntervalMs = 100, decreasePerLevelMs = 50),
    terminal = TerminalConfig(escapeSequenceWaitMs = 20, escapeSequenceSecondWaitMs = 5, inputPollIntervalMs = 20),
    timing = TimingConfig(titleDelayMs = 1000, outroDelayMs = 2000),
    replay = ReplayConfig(defaultSpeed = 1.0, baseFrameIntervalMs = 50)
  )

  val config: ULayer[AppConfig] = ZLayer.succeed(testConfig)

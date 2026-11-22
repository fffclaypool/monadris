package monadris.infrastructure.terminal

import zio.*

import monadris.config.*

/**
 * Mock implementations for testing only.
 * Not included in production code.
 */
object TestServices:

  /** Hands out the queued keys; `sleep` decides how long a wait of the given length takes */
  final private class QueueTty(queue: Queue[Int], sleeping: Long => UIO[Unit]) extends TtyService:
    def available(): Task[Int]                                 = queue.size
    def read(): Task[Int]                                      = queue.take
    def sleep(ms: Long): Task[Unit]                            = sleeping(ms)
    def readByteWithTimeout(timeoutMs: Int): Task[Option[Int]] = queue.poll

  private def queueTty(inputs: Chunk[Int], sleeping: Long => UIO[Unit]): ULayer[TtyService] =
    ZLayer.fromZIO {
      for
        queue <- Queue.unbounded[Int]
        _     <- queue.offerAll(inputs)
      yield QueueTty(queue, sleeping)
    }

  /** Every wait returns at once, including the gravity timer, so time ticks arrive in a burst */
  def tty(inputs: Chunk[Int]): ULayer[TtyService] =
    queueTty(inputs, _ => ZIO.unit)

  /**
   * Key waits return at once but a wait as long as a drop interval never ends,
   * so the game only reacts to the typed keys and the outcome is deterministic.
   */
  def ttyWithoutGravity(inputs: Chunk[Int]): ULayer[TtyService] =
    queueTty(inputs, ms => if ms >= testConfig.speed.minDropIntervalMs then ZIO.never else ZIO.unit)

  private object Keys:
    val LineFeed       = 10
    val CarriageReturn = 13
    val Backspace      = 8
    val Delete         = 127

  /** Reads keys from a queue and keeps every shown message for assertions */
  final class TestTerminalSession(queue: Queue[Int], val messages: Ref[List[String]]) extends TerminalSession:
    def showMessage(message: String): Task[Unit] =
      messages.update(_ :+ message)

    def prompt(message: String): Task[String] =
      showMessage(message) *> readLine("")

    def waitForKeypress(): Task[Int] =
      queue.take

    def showMessageAndWait(message: String): Task[Unit] =
      showMessage(message) *> showMessage("Press any key to continue...") *> queue.take.unit

    def enableRawMode: Task[Unit]  = ZIO.unit
    def disableRawMode: Task[Unit] = ZIO.unit

    private def readLine(typed: String): Task[String] =
      queue.take.flatMap {
        case Keys.CarriageReturn | Keys.LineFeed => ZIO.succeed(typed)
        case Keys.Delete | Keys.Backspace        => readLine(typed.dropRight(1))
        case key                                 => readLine(typed + key.toChar)
      }

  def terminalSession(inputs: Chunk[Int]): ULayer[TestTerminalSession] =
    ZLayer.fromZIO {
      for
        queue    <- Queue.unbounded[Int]
        _        <- queue.offerAll(inputs)
        messages <- Ref.make(List.empty[String])
      yield TestTerminalSession(queue, messages)
    }

  case class TestConsoleService(buffer: Ref[List[String]]) extends ConsoleService:
    def print(text: String): Task[Unit] = buffer.update(_ :+ text)
    def flush(): Task[Unit]             = ZIO.unit

  val console: ULayer[TestConsoleService] = ZLayer.fromZIO {
    for buffer <- Ref.make(List.empty[String])
    yield TestConsoleService(buffer)
  }

  case class TestCommandService(history: Ref[List[String]]) extends CommandService:
    def exec(cmd: String): Task[Unit]          = history.update(_ :+ cmd)
    def execCapture(cmd: String): Task[String] = history.update(_ :+ cmd).as("")

  val command: ULayer[TestCommandService] = ZLayer.fromZIO {
    for history <- Ref.make(List.empty[String])
    yield TestCommandService(history)
  }

  val testConfig: AppConfig = AppConfig(
    grid = GridConfig(width = 10, height = 20),
    score = ScoreConfig(singleLine = 100, doubleLine = 300, tripleLine = 500, tetris = 800, hardDropPerCell = 2),
    level = LevelConfig(linesPerLevel = 10),
    speed = SpeedConfig(baseDropIntervalMs = 1000, minDropIntervalMs = 100, decreasePerLevelMs = 50),
    terminal = TerminalConfig(escapeSequenceWaitMs = 20, escapeSequenceSecondWaitMs = 5, inputPollIntervalMs = 20),
    timing = TimingConfig(titleDelayMs = 1000, outroDelayMs = 2000),
    replay = ReplayConfig(defaultSpeed = 1.0, storage = ReplayStorage.File),
    database = DatabaseConfig(
      url = "jdbc:postgresql://localhost:5432/test",
      username = "test",
      password = "test",
      poolSize = 5,
      connectionTimeoutMs = 30000
    )
  )

  val config: ULayer[AppConfig] = ZLayer.succeed(testConfig)

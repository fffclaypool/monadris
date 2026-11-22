package monadris

import zio.*
import zio.test.*

import monadris.infrastructure.persistence.FileReplayRepository
import monadris.infrastructure.persistence.ReplayError
import monadris.infrastructure.persistence.ReplayRepository
import monadris.infrastructure.terminal.TestServices as Mocks
import monadris.replay.ReplayData
import monadris.view.GameView

/**
 * Integration tests for Main.program.
 * Tests the entire application using TestServices mock implementations.
 *
 * PlayGame/PlayAndRecord are not driven from here: the game clock needs
 * TestClock.adjust and the input fiber competes with the menu for key input.
 * Those paths are covered by the GameSession/GameLoopRunner specs.
 */
object MainSpec extends ZIOSpecDefault:

  private object Keys:
    val Escape: Int = 27
    val Enter: Int  = '\r'.toInt
    val Quit: Int   = 'q'.toInt

    val WatchReplay: Int = '3'.toInt
    val ListReplays: Int = '4'.toInt
    val AnyKey: Int      = ' '.toInt
    val Down: Int        = 'j'.toInt

    val LeftArrow: Chunk[Int] = Chunk(Escape, '['.toInt, 'D'.toInt)

  private object Expected:
    val Title          = "Functional Tetris in Scala"
    val Controls       = "Controls"
    val Goodbye        = "Thanks for playing!"
    val NoReplays      = "No replays found."
    val MenuItems      = GameView.MenuItems.all.map(_.label)
    val StepsDownToEnd = MenuItems.size - 1

  private val programTimeout = 5.seconds

  /** What the program wrote to the screen and what it showed through the terminal session */
  final private case class Transcript(screen: String, messages: List[String])

  val mockReplayRepository: ULayer[ReplayRepository] = ZLayer.succeed {
    new ReplayRepository:
      def save(name: String, replay: ReplayData): IO[ReplayError, Unit] = ZIO.unit
      def load(name: String): IO[ReplayError, ReplayData]               = ZIO.fail(ReplayError.NotFound(name))
      def list: IO[ReplayError, Vector[String]]                         = ZIO.succeed(Vector.empty)
      def exists(name: String): IO[ReplayError, Boolean]                = ZIO.succeed(false)
      def delete(name: String): IO[ReplayError, Unit]                   = ZIO.unit
  }

  /** Runs the whole program against the given key presses; fails if it does not exit by itself */
  private def runProgram(keys: Chunk[Int]): Task[Transcript] =
    val run =
      for
        console  <- ZIO.service[Mocks.TestConsoleService]
        session  <- ZIO.service[Mocks.TestTerminalSession]
        _        <- Main.program.timeoutFail(new RuntimeException("program did not exit"))(programTimeout)
        screen   <- console.buffer.get
        messages <- session.messages.get
      yield Transcript(screen.mkString, messages)
    run.provide(
      Mocks.tty(keys),
      Mocks.console,
      Mocks.command,
      Mocks.config,
      Mocks.terminalSession(keys),
      mockReplayRepository
    )

  private def saysGoodbyeAfter(keys: Chunk[Int]): Task[TestResult] =
    runProgram(keys).map(transcript => assertTrue(transcript.screen.contains(Expected.Goodbye)))

  private def quittingSuite = suite("quitting")(
    test("exits with lowercase q") {
      saysGoodbyeAfter(Chunk(Keys.Quit))
    },
    test("exits with uppercase Q") {
      saysGoodbyeAfter(Chunk('Q'.toInt))
    },
    test("treats a lone escape key as quit") {
      saysGoodbyeAfter(Chunk(Keys.Escape))
    },
    test("ignores movement keys in the menu before quit") {
      saysGoodbyeAfter(Chunk('h'.toInt, 'l'.toInt, Keys.Down, Keys.Quit))
    },
    test("ignores an arrow key that does not move vertically") {
      saysGoodbyeAfter(Keys.LeftArrow :+ Keys.Quit)
    },
    test("ignores unknown keys and continues") {
      saysGoodbyeAfter(Chunk('x'.toInt, Keys.Quit))
    },
    test("exits when Quit is selected by navigating to it") {
      runProgram(Chunk.fill(Expected.StepsDownToEnd)(Keys.Down) :+ Keys.Enter).map { transcript =>
        assertTrue(transcript.screen.contains(Expected.MenuItems.last))
      }
    }
  )

  private def screenSuite = suite("screens")(
    test("shows the title screen with the controls") {
      runProgram(Chunk(Keys.Quit)).map { transcript =>
        assertTrue(transcript.screen.contains(Expected.Title), transcript.screen.contains(Expected.Controls))
      }
    },
    test("shows every menu item") {
      runProgram(Chunk(Keys.Quit)).map { transcript =>
        assertTrue(Expected.MenuItems.forall(transcript.screen.contains))
      }
    },
    test("WatchReplay reports that there are no replays and returns to the menu") {
      runProgram(Chunk(Keys.WatchReplay, Keys.AnyKey, Keys.Quit)).map { transcript =>
        assertTrue(
          transcript.messages.exists(_.contains(Expected.NoReplays)),
          transcript.screen.contains(Expected.Goodbye)
        )
      }
    },
    test("ListReplays reports that there are no replays and returns to the menu") {
      runProgram(Chunk(Keys.ListReplays, Keys.AnyKey, Keys.Quit)).map { transcript =>
        assertTrue(
          transcript.messages.exists(_.contains(Expected.NoReplays)),
          transcript.screen.contains(Expected.Goodbye)
        )
      }
    }
  )

  def spec = suite("Main Application")(
    test("replayRepositoryLayer provides file storage without a database when storage is file") {
      ZIO.serviceWith[ReplayRepository](repository => assertTrue(repository.isInstanceOf[FileReplayRepository]))
    }.provide(Mocks.config, Main.replayRepositoryLayer),
    quittingSuite,
    screenSuite
  )

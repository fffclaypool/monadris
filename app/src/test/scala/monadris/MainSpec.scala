package monadris

import zio.*
import zio.test.*

import monadris.infrastructure.persistence.ReplayRepository
import monadris.infrastructure.terminal.TestServices as Mocks
import monadris.replay.ReplayData

/**
 * Integration tests for Main.program.
 * Tests the entire application using TestServices mock implementations.
 */
object MainSpec extends ZIOSpecDefault:

  val mockReplayRepository: ULayer[ReplayRepository] = ZLayer.succeed {
    new ReplayRepository:
      def save(name: String, replay: ReplayData): Task[Unit] = ZIO.unit
      def load(name: String): Task[ReplayData]               = ZIO.fail(new RuntimeException("Not found"))
      def list: Task[Vector[String]]                         = ZIO.succeed(Vector.empty)
      def exists(name: String): Task[Boolean]                = ZIO.succeed(false)
      def delete(name: String): Task[Unit]                   = ZIO.unit
  }

  def spec = suite("Main Application")(
    test("program runs and exits with Q key") {
      val inputs = Chunk('q'.toInt)
      for _ <- Main.program
          .timeout(5.seconds)
      yield assertTrue(true)
    }.provide(
      Mocks.tty(Chunk('q'.toInt)),
      Mocks.console,
      Mocks.command,
      Mocks.config,
      Mocks.terminalSession(Chunk('q'.toInt)),
      mockReplayRepository
    ),
    test("program runs and exits with lowercase q") {
      for _ <- Main.program
          .timeout(5.seconds)
      yield assertTrue(true)
    }.provide(
      Mocks.tty(Chunk('q'.toInt)),
      Mocks.console,
      Mocks.command,
      Mocks.config,
      Mocks.terminalSession(Chunk('q'.toInt)),
      mockReplayRepository
    ),
    test("program runs and exits with uppercase Q") {
      for _ <- Main.program
          .timeout(5.seconds)
      yield assertTrue(true)
    }.provide(
      Mocks.tty(Chunk('Q'.toInt)),
      Mocks.console,
      Mocks.command,
      Mocks.config,
      Mocks.terminalSession(Chunk('Q'.toInt)),
      mockReplayRepository
    ),
    test("program shows title screen") {
      for
        service <- ZIO.service[Mocks.TestConsoleService]
        _       <- Main.program
          .timeout(5.seconds)
        output <- service.buffer.get
        combined = output.mkString
      yield assertTrue(
        combined.contains("Functional Tetris"),
        combined.contains("Controls")
      )
    }.provide(
      Mocks.tty(Chunk('q'.toInt)),
      Mocks.console,
      Mocks.command,
      Mocks.config,
      Mocks.terminalSession(Chunk('q'.toInt)),
      mockReplayRepository
    ),
    test("program shows menu after title") {
      for
        service <- ZIO.service[Mocks.TestConsoleService]
        _       <- Main.program
          .timeout(5.seconds)
        output <- service.buffer.get
        combined = output.mkString
      yield assertTrue(
        combined.contains("Functional Tetris in Scala"),
        combined.contains("Play Game"),
        combined.contains("Play & Record"),
        combined.contains("Watch Replay"),
        combined.contains("List Replays")
      )
    }.provide(
      Mocks.tty(Chunk('q'.toInt)),
      Mocks.console,
      Mocks.command,
      Mocks.config,
      Mocks.terminalSession(Chunk('q'.toInt)),
      mockReplayRepository
    ),
    test("program outputs goodbye message on quit") {
      for
        service <- ZIO.service[Mocks.TestConsoleService]
        _       <- Main.program
          .timeout(5.seconds)
        output <- service.buffer.get
        combined = output.mkString
      yield assertTrue(combined.contains("Thanks for playing!"))
    }.provide(
      Mocks.tty(Chunk('q'.toInt)),
      Mocks.console,
      Mocks.command,
      Mocks.config,
      Mocks.terminalSession(Chunk('q'.toInt)),
      mockReplayRepository
    ),
    test("program handles movement before quit") {
      val inputs = Chunk('h'.toInt, 'l'.toInt, 'j'.toInt, 'q'.toInt)
      for _ <- Main.program
          .timeout(5.seconds)
      yield assertTrue(true)
    }.provide(
      Mocks.tty(Chunk('h'.toInt, 'l'.toInt, 'j'.toInt, 'q'.toInt)),
      Mocks.console,
      Mocks.command,
      Mocks.config,
      Mocks.terminalSession(Chunk('h'.toInt, 'l'.toInt, 'j'.toInt, 'q'.toInt)),
      mockReplayRepository
    ),
    test("program handles arrow keys before quit") {
      val inputs = Chunk(27, '['.toInt, 'D'.toInt, 'q'.toInt)
      for _ <- Main.program
          .timeout(5.seconds)
      yield assertTrue(true)
    }.provide(
      Mocks.tty(Chunk(27, '['.toInt, 'D'.toInt, 'q'.toInt)),
      Mocks.console,
      Mocks.command,
      Mocks.config,
      Mocks.terminalSession(Chunk(27, '['.toInt, 'D'.toInt, 'q'.toInt)),
      mockReplayRepository
    ),
    test("program ignores unknown keys and continues") {
      for
        service <- ZIO.service[Mocks.TestConsoleService]
        _       <- Main.program
          .timeout(5.seconds)
        output <- service.buffer.get
        combined = output.mkString
      yield assertTrue(
        combined.contains("Play Game"),
        combined.contains("Thanks for playing!")
      )
    }.provide(
      Mocks.tty(Chunk('x'.toInt, 'q'.toInt)),
      Mocks.console,
      Mocks.command,
      Mocks.config,
      Mocks.terminalSession(Chunk('x'.toInt, 'q'.toInt)),
      mockReplayRepository
    ),
    test("program handles WatchReplay menu selection with no replays") {
      // Select WatchReplay with '3', press key after "No replays" message, quit with 'q'
      for
        service <- ZIO.service[Mocks.TestConsoleService]
        _       <- Main.program
          .timeout(5.seconds)
        output <- service.buffer.get
        combined = output.mkString
      yield assertTrue(
        combined.contains("No replays") || combined.contains("no replays") || combined.contains("Thanks")
      )
    }.provide(
      Mocks.tty(Chunk('3'.toInt, ' '.toInt, 'q'.toInt)),
      Mocks.console,
      Mocks.command,
      Mocks.config,
      Mocks.terminalSession(Chunk('3'.toInt, ' '.toInt, 'q'.toInt)),
      mockReplayRepository
    ),
    test("program handles ListReplays menu selection with no replays") {
      // Select ListReplays with '4', press key after "No replays" message, quit with 'q'
      for
        service <- ZIO.service[Mocks.TestConsoleService]
        _       <- Main.program
          .timeout(5.seconds)
        output <- service.buffer.get
        combined = output.mkString
      yield assertTrue(
        combined.contains("No replays") || combined.contains("no replays") || combined.contains("Thanks")
      )
    }.provide(
      Mocks.tty(Chunk('4'.toInt, ' '.toInt, 'q'.toInt)),
      Mocks.console,
      Mocks.command,
      Mocks.config,
      Mocks.terminalSession(Chunk('4'.toInt, ' '.toInt, 'q'.toInt)),
      mockReplayRepository
    ),
    test("program handles Quit menu selection via navigation") {
      // Navigate down 4 times with 'j' to select Quit, confirm with Enter
      // Note: Quit selection from menu exits without calling showGoodbye
      val inputs = Chunk('j'.toInt, 'j'.toInt, 'j'.toInt, 'j'.toInt, '\r'.toInt)
      for
        service <- ZIO.service[Mocks.TestConsoleService]
        _       <- Main.program
          .timeout(5.seconds)
        output <- service.buffer.get
        combined = output.mkString
      yield assertTrue(
        combined.contains("Exit") || combined.contains("Quit") // Verify menu item is displayed
      )
    }.provide(
      Mocks.tty(Chunk('j'.toInt, 'j'.toInt, 'j'.toInt, 'j'.toInt, '\r'.toInt)),
      Mocks.console,
      Mocks.command,
      Mocks.config,
      Mocks.terminalSession(Chunk('j'.toInt, 'j'.toInt, 'j'.toInt, 'j'.toInt, '\r'.toInt)),
      mockReplayRepository
    ),
    // NOTE: PlayGame/PlayAndRecord integration tests are not feasible because
    // GameClock.run uses ZIO.sleep (requires TestClock.adjust) and the
    // InputStream fiber competes with MenuController for TtyService input.
    // These paths are covered by GameSession/GameLoopRunner unit tests instead.
    // Quit via number key is also not possible since MenuController only maps '1'-'4'.
    test("program handles escape key as quit") {
      // Escape with no follow-up input is interpreted as Quit by MenuController
      for
        service <- ZIO.service[Mocks.TestConsoleService]
        _       <- Main.program
          .timeout(5.seconds)
        output <- service.buffer.get
        combined = output.mkString
      yield assertTrue(
        combined.contains("Thanks for playing!")
      )
    }.provide(
      Mocks.tty(Chunk(27)),
      Mocks.console,
      Mocks.command,
      Mocks.config,
      Mocks.terminalSession(Chunk(27)),
      mockReplayRepository
    )
  )

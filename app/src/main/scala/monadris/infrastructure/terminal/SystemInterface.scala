package monadris.infrastructure.terminal

import java.io.FileInputStream
import java.io.InputStream
import java.nio.charset.StandardCharsets

import zio.*

import monadris.config.AppConfig
import monadris.config.ConfigLayer

trait TtyService:
  def available(): Task[Int]
  def read(): Task[Int]
  def sleep(ms: Long): Task[Unit]
  def readByteWithTimeout(timeoutMs: Int): Task[Option[Int]]

object TtyService:
  private object Polling:
    /** How often the terminal is checked for input while waiting with a timeout */
    val IntervalMs: Int = 10

  private object Device:
    val Path = "/dev/tty"

  def available(): ZIO[TtyService, Throwable, Int] =
    ZIO.serviceWithZIO(_.available())

  def read(): ZIO[TtyService, Throwable, Int] =
    ZIO.serviceWithZIO(_.read())

  def sleep(ms: Long): ZIO[TtyService, Throwable, Unit] =
    ZIO.serviceWithZIO(_.sleep(ms))

  def readByteWithTimeout(timeoutMs: Int): ZIO[TtyService, Throwable, Option[Int]] =
    ZIO.serviceWithZIO(_.readByteWithTimeout(timeoutMs))

  /** Opens the given device (or file) for the lifetime of the scope */
  def open(path: String): ZIO[Scope, Throwable, TtyService] =
    ZIO
      .acquireRelease(ZIO.attempt(new FileInputStream(path)))(stream => ZIO.succeed(stream.close()))
      .map(fromInputStream)

  def fromInputStream(input: InputStream): TtyService = StreamTty(input)

  private val openTerminal: ZIO[Scope, Nothing, TtyService] = open(Device.Path).orDie

  val live: ZLayer[Any, Nothing, TtyService] = ZLayer.scoped(openTerminal)

  final private class StreamTty(input: InputStream) extends TtyService:
    def available(): Task[Int]      = ZIO.attemptBlocking(input.available())
    def read(): Task[Int]           = ZIO.attemptBlocking(input.read())
    def sleep(ms: Long): Task[Unit] = ZIO.sleep(ms.millis)

    def readByteWithTimeout(timeoutMs: Int): Task[Option[Int]] =
      pollForInput(timeoutMs)

    /** Checks for input every polling interval until something arrives or the time is up */
    private def pollForInput(remainingMs: Int): Task[Option[Int]] =
      if remainingMs <= 0 then ZIO.succeed(None)
      else
        available().flatMap { avail =>
          if avail > 0 then read().map(Some(_))
          else sleep(Polling.IntervalMs) *> pollForInput(remainingMs - Polling.IntervalMs)
        }

trait ConsoleService:
  def print(text: String): Task[Unit]
  def flush(): Task[Unit]

object ConsoleService:
  def print(text: String): ZIO[ConsoleService, Throwable, Unit] =
    ZIO.serviceWithZIO(_.print(text))

  def flush(): ZIO[ConsoleService, Throwable, Unit] =
    ZIO.serviceWithZIO(_.flush())

  val live: ZLayer[Any, Nothing, ConsoleService] = ZLayer.succeed {
    new ConsoleService:
      def print(text: String): Task[Unit] = ZIO.attempt(scala.Console.print(text))
      def flush(): Task[Unit]             = ZIO.attempt(java.lang.System.out.flush())
  }

trait CommandService:
  /** Runs a shell command; fails if it exits with a non-zero status */
  def exec(cmd: String): Task[Unit]

  /** Runs a shell command and returns its standard output */
  def execCapture(cmd: String): Task[String]

object CommandService:
  private object Shell:
    val Path        = "/bin/sh"
    val CommandFlag = "-c"
    val SuccessCode = 0

  def exec(cmd: String): ZIO[CommandService, Throwable, Unit] =
    ZIO.serviceWithZIO(_.exec(cmd))

  def execCapture(cmd: String): ZIO[CommandService, Throwable, String] =
    ZIO.serviceWithZIO(_.execCapture(cmd))

  val live: ZLayer[Any, Nothing, CommandService] = ZLayer.succeed {
    new CommandService:
      def exec(cmd: String): Task[Unit] = execCapture(cmd).unit

      def execCapture(cmd: String): Task[String] =
        ZIO
          .attemptBlocking {
            val process = new ProcessBuilder(Shell.Path, Shell.CommandFlag, cmd)
              .redirectError(ProcessBuilder.Redirect.DISCARD)
              .start()
            val output = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
            (process.waitFor(), output)
          }
          .flatMap { case (exitCode, output) =>
            if exitCode == Shell.SuccessCode then ZIO.succeed(output)
            else ZIO.fail(new RuntimeException(s"Command failed with exit code $exitCode: $cmd"))
          }
  }

type GameEnv = TtyService & ConsoleService & CommandService & AppConfig & TerminalSession

object GameEnv:
  private val terminalServices: ZLayer[Any, Nothing, TtyService & ConsoleService & CommandService] =
    TtyService.live ++ ConsoleService.live ++ CommandService.live

  val live: ZLayer[Any, Config.Error, GameEnv] =
    terminalServices ++ ConfigLayer.live ++ (terminalServices >>> TerminalSession.live)

package monadris.infrastructure.terminal

import java.io.FileInputStream

import zio.*

import monadris.config.AppConfig
import monadris.config.ConfigLayer

trait TtyService:
  def available(): Task[Int]
  def read(): Task[Int]
  def sleep(ms: Long): Task[Unit]
  def readByteWithTimeout(timeoutMs: Int): Task[Option[Int]]

object TtyService:
  def available(): ZIO[TtyService, Throwable, Int] =
    ZIO.serviceWithZIO(_.available())

  def read(): ZIO[TtyService, Throwable, Int] =
    ZIO.serviceWithZIO(_.read())

  def sleep(ms: Long): ZIO[TtyService, Throwable, Unit] =
    ZIO.serviceWithZIO(_.sleep(ms))

  def readByteWithTimeout(timeoutMs: Int): ZIO[TtyService, Throwable, Option[Int]] =
    ZIO.serviceWithZIO(_.readByteWithTimeout(timeoutMs))

  val live: ZLayer[Any, Nothing, TtyService] = ZLayer.scoped {
    for tty <- ZIO.acquireRelease(
        ZIO.attempt(new FileInputStream("/dev/tty")).orDie
      )(fis => ZIO.succeed(fis.close()))
    yield new TtyService:
      def available(): Task[Int]                                 = ZIO.attemptBlocking(tty.available())
      def read(): Task[Int]                                      = ZIO.attemptBlocking(tty.read())
      def sleep(ms: Long): Task[Unit]                            = ZIO.sleep(ms.millis)
      def readByteWithTimeout(timeoutMs: Int): Task[Option[Int]] =
        pollForInput(timeoutMs)

      private def pollForInput(remainingMs: Int): Task[Option[Int]] =
        if remainingMs <= 0 then ZIO.succeed(None)
        else
          available().flatMap { avail =>
            if avail > 0 then read().map(Some(_))
            else
              val pollInterval = 10
              sleep(pollInterval) *> pollForInput(remainingMs - pollInterval)
          }
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
  def exec(cmd: String): Task[Unit]

object CommandService:
  def exec(cmd: String): ZIO[CommandService, Throwable, Unit] =
    ZIO.serviceWithZIO(_.exec(cmd))

  val live: ZLayer[Any, Nothing, CommandService] = ZLayer.succeed {
    new CommandService:
      def exec(cmd: String): Task[Unit] = ZIO.attempt {
        java.lang.Runtime.getRuntime.exec(Array("/bin/sh", "-c", cmd)).waitFor()
      }.unit
  }

type GameEnv = TtyService & ConsoleService & CommandService & AppConfig & TerminalSession

object GameEnv:
  private val terminalServices: ZLayer[Any, Nothing, TtyService & ConsoleService & CommandService] =
    TtyService.live ++ ConsoleService.live ++ CommandService.live

  val live: ZLayer[Any, Config.Error, GameEnv] =
    terminalServices ++ ConfigLayer.live ++ (terminalServices >>> TerminalSession.live)

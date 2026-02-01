package monadris.infrastructure.terminal

import zio.*

trait TerminalSession:
  def showMessage(message: String): Task[Unit]
  def prompt(message: String): Task[String]
  def waitForKeypress(): Task[Int]
  def showMessageAndWait(message: String): Task[Unit]
  def withRawMode[A](effect: Task[A]): Task[A]
  def withCooked[A](effect: Task[A]): Task[A]
  def enableRawMode: Task[Unit]
  def disableRawMode: Task[Unit]

object TerminalSession:

  def showMessage(message: String): ZIO[TerminalSession, Throwable, Unit] =
    ZIO.serviceWithZIO(_.showMessage(message))

  def prompt(message: String): ZIO[TerminalSession, Throwable, String] =
    ZIO.serviceWithZIO(_.prompt(message))

  def waitForKeypress(): ZIO[TerminalSession, Throwable, Int] =
    ZIO.serviceWithZIO(_.waitForKeypress())

  def showMessageAndWait(message: String): ZIO[TerminalSession, Throwable, Unit] =
    ZIO.serviceWithZIO(_.showMessageAndWait(message))

  def withRawMode[R <: TerminalSession, A](effect: ZIO[R, Throwable, A]): ZIO[R, Throwable, A] =
    ZIO.serviceWithZIO[TerminalSession](session =>
      ZIO.acquireReleaseWith(session.enableRawMode)(_ => session.disableRawMode.ignore)(_ => effect)
    )

  def withCooked[R <: TerminalSession, A](effect: ZIO[R, Throwable, A]): ZIO[R, Throwable, A] =
    ZIO.serviceWithZIO[TerminalSession](session =>
      ZIO.acquireReleaseWith(session.disableRawMode)(_ => session.enableRawMode.ignore)(_ => effect)
    )

  def enableRawMode: ZIO[TerminalSession, Throwable, Unit] =
    ZIO.serviceWithZIO(_.enableRawMode)

  def disableRawMode: ZIO[TerminalSession, Throwable, Unit] =
    ZIO.serviceWithZIO(_.disableRawMode)

  val live: ZLayer[TtyService & ConsoleService & CommandService, Nothing, TerminalSession] =
    ZLayer.fromFunction { (tty: TtyService, console: ConsoleService, cmd: CommandService) =>
      new TerminalSession:
        def showMessage(message: String): Task[Unit] =
          for
            _ <- cmd.exec("stty cooked echo < /dev/tty")
            _ <- console.print(message)
            _ <- cmd.exec("stty raw -echo < /dev/tty")
          yield ()

        def prompt(message: String): Task[String] =
          for
            _    <- cmd.exec("stty cooked echo < /dev/tty")
            _    <- console.print(message)
            line <- readLineImpl
            _    <- cmd.exec("stty raw -echo < /dev/tty")
          yield line

        def waitForKeypress(): Task[Int] =
          tty.read()

        def showMessageAndWait(message: String): Task[Unit] =
          for
            _ <- cmd.exec("stty cooked echo < /dev/tty")
            _ <- console.print(message)
            _ <- console.print("Press any key to continue...")
            _ <- cmd.exec("stty raw -echo < /dev/tty")
            _ <- tty.read()
          yield ()

        def withRawMode[A](effect: Task[A]): Task[A] =
          ZIO.acquireReleaseWith(
            cmd.exec("stty raw -echo < /dev/tty")
          )(_ => cmd.exec("stty cooked echo < /dev/tty").ignore)(_ => effect)

        def withCooked[A](effect: Task[A]): Task[A] =
          ZIO.acquireReleaseWith(
            cmd.exec("stty cooked echo < /dev/tty")
          )(_ => cmd.exec("stty raw -echo < /dev/tty").ignore)(_ => effect)

        def enableRawMode: Task[Unit] =
          cmd.exec("stty raw -echo < /dev/tty")

        def disableRawMode: Task[Unit] =
          cmd.exec("stty cooked echo < /dev/tty")

        private def readLineImpl: Task[String] =
          loop(new StringBuilder)

        private object Keys:
          val Enter: Int       = '\r'.toInt
          val LineFeed: Int    = '\n'.toInt
          val Backspace: Int   = 127
          val BackspaceCR: Int = 8

        private def loop(acc: StringBuilder): Task[String] =
          tty.read().flatMap { ch =>
            ch match
              case Keys.Enter | Keys.LineFeed =>
                ZIO.succeed(acc.toString)
              case Keys.Backspace | Keys.BackspaceCR =>
                if acc.nonEmpty then
                  for
                    _      <- console.print("\b \b")
                    result <- loop(acc.deleteCharAt(acc.length - 1))
                  yield result
                else loop(acc)
              case _ =>
                for
                  _      <- console.print(ch.toChar.toString)
                  result <- loop(acc.append(ch.toChar))
                yield result
          }
    }

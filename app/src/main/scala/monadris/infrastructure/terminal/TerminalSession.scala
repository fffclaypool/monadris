package monadris.infrastructure.terminal

import java.nio.charset.StandardCharsets

import zio.*

import monadris.view.AnsiRenderer

/**
 * Owns the terminal mode for the lifetime of the application.
 *
 * The game runs in raw mode. `prompt` temporarily hands the terminal back to its
 * original line mode so that echo, backspace and multi-byte input are handled by
 * the terminal driver itself.
 */
trait TerminalSession:
  def showMessage(message: String): Task[Unit]
  def prompt(message: String): Task[String]
  def waitForKeypress(): Task[Int]
  def showMessageAndWait(message: String): Task[Unit]
  def enableRawMode: Task[Unit]
  def disableRawMode: Task[Unit]

object TerminalSession:

  private[terminal] object Stty:
    private val Device = "/dev/tty"

    val SaveState: String = s"stty -g < $Device"
    val Raw: String       = s"stty raw -echo < $Device"

    /** Fallback when the original settings could not be captured */
    val Cooked: String = s"stty cooked echo < $Device"

    def restore(savedState: String): String = s"stty $savedState < $Device"

    /** Output format of `stty -g` (GNU: "4500:5:bf:...", BSD: "gfmt1:cflag=4b00:...") */
    def isValidState(state: String): Boolean =
      state.nonEmpty && state.forall(c => Character.isLetterOrDigit(c) || c == ':' || c == '=')

  private object Keys:
    val Enter: Int       = '\r'.toInt
    val LineFeed: Int    = '\n'.toInt
    val EndOfStream: Int = -1

  private object Messages:
    val PressAnyKey = "Press any key to continue..."

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

  def enableRawMode: ZIO[TerminalSession, Throwable, Unit] =
    ZIO.serviceWithZIO(_.enableRawMode)

  def disableRawMode: ZIO[TerminalSession, Throwable, Unit] =
    ZIO.serviceWithZIO(_.disableRawMode)

  val live: ZLayer[TtyService & ConsoleService & CommandService, Nothing, TerminalSession] =
    ZLayer.fromZIO {
      for
        tty        <- ZIO.service[TtyService]
        console    <- ZIO.service[ConsoleService]
        cmd        <- ZIO.service[CommandService]
        savedState <- Ref.make(Option.empty[String])
      yield Live(tty, console, cmd, savedState)
    }

  // A named class rather than an anonymous one: coverage reports merge all anonymous
  // classes of a package into a single "$anon" entry and misattribute their lines.
  final private class Live(
    tty: TtyService,
    console: ConsoleService,
    cmd: CommandService,
    savedState: Ref[Option[String]]
  ) extends TerminalSession:
    def showMessage(message: String): Task[Unit] =
      console.print(message) *> console.flush()

    def prompt(message: String): Task[String] =
      ZIO.acquireReleaseWith(disableRawMode)(_ => enableRawMode.ignore) { _ =>
        showMessage(message) *> readLine(Chunk.empty)
      }

    def waitForKeypress(): Task[Int] =
      tty.read()

    def showMessageAndWait(message: String): Task[Unit] =
      showMessage(message + Messages.PressAnyKey) *> tty.read().unit

    /** Remembers the settings found on first use so they can be restored exactly. */
    def enableRawMode: Task[Unit] =
      for
        current <- cmd.execCapture(Stty.SaveState).map(_.trim).option
        _       <- savedState.update(_.orElse(current.filter(Stty.isValidState)))
        _       <- cmd.exec(Stty.Raw)
      yield ()

    def disableRawMode: Task[Unit] =
      for
        saved <- savedState.get
        _     <- cmd.exec(saved.fold(Stty.Cooked)(Stty.restore))
        _     <- console.print(AnsiRenderer.Codes.ShowCursor)
        _     <- console.flush()
      yield ()

    /** Reads raw bytes up to the end of the line and decodes them as UTF-8. */
    private def readLine(acc: Chunk[Byte]): Task[String] =
      tty.read().flatMap {
        case Keys.Enter | Keys.LineFeed | Keys.EndOfStream =>
          ZIO.succeed(new String(acc.toArray, StandardCharsets.UTF_8))
        case byte =>
          readLine(acc :+ byte.toByte)
      }

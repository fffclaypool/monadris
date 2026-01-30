package monadris.infrastructure.terminal

import zio.*

object LineReader:

  private object Keys:
    val Enter: Int       = '\r'.toInt
    val LineFeed: Int    = '\n'.toInt
    val Backspace: Int   = 127
    val BackspaceCR: Int = 8

  def readLine: ZIO[TtyService & ConsoleService, Throwable, String] =
    loop(new StringBuilder)

  private def loop(acc: StringBuilder): ZIO[TtyService & ConsoleService, Throwable, String] =
    TtyService.read().flatMap { ch =>
      ch match
        case Keys.Enter | Keys.LineFeed =>
          ZIO.succeed(acc.toString)
        case Keys.Backspace | Keys.BackspaceCR =>
          if acc.nonEmpty then
            for
              _      <- ConsoleService.print("\b \b")
              result <- loop(acc.deleteCharAt(acc.length - 1))
            yield result
          else loop(acc)
        case _ =>
          for
            _      <- ConsoleService.print(ch.toChar.toString)
            result <- loop(acc.append(ch.toChar))
          yield result
    }

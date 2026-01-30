package monadris.infrastructure.terminal

import zio.*

import monadris.view.AnsiRenderer
import monadris.view.ScreenBuffer

object ConsoleRenderer:

  export AnsiRenderer.bufferToString

  def render(buffer: ScreenBuffer): ZIO[ConsoleService, Throwable, Unit] =
    for
      _ <- ConsoleService.print(
        s"${AnsiRenderer.Codes.HideCursor}${AnsiRenderer.Codes.Home}${AnsiRenderer.Codes.ClearScreen}"
      )
      _ <- ConsoleService.print(AnsiRenderer.bufferToString(buffer))
      _ <- ConsoleService.print(AnsiRenderer.Codes.Newline)
      _ <- ConsoleService.flush()
    yield ()

  def render(current: ScreenBuffer, previous: Option[ScreenBuffer]): ZIO[ConsoleService, Throwable, Unit] =
    previous match
      case None       => render(current)
      case Some(prev) => renderDiff(current, prev)

  private def renderDiff(current: ScreenBuffer, previous: ScreenBuffer): ZIO[ConsoleService, Throwable, Unit] =
    val diffString = AnsiRenderer.computeDiffString(current, previous)
    if diffString.isEmpty then ZIO.unit
    else
      for
        _ <- ConsoleService.print(AnsiRenderer.Codes.HideCursor + diffString)
        _ <- ConsoleService.flush()
      yield ()

  def renderWithoutClear(buffer: ScreenBuffer): ZIO[ConsoleService, Throwable, Unit] =
    for
      _ <- ConsoleService.print(AnsiRenderer.Codes.HideCursor)
      _ <- ConsoleService.print(AnsiRenderer.bufferToString(buffer))
      _ <- ConsoleService.print(AnsiRenderer.Codes.Newline)
      _ <- ConsoleService.flush()
    yield ()

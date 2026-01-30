package monadris.infrastructure.game

import zio.*

import monadris.config.AppConfig
import monadris.infrastructure.persistence.ReplayRepository
import monadris.infrastructure.terminal.ConsoleService
import monadris.infrastructure.terminal.GameEnv
import monadris.infrastructure.terminal.LineReader
import monadris.infrastructure.terminal.Renderer
import monadris.infrastructure.terminal.TerminalControl
import monadris.infrastructure.terminal.TtyService

object ReplaySelector:

  def watchReplay: ZIO[GameEnv & ReplayRepository, Throwable, Unit] =
    for
      replays <- ReplayRepository.list
      _       <-
        if replays.isEmpty then showNoReplaysMessage
        else selectAndPlayReplay(replays)
    yield ()

  def listReplays: ZIO[GameEnv & ReplayRepository, Throwable, Unit] =
    for
      replays <- ReplayRepository.list
      _       <-
        if replays.isEmpty then showNoReplaysMessage
        else printReplayList(replays)
    yield ()

  private def showNoReplaysMessage: ZIO[GameEnv, Throwable, Unit] =
    for
      _ <- TerminalControl.disableRawMode
      _ <- ConsoleService.print("\r\n\r\nNo replays found.\r\n")
      _ <- ConsoleService.print("Press any key to continue...")
      _ <- TerminalControl.enableRawMode
      _ <- TtyService.read()
    yield ()

  private def selectAndPlayReplay(replays: Vector[String]): ZIO[GameEnv & ReplayRepository, Throwable, Unit] =
    for
      _ <- TerminalControl.disableRawMode
      _ <- ConsoleService.print("\r\n\r\nAvailable replays:\r\n")
      _ <- ZIO.foreach(replays.zipWithIndex) { case (name, idx) =>
        ConsoleService.print(s"  ${idx + 1}. $name\r\n")
      }
      _     <- ConsoleService.print("\r\nEnter replay number (or 0 to cancel): ")
      input <- LineReader.readLine
      _     <- TerminalControl.enableRawMode
      _     <- input.toIntOption match
        case Some(n) if n > 0 && n <= replays.size =>
          playReplay(replays(n - 1))
        case Some(0) => ZIO.unit
        case _       => showInvalidSelection
    yield ()

  private def playReplay(name: String): ZIO[GameEnv & ReplayRepository, Throwable, Unit] =
    for
      _      <- TerminalControl.disableRawMode
      _      <- ConsoleService.print(s"\r\nLoading replay: $name\r\n")
      _      <- TerminalControl.enableRawMode
      replay <- ReplayRepository.load(name)
      config <- ZIO.service[AppConfig]
      _      <- ReplayRunner.run(replay, config, Renderer.live)
      _      <- TtyService.read()
    yield ()

  private def showInvalidSelection: ZIO[GameEnv, Throwable, Unit] =
    for
      _ <- TerminalControl.disableRawMode
      _ <- ConsoleService.print("\r\nInvalid selection.\r\n")
      _ <- ConsoleService.print("Press any key to continue...")
      _ <- TerminalControl.enableRawMode
      _ <- TtyService.read()
    yield ()

  private def printReplayList(replays: Vector[String]): ZIO[GameEnv, Throwable, Unit] =
    for
      _ <- TerminalControl.disableRawMode
      _ <- ConsoleService.print("\r\n\r\nSaved replays:\r\n")
      _ <- ZIO.foreach(replays)(name => ConsoleService.print(s"  - $name\r\n"))
      _ <- ConsoleService.print("\r\nPress any key to continue...")
      _ <- TerminalControl.enableRawMode
      _ <- TtyService.read()
    yield ()

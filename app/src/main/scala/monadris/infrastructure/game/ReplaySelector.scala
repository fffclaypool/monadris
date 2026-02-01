package monadris.infrastructure.game

import zio.*

import monadris.config.AppConfig
import monadris.infrastructure.persistence.ReplayRepository
import monadris.infrastructure.terminal.GameEnv
import monadris.infrastructure.terminal.Renderer
import monadris.infrastructure.terminal.TerminalSession

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
    TerminalSession.showMessageAndWait("\r\n\r\nNo replays found.\r\n")

  private def selectAndPlayReplay(replays: Vector[String]): ZIO[GameEnv & ReplayRepository, Throwable, Unit] =
    for
      replayList <- ZIO.succeed(formatReplayList(replays))
      input      <- TerminalSession.prompt(
        s"\r\n\r\nAvailable replays:\r\n$replayList\r\nEnter replay number (or 0 to cancel): "
      )
      _ <- input.toIntOption match
        case Some(n) if n > 0 && n <= replays.size =>
          playReplay(replays(n - 1))
        case Some(0) => ZIO.unit
        case _       => showInvalidSelection
    yield ()

  private def formatReplayList(replays: Vector[String]): String =
    replays.zipWithIndex.map { case (name, idx) =>
      s"  ${idx + 1}. $name\r\n"
    }.mkString

  private def playReplay(name: String): ZIO[GameEnv & ReplayRepository, Throwable, Unit] =
    for
      _      <- TerminalSession.showMessage(s"\r\nLoading replay: $name\r\n")
      replay <- ReplayRepository.load(name)
      config <- ZIO.service[AppConfig]
      _      <- ReplayRunner.run(replay, config, Renderer.live)
      _      <- TerminalSession.waitForKeypress()
    yield ()

  private def showInvalidSelection: ZIO[GameEnv, Throwable, Unit] =
    TerminalSession.showMessageAndWait("\r\nInvalid selection.\r\n")

  private def printReplayList(replays: Vector[String]): ZIO[GameEnv, Throwable, Unit] =
    val replayList = replays.map(name => s"  - $name\r\n").mkString
    TerminalSession.showMessageAndWait(s"\r\n\r\nSaved replays:\r\n$replayList\r\n")

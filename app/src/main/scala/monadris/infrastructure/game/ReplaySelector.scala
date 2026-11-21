package monadris.infrastructure.game

import zio.*

import monadris.config.AppConfig
import monadris.infrastructure.persistence.ReplayRepository
import monadris.infrastructure.terminal.GameEnv
import monadris.infrastructure.terminal.Renderer
import monadris.infrastructure.terminal.TerminalSession
import monadris.replay.ReplayData

object ReplaySelector:

  def watchReplay: ZIO[GameEnv & ReplayRepository, Throwable, Unit] =
    withReplays(selectAndPlayReplay)

  def listReplays: ZIO[GameEnv & ReplayRepository, Throwable, Unit] =
    withReplays(printReplayList)

  private def withReplays(
    use: Vector[String] => ZIO[GameEnv & ReplayRepository, Throwable, Unit]
  ): ZIO[GameEnv & ReplayRepository, Throwable, Unit] =
    ReplayRepository.list.either.flatMap {
      case Left(error)                       => StorageErrors.report("list replays", error)
      case Right(replays) if replays.isEmpty => showNoReplaysMessage
      case Right(replays)                    => use(replays)
    }

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
      loaded <- ReplayRepository.load(name).either
      _      <- loaded match
        case Left(error)   => StorageErrors.report("load the replay", error)
        case Right(replay) => runReplay(replay)
    yield ()

  private def runReplay(replay: ReplayData): ZIO[GameEnv, Throwable, Unit] =
    for
      config <- ZIO.service[AppConfig]
      _      <- ReplayRunner.run(replay, config, Renderer.live)
      _      <- TerminalSession.waitForKeypress()
    yield ()

  private def showInvalidSelection: ZIO[GameEnv, Throwable, Unit] =
    TerminalSession.showMessageAndWait("\r\nInvalid selection.\r\n")

  private def printReplayList(replays: Vector[String]): ZIO[GameEnv, Throwable, Unit] =
    val replayList = replays.map(name => s"  - $name\r\n").mkString
    TerminalSession.showMessageAndWait(s"\r\n\r\nSaved replays:\r\n$replayList\r\n")

package monadris.infrastructure.game

import zio.*

import monadris.config.AppConfig
import monadris.domain.*
import monadris.infrastructure.persistence.ReplayRepository
import monadris.infrastructure.terminal.ConsoleService
import monadris.infrastructure.terminal.GameEnv
import monadris.infrastructure.terminal.LineReader
import monadris.infrastructure.terminal.Renderer
import monadris.infrastructure.terminal.TerminalControl
import monadris.infrastructure.terminal.TtyService

object GameSession:

  def showIntro: ZIO[GameEnv, Throwable, Unit] =
    for
      config <- ZIO.service[AppConfig]
      _      <- ZIO.logInfo("Monadris starting...")
      _      <- GameRunner.showTitle
      _      <- TtyService.sleep(config.timing.titleDelayMs)
    yield ()

  def playGame: ZIO[GameEnv, Throwable, Unit] =
    for
      initialState <- initializeGame
      finalState   <- GameRunner.interactiveGameLoop(initialState)
      _            <- showOutro(finalState)
    yield ()

  def playAndRecord: ZIO[GameEnv & ReplayRepository, Throwable, Unit] =
    for
      _            <- TerminalControl.disableRawMode
      _            <- ConsoleService.print("\r\n\r\nEnter replay name: ")
      name         <- LineReader.readLine
      _            <- TerminalControl.enableRawMode
      initialState <- initializeGame
      config       <- ZIO.service[AppConfig]
      result       <- RecordingGameLoopRunner.recordingGameLoop(
        initialState,
        config,
        Renderer.live,
        GameRunner.RandomPieceGenerator
      )
      (finalState, replayData) = result
      _ <- ReplayRepository.save(name, replayData)
      _ <- showOutro(finalState)
      _ <- TerminalControl.disableRawMode
      _ <- ConsoleService.print(s"\r\nReplay saved as: $name\r\n")
      _ <- ConsoleService.print("Press any key to continue...")
      _ <- TerminalControl.enableRawMode
      _ <- TtyService.read()
    yield ()

  private def initializeGame: ZIO[AppConfig, Nothing, GameState] =
    for
      config     <- ZIO.service[AppConfig]
      firstShape <- GameRunner.RandomPieceGenerator.nextShape
      nextShape  <- GameRunner.RandomPieceGenerator.nextShape
    yield GameState.initial(firstShape, nextShape, config.grid.width, config.grid.height)

  private def showOutro(finalState: GameState): ZIO[GameEnv, Throwable, Unit] =
    for
      config <- ZIO.service[AppConfig]
      _      <- ZIO.logInfo(
        s"Game finished - Score: ${finalState.score}, Lines: ${finalState.linesCleared}, Level: ${finalState.level}"
      )
      _ <- GameRunner.renderGameOver(finalState)
      _ <- TtyService.sleep(config.timing.outroDelayMs)
    yield ()

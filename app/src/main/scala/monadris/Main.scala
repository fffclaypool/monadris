package monadris

import zio.*
import zio.logging.backend.SLF4J

import monadris.domain.config.AppConfig
import monadris.domain.*
import monadris.infrastructure.*

/**
 * テトリスのメインエントリーポイント
 * ZIOAppを使用してエフェクトを実行
 * 構成と起動のみに集中し、ロジックはinfrastructureパッケージに委譲
 */
object Main extends ZIOAppDefault:

  override def run: Task[Unit] =
    program
      .provideLayer(Runtime.removeDefaultLoggers >>> SLF4J.slf4j ++ GameEnv.live)
      .catchAll {
        case error: Config.Error =>
          ZIO.logError(s"Configuration error: ${error.getMessage}")
        case error =>
          ZIO.logError(s"Application failed: $error")
      }

  val program: ZIO[GameEnv, Throwable, Unit] =
    ZIO.scoped {
      for
        _            <- showIntro
        initialState <- initializeGame
        finalState   <- runGameSession(initialState)
        _            <- showOutro(finalState)
      yield ()
    }

  private val showIntro: ZIO[GameEnv, Throwable, Unit] =
    for
      config <- ZIO.service[AppConfig]
      _      <- ZIO.logInfo("Monadris starting...")
      _      <- GameRunner.showTitle
      _      <- TtyService.sleep(config.timing.titleDelayMs)
    yield ()

  private val initializeGame: ZIO[AppConfig, Nothing, GameState] =
    for
      config     <- ZIO.service[AppConfig]
      firstShape <- GameRunner.RandomPieceGenerator.nextShape
      nextShape  <- GameRunner.RandomPieceGenerator.nextShape
    yield GameState.initial(firstShape, nextShape, config.grid.width, config.grid.height)

  private def runGameSession(initialState: GameState): ZIO[GameEnv, Throwable, GameState] =
    for
      _          <- TerminalControl.enableRawMode
      finalState <- GameRunner.interactiveGameLoop(initialState)
        .ensuring(TerminalControl.disableRawMode.ignore)
    yield finalState

  private def showOutro(finalState: GameState): ZIO[GameEnv, Throwable, Unit] =
    for
      config <- ZIO.service[AppConfig]
      _      <- ZIO.logInfo(s"Game finished - Score: ${finalState.score}, Lines: ${finalState.linesCleared}, Level: ${finalState.level}")
      _      <- GameRunner.renderGameOver(finalState)
      _      <- ConsoleService.print("\nGame ended.\r\n")
      _      <- TtyService.sleep(config.timing.outroDelayMs)
      _      <- ZIO.logInfo("Monadris shutting down...")
    yield ()

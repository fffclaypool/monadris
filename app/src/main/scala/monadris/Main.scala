package monadris

import zio.*
import zio.logging.backend.SLF4J

import monadris.domain.config.AppConfig
import monadris.domain.model.game.TetrisGame
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
        _           <- showIntro
        initialGame <- initializeGame
        finalGame   <- runGameSession(initialGame)
        _           <- showOutro(finalGame)
      yield ()
    }

  private val showIntro: ZIO[GameEnv, Throwable, Unit] =
    for
      config <- ZIO.service[AppConfig]
      _      <- ZIO.logInfo("Monadris starting...")
      _      <- GameRunner.showTitle
      _      <- TtyService.sleep(config.timing.titleDelayMs)
    yield ()

  private val initializeGame: ZIO[AppConfig, Nothing, TetrisGame] =
    for
      config <- ZIO.service[AppConfig]
      seed   <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
    yield TetrisGame.create(
      seed,
      config.grid.width,
      config.grid.height,
      config.score,
      config.level
    )

  private def runGameSession(initialGame: TetrisGame): ZIO[GameEnv, Throwable, TetrisGame] =
    for
      _ <- TerminalControl.enableRawMode
      finalGame <- GameRunner
        .interactiveGameLoop(initialGame)
        .ensuring(TerminalControl.disableRawMode.ignore)
    yield finalGame

  private def showOutro(finalGame: TetrisGame): ZIO[GameEnv, Throwable, Unit] =
    val score = finalGame.scoreState
    for
      config <- ZIO.service[AppConfig]
      _ <- ZIO.logInfo(
        s"Game finished - Score: ${score.score}, Lines: ${score.linesCleared}, Level: ${score.level}"
      )
      _ <- GameRunner.renderGameOver(finalGame)
      _ <- ConsoleService.print("\nGame ended.\r\n")
      _ <- TtyService.sleep(config.timing.outroDelayMs)
      _ <- ZIO.logInfo("Monadris shutting down...")
    yield ()

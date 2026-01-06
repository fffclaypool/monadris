package monadris

import zio.*
import zio.logging.backend.SLF4J

import monadris.application.GameEngine
import monadris.config.ConfigLayer
import monadris.domain.config.AppConfig
import monadris.domain.model.game.TetrisGame
import monadris.infrastructure.*

/**
 * テトリスのメインエントリーポイント
 * ZIOAppを使用してエフェクトを実行
 * 構成と起動のみに集中し、ロジックはGameEngineに委譲
 */
object Main extends ZIOAppDefault:

  // ゲームに必要な環境型
  type GameEnv = Terminal & AppConfig

  override def run: Task[Unit] =
    program
      .provideLayer(Runtime.removeDefaultLoggers >>> SLF4J.slf4j ++ Terminal.live ++ ConfigLayer.live)
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
      _      <- GameEngine.showTitleScreen
      _      <- ZIO.sleep(config.timing.titleDelayMs.millis)
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
    Terminal.withRawMode {
      GameEngine.runSession(initialGame)
    }

  private def showOutro(finalGame: TetrisGame): ZIO[GameEnv, Throwable, Unit] =
    val score = finalGame.scoreState
    for
      config <- ZIO.service[AppConfig]
      _ <- ZIO.logInfo(
        s"Game finished - Score: ${score.score}, Lines: ${score.linesCleared}, Level: ${score.level}"
      )
      _ <- GameEngine.showGameOverScreen(finalGame)
      _ <- printGameEnded
      _ <- ZIO.sleep(config.timing.outroDelayMs.millis)
      _ <- ZIO.logInfo("Monadris shutting down...")
    yield ()

  private def printGameEnded: Task[Unit] =
    ZIO.attempt {
      scala.Console.print("\nGame ended.\r\n")
      java.lang.System.out.flush()
    }

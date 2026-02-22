package monadris

import zio.*
import zio.logging.backend.SLF4J

import monadris.config.ConfigLayer
import monadris.config.DatabaseConfig
import monadris.infrastructure.game.GameSession
import monadris.infrastructure.game.MenuController
import monadris.infrastructure.game.ReplaySelector
import monadris.infrastructure.persistence.DatabaseLayer
import monadris.infrastructure.persistence.FlywayMigration
import monadris.infrastructure.persistence.PostgresReplayRepository
import monadris.infrastructure.persistence.ReplayRepository
import monadris.infrastructure.terminal.GameEnv
import monadris.infrastructure.terminal.TerminalSession
import monadris.view.GameView

object Main extends ZIOAppDefault:

  override def run: Task[Unit] =
    program
      .provideLayer(
        Runtime.removeDefaultLoggers >>> SLF4J.slf4j ++
          GameEnv.live ++
          databaseLayer
      )
      .catchAll {
        case error: Config.Error =>
          ZIO.logError(s"Configuration error: ${error.getMessage}")
        case error =>
          ZIO.logError(s"Application failed: $error")
      }

  private val databaseConfigLayer: ZLayer[Any, Config.Error, DatabaseConfig] =
    ConfigLayer.live.project(_.database)

  private val migrationLayer: ZLayer[DatabaseConfig, Throwable, Unit] =
    ZLayer.fromZIO(FlywayMigration.migrate.unit)

  private val databaseLayer: ZLayer[Any, Throwable, ReplayRepository] =
    databaseConfigLayer >>>
      (migrationLayer ++ DatabaseLayer.live) >>>
      PostgresReplayRepository.layer

  val program: ZIO[GameEnv & ReplayRepository, Throwable, Unit] =
    ZIO.scoped {
      for
        _ <- GameSession.showIntro
        _ <- TerminalSession.withRawMode {
          MenuController.run(executeMenuItem)
        }
        _ <- ZIO.logInfo("Monadris shutting down...")
      yield ()
    }

  private def executeMenuItem(index: Int): ZIO[GameEnv & ReplayRepository, Throwable, Boolean] =
    val menuItems = GameView.MenuItems.all
    if index >= 0 && index < menuItems.size then
      menuItems(index) match
        case GameView.MenuItems.PlayGame      => GameSession.playGame.as(true)
        case GameView.MenuItems.PlayAndRecord => GameSession.playAndRecord.as(true)
        case GameView.MenuItems.WatchReplay   => ReplaySelector.watchReplay.as(true)
        case GameView.MenuItems.ListReplays   => ReplaySelector.listReplays.as(true)
        case GameView.MenuItems.Quit          => ZIO.succeed(false)
        case _                                => ZIO.succeed(true)
    else ZIO.succeed(true)

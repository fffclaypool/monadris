package monadris

import zio.*
import zio.logging.backend.SLF4J

import monadris.config.AppConfig
import monadris.config.DatabaseConfig
import monadris.config.ReplayStorage
import monadris.infrastructure.game.GameSession
import monadris.infrastructure.game.MenuController
import monadris.infrastructure.game.ReplaySelector
import monadris.infrastructure.persistence.DatabaseLayer
import monadris.infrastructure.persistence.FileReplayRepository
import monadris.infrastructure.persistence.FlywayMigration
import monadris.infrastructure.persistence.PostgresReplayRepository
import monadris.infrastructure.persistence.ReplayRepository
import monadris.infrastructure.terminal.GameEnv
import monadris.infrastructure.terminal.TerminalSession
import monadris.view.GameView

object Main extends ZIOAppDefault:

  /** Errors are logged and then propagated, so the process exits with a non-zero code. */
  override def run: Task[Unit] =
    program
      .provideLayer(appLayer)
      .tapError {
        case error: Config.Error => ZIO.logError(s"Configuration error: ${error.getMessage}")
        case error               => ZIO.logError(s"Application failed: $error")
      }

  private val loggingLayer: ZLayer[Any, Nothing, Unit] =
    Runtime.removeDefaultLoggers >>> SLF4J.slf4j

  private val migrationLayer: ZLayer[DatabaseConfig, Throwable, Unit] =
    ZLayer.fromZIO(FlywayMigration.migrate.unit)

  /**
   * Only built when PostgreSQL storage is selected.
   * Migration runs before the connection pool is created, so an unreachable
   * database fails fast instead of waiting for the pool's connection timeout.
   */
  private val postgresReplayLayer: ZLayer[AppConfig, Throwable, ReplayRepository] =
    ZLayer.fromFunction((config: AppConfig) => config.database) >>>
      migrationLayer.flatMap(_ => DatabaseLayer.live) >>>
      PostgresReplayRepository.layer

  /** Chooses the replay backend from `monadris.replay.storage`; the default (file) needs no database. */
  val replayRepositoryLayer: ZLayer[AppConfig, Throwable, ReplayRepository] =
    ZLayer.service[AppConfig].flatMap { env =>
      env.get.replay.storage match
        case ReplayStorage.File     => FileReplayRepository.layer
        case ReplayStorage.Postgres => postgresReplayLayer
    }

  private val appLayer: ZLayer[Any, Throwable, GameEnv & ReplayRepository] =
    loggingLayer ++ (GameEnv.live >+> replayRepositoryLayer)

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

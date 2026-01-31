package monadris.infrastructure.persistence

import zio.*

import monadris.config.DatabaseConfig

import org.flywaydb.core.Flyway

object FlywayMigration:

  def migrate: ZIO[DatabaseConfig, Throwable, Int] =
    for
      config          <- ZIO.service[DatabaseConfig]
      migrationsCount <- runMigration(config)
      _               <- ZIO.logInfo(s"Applied $migrationsCount database migrations")
    yield migrationsCount

  private def runMigration(config: DatabaseConfig): Task[Int] =
    ZIO.attemptBlocking {
      val flyway = Flyway
        .configure()
        .dataSource(config.url, config.username, config.password)
        .locations("classpath:db/migration")
        .load()
      flyway.migrate().migrationsExecuted
    }

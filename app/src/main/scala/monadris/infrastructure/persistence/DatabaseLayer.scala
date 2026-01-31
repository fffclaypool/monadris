package monadris.infrastructure.persistence

import javax.sql.DataSource

import zio.*

import monadris.config.DatabaseConfig

import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import io.getquill.*
import io.getquill.jdbczio.Quill

object DatabaseLayer:

  val dataSourceLayer: ZLayer[DatabaseConfig, Throwable, DataSource] =
    ZLayer.scoped {
      for
        config     <- ZIO.service[DatabaseConfig]
        dataSource <- makeDataSourceScoped(config)
      yield dataSource
    }

  def makeDataSourceScoped(config: DatabaseConfig): ZIO[Scope, Throwable, HikariDataSource] =
    ZIO.acquireRelease {
      ZIO.attemptBlocking {
        val hikariConfig = new HikariConfig()
        hikariConfig.setJdbcUrl(config.url)
        hikariConfig.setUsername(config.username)
        hikariConfig.setPassword(config.password)
        hikariConfig.setMaximumPoolSize(config.poolSize)
        hikariConfig.setConnectionTimeout(config.connectionTimeoutMs)
        hikariConfig.setDriverClassName("org.postgresql.Driver")
        new HikariDataSource(hikariConfig)
      }
    }(ds => ZIO.attemptBlocking(ds.close()).orDie)

  val quillLayer: ZLayer[DataSource, Nothing, Quill.Postgres[SnakeCase]] =
    Quill.Postgres.fromNamingStrategy(SnakeCase)

  val live: ZLayer[DatabaseConfig, Throwable, Quill.Postgres[SnakeCase]] =
    dataSourceLayer >>> quillLayer

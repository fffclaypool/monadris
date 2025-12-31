package monadris.config

import zio.*
import zio.config.*
import zio.config.magnolia.*
import zio.config.typesafe.*

import monadris.domain.config.AppConfig

/**
 * ZIO Configを使った設定読み込みレイヤー
 */
object ConfigLayer:
  private val configDescriptor: Config[AppConfig] =
    deriveConfig[AppConfig].nested("monadris")

  val live: ZLayer[Any, Config.Error, AppConfig] =
    ZLayer.fromZIO(
      TypesafeConfigProvider.fromResourcePath().load(configDescriptor)
    )

package monadris.config

import zio.*
import zio.config.*
import zio.config.magnolia.*
import zio.config.typesafe.*

object ConfigLayer:
  private given DeriveConfig[ReplayStorage] =
    DeriveConfig.from(
      Config.string.mapOrFail { value =>
        ReplayStorage
          .fromString(value)
          .toRight(
            Config.Error.InvalidData(
              message = s"Unknown replay storage '$value' (expected one of: ${ReplayStorage.values.mkString(", ")})"
            )
          )
      }
    )

  private val configDescriptor: Config[AppConfig] =
    deriveConfig[AppConfig].nested("monadris")

  val live: ZLayer[Any, Config.Error, AppConfig] =
    ZLayer.fromZIO(
      TypesafeConfigProvider.fromResourcePath().load(configDescriptor)
    )

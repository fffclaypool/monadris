package monadris.config

import zio.*
import zio.test.*

import monadris.infrastructure.terminal.TestServices as Mocks

object ConfigLayerSpec extends ZIOSpecDefault:

  private object Keys:
    val Storage = "storage"

  private object Values:
    val File          = "file"
    val PostgresUpper = "POSTGRES"
    val Unknown       = "s3"

  private def loadStorage(value: String): IO[Config.Error, ReplayStorage] =
    ConfigProvider.fromMap(Map(Keys.Storage -> value)).load(ConfigLayer.replayStorageConfig.nested(Keys.Storage))

  def spec = suite("ConfigLayer")(
    test("replay storage is parsed case-insensitively") {
      for
        file     <- loadStorage(Values.File)
        postgres <- loadStorage(Values.PostgresUpper)
      yield assertTrue(file == ReplayStorage.File, postgres == ReplayStorage.Postgres)
    },
    test("a loaded configuration that violates a rule is a configuration error") {
      val invalid = Mocks.testConfig.copy(level = LevelConfig(linesPerLevel = 0))
      for
        rejected <- ConfigLayer.validated(invalid).either
        accepted <- ConfigLayer.validated(Mocks.testConfig).either
      yield assertTrue(
        rejected.left.exists(_.getMessage.contains("level.linesPerLevel")),
        accepted == Right(Mocks.testConfig)
      )
    },
    test("an unknown replay storage is a configuration error naming the valid values") {
      for result <- loadStorage(Values.Unknown).either
      yield assertTrue(
        result.left.exists { error =>
          val message = error.getMessage
          message.contains(Values.Unknown) && ReplayStorage.values.forall(storage => message.contains(storage.toString))
        }
      )
    }
  )

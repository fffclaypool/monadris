package monadris.config

import zio.test.*

import monadris.infrastructure.terminal.TestServices as Mocks

object ConfigValidationSpec extends ZIOSpecDefault:

  private val valid = Mocks.testConfig

  private object Expected:
    val InfrastructureErrors = 6
    val Password             = "s3cret-value"

  private def errorsOf(config: AppConfig): Vector[String] =
    ConfigValidation.validate(config).fold(identity, _ => Vector.empty)

  def spec = suite("ConfigValidation")(
    test("accepts the default configuration") {
      assertTrue(ConfigValidation.validate(valid) == Right(valid))
    },
    test("reports game rule violations") {
      val errors = errorsOf(valid.copy(level = LevelConfig(linesPerLevel = 0)))
      assertTrue(errors.exists(_.contains("level.linesPerLevel")))
    },
    test("reports every violated infrastructure rule at once") {
      val errors = errorsOf(
        valid.copy(
          terminal = valid.terminal.copy(inputPollIntervalMs = 0),
          timing = TimingConfig(titleDelayMs = -1, outroDelayMs = -1),
          replay = valid.replay.copy(defaultSpeed = 0.0),
          database = valid.database.copy(poolSize = 0, connectionTimeoutMs = 0)
        )
      )
      assertTrue(errors.size == Expected.InfrastructureErrors)
    },
    test("game exposes the rules handed to the core") {
      assertTrue(
        valid.game == GameConfig(valid.grid, valid.score, valid.level, valid.speed)
      )
    },
    test("the database password is not printed") {
      val database = valid.database.copy(password = Expected.Password)
      assertTrue(
        !database.toString.contains(Expected.Password),
        database.toString.contains(database.url)
      )
    }
  )

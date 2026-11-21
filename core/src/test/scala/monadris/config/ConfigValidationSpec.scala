package monadris.config

import monadris.TestConfig

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConfigValidationSpec extends AnyFlatSpec with Matchers:

  private val valid = TestConfig.testConfig

  private object Expected:
    val SingleError       = 1
    val AllViolatedErrors = 6

  private def errorsOf(config: AppConfig): Vector[String] =
    ConfigValidation.validate(config).fold(identity, _ => Vector.empty)

  "ConfigValidation.validate" should "accept the default configuration" in {
    ConfigValidation.validate(valid) shouldBe Right(valid)
  }

  it should "reject a level setting that would divide by zero" in {
    val errors = errorsOf(valid.copy(level = valid.level.copy(linesPerLevel = 0)))

    errors should have size Expected.SingleError
    errors.head should include("level.linesPerLevel")
  }

  it should "reject a grid too small for the pieces" in {
    val tooSmall = ConfigValidation.MinGridWidth - 1
    val errors   = errorsOf(valid.copy(grid = GridConfig(width = tooSmall, height = tooSmall)))

    errors.exists(_.contains("grid.width")) shouldBe true
    errors.exists(_.contains("grid.height")) shouldBe true
  }

  it should "reject a minimum drop interval above the base interval" in {
    val speed  = valid.speed.copy(minDropIntervalMs = valid.speed.baseDropIntervalMs + 1)
    val errors = errorsOf(valid.copy(speed = speed))

    errors should have size Expected.SingleError
    errors.head should include("minDropIntervalMs")
  }

  it should "reject negative scores and non-positive replay speed" in {
    val errors = errorsOf(
      valid.copy(
        score = valid.score.copy(tetris = -1, hardDropPerCell = -1),
        replay = valid.replay.copy(defaultSpeed = 0.0)
      )
    )

    errors.exists(_.contains("score.tetris")) shouldBe true
    errors.exists(_.contains("score.hardDropPerCell")) shouldBe true
    errors.exists(_.contains("replay.defaultSpeed")) shouldBe true
  }

  it should "report every violated rule at once" in {
    val errors = errorsOf(
      valid.copy(
        level = LevelConfig(linesPerLevel = 0),
        terminal = valid.terminal.copy(inputPollIntervalMs = 0),
        timing = TimingConfig(titleDelayMs = -1, outroDelayMs = -1),
        database = valid.database.copy(poolSize = 0, connectionTimeoutMs = 0)
      )
    )

    errors should have size Expected.AllViolatedErrors
  }

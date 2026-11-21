package monadris.config

import monadris.TestConfig

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GameConfigValidationSpec extends AnyFlatSpec with Matchers:

  private val valid = TestConfig.testConfig

  private object Expected:
    val SingleError = 1

  private def errorsOf(config: GameConfig): Vector[String] =
    GameConfigValidation.validate(config).fold(identity, _ => Vector.empty)

  "GameConfigValidation.validate" should "accept the default configuration" in {
    GameConfigValidation.validate(valid) shouldBe Right(valid)
  }

  it should "reject a level setting that would divide by zero" in {
    val errors = errorsOf(valid.copy(level = valid.level.copy(linesPerLevel = 0)))

    errors should have size Expected.SingleError
    errors.head should include("level.linesPerLevel")
  }

  it should "reject a grid too small for the pieces" in {
    val tooSmall = GameConfigValidation.MinGridWidth - 1
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

  it should "reject negative scores" in {
    val errors = errorsOf(valid.copy(score = valid.score.copy(tetris = -1, hardDropPerCell = -1)))

    errors.exists(_.contains("score.tetris")) shouldBe true
    errors.exists(_.contains("score.hardDropPerCell")) shouldBe true
  }

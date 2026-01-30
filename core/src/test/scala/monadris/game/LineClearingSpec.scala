package monadris.game

import monadris.TestConfig
import monadris.domain.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LineClearingSpec extends AnyFlatSpec with Matchers:

  val config      = TestConfig.testConfig
  val scoreConfig = config.score
  val levelConfig = config.level
  val speedConfig = config.speed
  val gridWidth   = config.grid.width
  val gridHeight  = config.grid.height

  val singleLine: Int = 1
  val doubleLine: Int = 2
  val tripleLine: Int = 3
  val tetrisLine: Int = 4
  val noLines: Int    = 0

  val baseLevel: Int        = 1
  val midLevel: Int         = 5
  val highLevel: Int        = 10
  val veryHighLevel: Int    = 100
  val customStartLevel: Int = 5

  val centerX: Int = gridWidth / 2

  "LineClearing.clearLines" should "return unchanged grid when no lines completed" in {
    val grid   = Grid.empty(gridWidth, gridHeight)
    val result = LineClearing.clearLines(grid, level = baseLevel, scoreConfig)

    result.linesCleared shouldBe noLines
    result.scoreGained shouldBe noLines
    result.grid shouldBe grid
  }

  it should "clear one completed line" in {
    val grid       = Grid.empty(gridWidth, gridHeight)
    val filled     = Cell.Filled(TetrominoShape.I)
    val bottomRow  = gridHeight - 1
    val filledGrid = (0 until gridWidth).foldLeft(grid) { (g, x) =>
      g.place(Position(x, bottomRow), filled)
    }

    val result = LineClearing.clearLines(filledGrid, level = baseLevel, scoreConfig)

    result.linesCleared shouldBe singleLine
    result.grid.isEmpty(Position(0, bottomRow)) shouldBe true
  }

  it should "clear multiple completed lines" in {
    val grid            = Grid.empty(gridWidth, gridHeight)
    val filled          = Cell.Filled(TetrominoShape.I)
    val bottomRow       = gridHeight - 1
    val secondBottomRow = gridHeight - 2
    val filledGrid      = (0 until gridWidth).foldLeft(grid) { (g, x) =>
      g.place(Position(x, secondBottomRow), filled)
        .place(Position(x, bottomRow), filled)
    }

    val result = LineClearing.clearLines(filledGrid, level = baseLevel, scoreConfig)

    result.linesCleared shouldBe doubleLine
  }

  it should "shift rows down after clearing" in {
    val grid            = Grid.empty(gridWidth, gridHeight)
    val filled          = Cell.Filled(TetrominoShape.I)
    val bottomRow       = gridHeight - 1
    val secondBottomRow = gridHeight - 2
    val thirdBottomRow  = gridHeight - 3

    val setupGrid = (0 until gridWidth)
      .foldLeft(grid) { (g, x) =>
        g.place(Position(x, secondBottomRow), filled)
          .place(Position(x, bottomRow), filled)
      }
      .place(Position(centerX, thirdBottomRow), filled)

    val result = LineClearing.clearLines(setupGrid, level = baseLevel, scoreConfig)

    result.grid.isEmpty(Position(centerX, bottomRow)) shouldBe false
  }

  "LineClearing.calculateScore" should "calculate correct score for 1 line" in {
    LineClearing.calculateScore(singleLine, level = baseLevel, scoreConfig) shouldBe scoreConfig.singleLine
    LineClearing.calculateScore(
      singleLine,
      level = baseLevel + 1,
      scoreConfig
    ) shouldBe scoreConfig.singleLine * (baseLevel + 1)
    LineClearing.calculateScore(singleLine, level = midLevel, scoreConfig) shouldBe scoreConfig.singleLine * midLevel
  }

  it should "calculate correct score for 2 lines" in {
    LineClearing.calculateScore(doubleLine, level = baseLevel, scoreConfig) shouldBe scoreConfig.doubleLine
    LineClearing.calculateScore(
      doubleLine,
      level = baseLevel + 1,
      scoreConfig
    ) shouldBe scoreConfig.doubleLine * (baseLevel + 1)
  }

  it should "calculate correct score for 3 lines" in {
    LineClearing.calculateScore(tripleLine, level = baseLevel, scoreConfig) shouldBe scoreConfig.tripleLine
    LineClearing.calculateScore(
      tripleLine,
      level = tripleLine,
      scoreConfig
    ) shouldBe scoreConfig.tripleLine * tripleLine
  }

  it should "calculate correct score for 4 lines (Tetris)" in {
    LineClearing.calculateScore(tetrisLine, level = baseLevel, scoreConfig) shouldBe scoreConfig.tetris
    LineClearing.calculateScore(
      tetrisLine,
      level = baseLevel + 1,
      scoreConfig
    ) shouldBe scoreConfig.tetris * (baseLevel + 1)
  }

  "LineClearing.calculateLevel" should "start at level 1" in {
    LineClearing.calculateLevel(noLines, levelConfig) shouldBe baseLevel
  }

  it should "increase level every LinesPerLevel lines" in {
    LineClearing.calculateLevel(levelConfig.linesPerLevel - 1, levelConfig) shouldBe baseLevel
    LineClearing.calculateLevel(levelConfig.linesPerLevel, levelConfig) shouldBe baseLevel + 1
    LineClearing.calculateLevel(levelConfig.linesPerLevel * 2 - 1, levelConfig) shouldBe baseLevel + 1
    LineClearing.calculateLevel(levelConfig.linesPerLevel * 2, levelConfig) shouldBe baseLevel + 2
  }

  it should "respect start level" in {
    LineClearing.calculateLevel(noLines, levelConfig, startLevel = customStartLevel) shouldBe customStartLevel
    LineClearing.calculateLevel(
      levelConfig.linesPerLevel,
      levelConfig,
      startLevel = customStartLevel
    ) shouldBe customStartLevel + 1
  }

  "LineClearing.dropInterval" should "decrease with higher levels" in {
    val intervalLevel1  = LineClearing.dropInterval(baseLevel, speedConfig)
    val intervalLevel5  = LineClearing.dropInterval(midLevel, speedConfig)
    val intervalLevel10 = LineClearing.dropInterval(highLevel, speedConfig)

    intervalLevel5 should be < intervalLevel1
    intervalLevel10 should be < intervalLevel5
  }

  it should "have minimum interval" in {
    val intervalHighLevel = LineClearing.dropInterval(veryHighLevel, speedConfig)
    intervalHighLevel should be >= speedConfig.minDropIntervalMs
  }

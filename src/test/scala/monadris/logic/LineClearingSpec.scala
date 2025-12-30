package monadris.logic

import monadris.domain.*
import monadris.effect.TestServices

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LineClearingSpec extends AnyFlatSpec with Matchers:

  val config = TestServices.testConfig
  val scoreConfig = config.score
  val levelConfig = config.level
  val speedConfig = config.speed
  val gridWidth = config.grid.width
  val gridHeight = config.grid.height

  "LineClearing.clearLines" should "return unchanged grid when no lines completed" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    val result = LineClearing.clearLines(grid, level = 1, scoreConfig)

    result.linesCleared shouldBe 0
    result.scoreGained shouldBe 0
    result.grid shouldBe grid
  }

  it should "clear one completed line" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    val filled = Cell.Filled(TetrominoShape.I)
    val bottomRow = gridHeight - 1
    val filledGrid = (0 until gridWidth).foldLeft(grid) { (g, x) =>
      g.place(Position(x, bottomRow), filled)
    }

    val result = LineClearing.clearLines(filledGrid, level = 1, scoreConfig)

    result.linesCleared shouldBe 1
    result.grid.isEmpty(Position(0, bottomRow)) shouldBe true
  }

  it should "clear multiple completed lines" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    val filled = Cell.Filled(TetrominoShape.I)
    val bottomRow = gridHeight - 1
    val secondBottomRow = gridHeight - 2
    val filledGrid = (0 until gridWidth).foldLeft(grid) { (g, x) =>
      g.place(Position(x, secondBottomRow), filled)
        .place(Position(x, bottomRow), filled)
    }

    val result = LineClearing.clearLines(filledGrid, level = 1, scoreConfig)

    result.linesCleared shouldBe 2
  }

  it should "shift rows down after clearing" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    val filled = Cell.Filled(TetrominoShape.I)
    val bottomRow = gridHeight - 1
    val secondBottomRow = gridHeight - 2
    val thirdBottomRow = gridHeight - 3

    // thirdBottomRow行目にブロックを置き、secondBottomRow-bottomRow行目を埋める
    val setupGrid = (0 until gridWidth).foldLeft(grid) { (g, x) =>
      g.place(Position(x, secondBottomRow), filled)
        .place(Position(x, bottomRow), filled)
    }.place(Position(5, thirdBottomRow), filled)

    val result = LineClearing.clearLines(setupGrid, level = 1, scoreConfig)

    // thirdBottomRow行目のブロックはbottomRow行目に落ちるはず
    result.grid.isEmpty(Position(5, bottomRow)) shouldBe false
  }

  "LineClearing.calculateScore" should "calculate correct score for 1 line" in {
    LineClearing.calculateScore(1, level = 1, scoreConfig) shouldBe scoreConfig.singleLine
    LineClearing.calculateScore(1, level = 2, scoreConfig) shouldBe scoreConfig.singleLine * 2
    LineClearing.calculateScore(1, level = 5, scoreConfig) shouldBe scoreConfig.singleLine * 5
  }

  it should "calculate correct score for 2 lines" in {
    LineClearing.calculateScore(2, level = 1, scoreConfig) shouldBe scoreConfig.doubleLine
    LineClearing.calculateScore(2, level = 2, scoreConfig) shouldBe scoreConfig.doubleLine * 2
  }

  it should "calculate correct score for 3 lines" in {
    LineClearing.calculateScore(3, level = 1, scoreConfig) shouldBe scoreConfig.tripleLine
    LineClearing.calculateScore(3, level = 3, scoreConfig) shouldBe scoreConfig.tripleLine * 3
  }

  it should "calculate correct score for 4 lines (Tetris)" in {
    LineClearing.calculateScore(4, level = 1, scoreConfig) shouldBe scoreConfig.tetris
    LineClearing.calculateScore(4, level = 2, scoreConfig) shouldBe scoreConfig.tetris * 2
  }

  "LineClearing.calculateLevel" should "start at level 1" in {
    LineClearing.calculateLevel(0, levelConfig) shouldBe 1
  }

  it should "increase level every LinesPerLevel lines" in {
    LineClearing.calculateLevel(levelConfig.linesPerLevel - 1, levelConfig) shouldBe 1
    LineClearing.calculateLevel(levelConfig.linesPerLevel, levelConfig) shouldBe 2
    LineClearing.calculateLevel(levelConfig.linesPerLevel * 2 - 1, levelConfig) shouldBe 2
    LineClearing.calculateLevel(levelConfig.linesPerLevel * 2, levelConfig) shouldBe 3
  }

  it should "respect start level" in {
    LineClearing.calculateLevel(0, levelConfig, startLevel = 5) shouldBe 5
    LineClearing.calculateLevel(levelConfig.linesPerLevel, levelConfig, startLevel = 5) shouldBe 6
  }

  "LineClearing.dropInterval" should "decrease with higher levels" in {
    val level1 = LineClearing.dropInterval(1, speedConfig)
    val level5 = LineClearing.dropInterval(5, speedConfig)
    val level10 = LineClearing.dropInterval(10, speedConfig)

    level5 should be < level1
    level10 should be < level5
  }

  it should "have minimum interval" in {
    val highLevel = LineClearing.dropInterval(100, speedConfig)
    highLevel should be >= speedConfig.minDropIntervalMs
  }

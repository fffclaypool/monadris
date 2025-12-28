package monadris.logic

import monadris.domain.*
import monadris.domain.GameConfig.{Grid as GridConfig, Level as LevelConfig, Score, Speed}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LineClearingSpec extends AnyFlatSpec with Matchers:

  "LineClearing.clearLines" should "return unchanged grid when no lines completed" in {
    val grid = Grid.empty(GridConfig.DefaultWidth, GridConfig.DefaultHeight)
    val result = LineClearing.clearLines(grid, level = 1)

    result.linesCleared shouldBe 0
    result.scoreGained shouldBe 0
    result.grid shouldBe grid
  }

  it should "clear one completed line" in {
    val grid = Grid.empty(GridConfig.DefaultWidth, GridConfig.DefaultHeight)
    val filled = Cell.Filled(TetrominoShape.I)
    val bottomRow = GridConfig.DefaultHeight - 1
    val filledGrid = (0 until GridConfig.DefaultWidth).foldLeft(grid) { (g, x) =>
      g.place(Position(x, bottomRow), filled)
    }

    val result = LineClearing.clearLines(filledGrid, level = 1)

    result.linesCleared shouldBe 1
    result.grid.isEmpty(Position(0, bottomRow)) shouldBe true
  }

  it should "clear multiple completed lines" in {
    val grid = Grid.empty(GridConfig.DefaultWidth, GridConfig.DefaultHeight)
    val filled = Cell.Filled(TetrominoShape.I)
    val bottomRow = GridConfig.DefaultHeight - 1
    val secondBottomRow = GridConfig.DefaultHeight - 2
    val filledGrid = (0 until GridConfig.DefaultWidth).foldLeft(grid) { (g, x) =>
      g.place(Position(x, secondBottomRow), filled)
        .place(Position(x, bottomRow), filled)
    }

    val result = LineClearing.clearLines(filledGrid, level = 1)

    result.linesCleared shouldBe 2
  }

  it should "shift rows down after clearing" in {
    val grid = Grid.empty(GridConfig.DefaultWidth, GridConfig.DefaultHeight)
    val filled = Cell.Filled(TetrominoShape.I)
    val bottomRow = GridConfig.DefaultHeight - 1
    val secondBottomRow = GridConfig.DefaultHeight - 2
    val thirdBottomRow = GridConfig.DefaultHeight - 3

    // thirdBottomRow行目にブロックを置き、secondBottomRow-bottomRow行目を埋める
    val setupGrid = (0 until GridConfig.DefaultWidth).foldLeft(grid) { (g, x) =>
      g.place(Position(x, secondBottomRow), filled)
        .place(Position(x, bottomRow), filled)
    }.place(Position(5, thirdBottomRow), filled)

    val result = LineClearing.clearLines(setupGrid, level = 1)

    // thirdBottomRow行目のブロックはbottomRow行目に落ちるはず
    result.grid.isEmpty(Position(5, bottomRow)) shouldBe false
  }

  "LineClearing.calculateScore" should "calculate correct score for 1 line" in {
    LineClearing.calculateScore(1, level = 1) shouldBe Score.SingleLine
    LineClearing.calculateScore(1, level = 2) shouldBe Score.SingleLine * 2
    LineClearing.calculateScore(1, level = 5) shouldBe Score.SingleLine * 5
  }

  it should "calculate correct score for 2 lines" in {
    LineClearing.calculateScore(2, level = 1) shouldBe Score.DoubleLine
    LineClearing.calculateScore(2, level = 2) shouldBe Score.DoubleLine * 2
  }

  it should "calculate correct score for 3 lines" in {
    LineClearing.calculateScore(3, level = 1) shouldBe Score.TripleLine
    LineClearing.calculateScore(3, level = 3) shouldBe Score.TripleLine * 3
  }

  it should "calculate correct score for 4 lines (Tetris)" in {
    LineClearing.calculateScore(4, level = 1) shouldBe Score.Tetris
    LineClearing.calculateScore(4, level = 2) shouldBe Score.Tetris * 2
  }

  "LineClearing.calculateLevel" should "start at level 1" in {
    LineClearing.calculateLevel(0) shouldBe 1
  }

  it should "increase level every LinesPerLevel lines" in {
    LineClearing.calculateLevel(LevelConfig.LinesPerLevel - 1) shouldBe 1
    LineClearing.calculateLevel(LevelConfig.LinesPerLevel) shouldBe 2
    LineClearing.calculateLevel(LevelConfig.LinesPerLevel * 2 - 1) shouldBe 2
    LineClearing.calculateLevel(LevelConfig.LinesPerLevel * 2) shouldBe 3
  }

  it should "respect start level" in {
    LineClearing.calculateLevel(0, startLevel = 5) shouldBe 5
    LineClearing.calculateLevel(LevelConfig.LinesPerLevel, startLevel = 5) shouldBe 6
  }

  "LineClearing.dropInterval" should "decrease with higher levels" in {
    val level1 = LineClearing.dropInterval(1)
    val level5 = LineClearing.dropInterval(5)
    val level10 = LineClearing.dropInterval(10)

    level5 should be < level1
    level10 should be < level5
  }

  it should "have minimum interval" in {
    val highLevel = LineClearing.dropInterval(100)
    highLevel should be >= Speed.MinDropIntervalMs
  }

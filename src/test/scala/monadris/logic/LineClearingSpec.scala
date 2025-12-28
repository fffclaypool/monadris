package monadris.logic

import monadris.domain.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LineClearingSpec extends AnyFlatSpec with Matchers:

  "LineClearing.clearLines" should "return unchanged grid when no lines completed" in {
    val grid = Grid.empty(10, 20)
    val result = LineClearing.clearLines(grid, level = 1)

    result.linesCleared shouldBe 0
    result.scoreGained shouldBe 0
    result.grid shouldBe grid
  }

  it should "clear one completed line" in {
    val grid = Grid.empty(10, 20)
    val filled = Cell.Filled(TetrominoShape.I)
    val filledGrid = (0 until 10).foldLeft(grid) { (g, x) =>
      g.place(Position(x, 19), filled)
    }

    val result = LineClearing.clearLines(filledGrid, level = 1)

    result.linesCleared shouldBe 1
    result.grid.isEmpty(Position(0, 19)) shouldBe true
  }

  it should "clear multiple completed lines" in {
    val grid = Grid.empty(10, 20)
    val filled = Cell.Filled(TetrominoShape.I)
    val filledGrid = (0 until 10).foldLeft(grid) { (g, x) =>
      g.place(Position(x, 18), filled)
        .place(Position(x, 19), filled)
    }

    val result = LineClearing.clearLines(filledGrid, level = 1)

    result.linesCleared shouldBe 2
  }

  it should "shift rows down after clearing" in {
    val grid = Grid.empty(10, 20)
    val filled = Cell.Filled(TetrominoShape.I)

    // 17行目にブロックを置き、18-19行目を埋める
    val setupGrid = (0 until 10).foldLeft(grid) { (g, x) =>
      g.place(Position(x, 18), filled)
        .place(Position(x, 19), filled)
    }.place(Position(5, 17), filled)

    val result = LineClearing.clearLines(setupGrid, level = 1)

    // 17行目のブロックは19行目に落ちるはず
    result.grid.isEmpty(Position(5, 19)) shouldBe false
  }

  "LineClearing.calculateScore" should "calculate correct score for 1 line" in {
    LineClearing.calculateScore(1, level = 1) shouldBe 100
    LineClearing.calculateScore(1, level = 2) shouldBe 200
    LineClearing.calculateScore(1, level = 5) shouldBe 500
  }

  it should "calculate correct score for 2 lines" in {
    LineClearing.calculateScore(2, level = 1) shouldBe 300
    LineClearing.calculateScore(2, level = 2) shouldBe 600
  }

  it should "calculate correct score for 3 lines" in {
    LineClearing.calculateScore(3, level = 1) shouldBe 500
    LineClearing.calculateScore(3, level = 3) shouldBe 1500
  }

  it should "calculate correct score for 4 lines (Tetris)" in {
    LineClearing.calculateScore(4, level = 1) shouldBe 800
    LineClearing.calculateScore(4, level = 2) shouldBe 1600
  }

  "LineClearing.calculateLevel" should "start at level 1" in {
    LineClearing.calculateLevel(0) shouldBe 1
  }

  it should "increase level every 10 lines" in {
    LineClearing.calculateLevel(9) shouldBe 1
    LineClearing.calculateLevel(10) shouldBe 2
    LineClearing.calculateLevel(19) shouldBe 2
    LineClearing.calculateLevel(20) shouldBe 3
  }

  it should "respect start level" in {
    LineClearing.calculateLevel(0, startLevel = 5) shouldBe 5
    LineClearing.calculateLevel(10, startLevel = 5) shouldBe 6
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
    highLevel should be >= 100L
  }

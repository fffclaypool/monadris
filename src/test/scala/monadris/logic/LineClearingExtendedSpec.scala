package monadris.logic

import monadris.domain.*
import monadris.domain.GameConfig.{Grid as GridConfig, Score}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Extended tests for LineClearing to improve branch coverage
 */
class LineClearingExtendedSpec extends AnyFlatSpec with Matchers:

  // ============================================================
  // clearLines edge cases
  // ============================================================

  "LineClearing.clearLines" should "return zero score when no lines cleared" in {
    val grid = Grid.empty()
    val result = LineClearing.clearLines(grid, level = 1)

    result.linesCleared shouldBe 0
    result.scoreGained shouldBe 0
    result.grid shouldBe grid
  }

  it should "clear single line and calculate correct score" in {
    var grid = Grid.empty()
    val filled = Cell.Filled(TetrominoShape.I)

    // Fill bottom row completely
    for x <- 0 until GridConfig.DefaultWidth do
      grid = grid.place(Position(x, GridConfig.DefaultHeight - 1), filled)

    val result = LineClearing.clearLines(grid, level = 1)

    result.linesCleared shouldBe 1
    result.scoreGained shouldBe Score.SingleLine * 1
  }

  it should "clear double line and calculate correct score" in {
    var grid = Grid.empty()
    val filled = Cell.Filled(TetrominoShape.I)

    // Fill bottom 2 rows completely
    for y <- (GridConfig.DefaultHeight - 2) until GridConfig.DefaultHeight do
      for x <- 0 until GridConfig.DefaultWidth do
        grid = grid.place(Position(x, y), filled)

    val result = LineClearing.clearLines(grid, level = 2)

    result.linesCleared shouldBe 2
    result.scoreGained shouldBe Score.DoubleLine * 2
  }

  it should "clear triple line and calculate correct score" in {
    var grid = Grid.empty()
    val filled = Cell.Filled(TetrominoShape.I)

    // Fill bottom 3 rows completely
    for y <- (GridConfig.DefaultHeight - 3) until GridConfig.DefaultHeight do
      for x <- 0 until GridConfig.DefaultWidth do
        grid = grid.place(Position(x, y), filled)

    val result = LineClearing.clearLines(grid, level = 3)

    result.linesCleared shouldBe 3
    result.scoreGained shouldBe Score.TripleLine * 3
  }

  it should "clear tetris (4 lines) and calculate correct score" in {
    var grid = Grid.empty()
    val filled = Cell.Filled(TetrominoShape.I)

    // Fill bottom 4 rows completely
    for y <- (GridConfig.DefaultHeight - 4) until GridConfig.DefaultHeight do
      for x <- 0 until GridConfig.DefaultWidth do
        grid = grid.place(Position(x, y), filled)

    val result = LineClearing.clearLines(grid, level = 5)

    result.linesCleared shouldBe 4
    result.scoreGained shouldBe Score.Tetris * 5
  }

  // ============================================================
  // calculateScore edge cases
  // ============================================================

  "LineClearing.calculateScore" should "return 0 for 0 lines" in {
    LineClearing.calculateScore(0, 1) shouldBe 0
  }

  it should "return 0 for negative lines" in {
    LineClearing.calculateScore(-1, 1) shouldBe 0
  }

  it should "return 0 for more than 4 lines" in {
    LineClearing.calculateScore(5, 1) shouldBe 0
    LineClearing.calculateScore(10, 1) shouldBe 0
  }

  it should "scale correctly with level" in {
    val level1Score = LineClearing.calculateScore(1, 1)
    val level10Score = LineClearing.calculateScore(1, 10)

    level10Score shouldBe level1Score * 10
  }

  it should "return correct scores for all line counts at level 1" in {
    LineClearing.calculateScore(1, 1) shouldBe Score.SingleLine
    LineClearing.calculateScore(2, 1) shouldBe Score.DoubleLine
    LineClearing.calculateScore(3, 1) shouldBe Score.TripleLine
    LineClearing.calculateScore(4, 1) shouldBe Score.Tetris
  }

  // ============================================================
  // calculateLevel edge cases
  // ============================================================

  "LineClearing.calculateLevel" should "start at level 1 by default" in {
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

  it should "handle large line counts" in {
    LineClearing.calculateLevel(100) shouldBe 11
    LineClearing.calculateLevel(200) shouldBe 21
  }

  // ============================================================
  // dropInterval edge cases
  // ============================================================

  "LineClearing.dropInterval" should "start at base interval for level 1" in {
    LineClearing.dropInterval(1) shouldBe GameConfig.Speed.BaseDropIntervalMs
  }

  it should "decrease as level increases" in {
    val level1 = LineClearing.dropInterval(1)
    val level5 = LineClearing.dropInterval(5)
    val level10 = LineClearing.dropInterval(10)

    level5 should be < level1
    level10 should be < level5
  }

  it should "not go below minimum interval" in {
    val highLevel = LineClearing.dropInterval(100)
    highLevel shouldBe GameConfig.Speed.MinDropIntervalMs
  }

  it should "return minimum for very high levels" in {
    LineClearing.dropInterval(50) shouldBe GameConfig.Speed.MinDropIntervalMs
    LineClearing.dropInterval(1000) shouldBe GameConfig.Speed.MinDropIntervalMs
  }

  it should "decrease by correct amount per level" in {
    val level1 = LineClearing.dropInterval(1)
    val level2 = LineClearing.dropInterval(2)

    (level1 - level2) shouldBe GameConfig.Speed.DecreasePerLevelMs
  }

package monadris.logic

import monadris.TestConfig
import monadris.domain.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * ブランチカバレッジを向上させるためのLineClearing拡張テスト
 */
class LineClearingExtendedSpec extends AnyFlatSpec with Matchers:

  val config      = TestConfig.testConfig
  val scoreConfig = config.score
  val levelConfig = config.level
  val speedConfig = config.speed
  val gridWidth   = config.grid.width
  val gridHeight  = config.grid.height

  // ============================================================
  // clearLinesのエッジケース
  // ============================================================

  "LineClearing.clearLines" should "return zero score when no lines cleared" in {
    val grid   = Grid.empty(gridWidth, gridHeight)
    val result = LineClearing.clearLines(grid, level = 1, scoreConfig)

    result.linesCleared shouldBe 0
    result.scoreGained shouldBe 0
    result.grid shouldBe grid
  }

  it should "clear single line and calculate correct score" in {
    val filled = Cell.Filled(TetrominoShape.I)

    // 最下行を完全に埋める
    val grid = (0 until gridWidth).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, x) =>
      g.place(Position(x, gridHeight - 1), filled)
    }

    val result = LineClearing.clearLines(grid, level = 1, scoreConfig)

    result.linesCleared shouldBe 1
    result.scoreGained shouldBe scoreConfig.singleLine * 1
  }

  it should "clear double line and calculate correct score" in {
    val filled = Cell.Filled(TetrominoShape.I)

    // 下から2行を完全に埋める
    val grid = (for
      y <- (gridHeight - 2) until gridHeight
      x <- 0 until gridWidth
    yield Position(x, y)).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, pos) =>
      g.place(pos, filled)
    }

    val result = LineClearing.clearLines(grid, level = 2, scoreConfig)

    result.linesCleared shouldBe 2
    result.scoreGained shouldBe scoreConfig.doubleLine * 2
  }

  it should "clear triple line and calculate correct score" in {
    val filled = Cell.Filled(TetrominoShape.I)

    // 下から3行を完全に埋める
    val grid = (for
      y <- (gridHeight - 3) until gridHeight
      x <- 0 until gridWidth
    yield Position(x, y)).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, pos) =>
      g.place(pos, filled)
    }

    val result = LineClearing.clearLines(grid, level = 3, scoreConfig)

    result.linesCleared shouldBe 3
    result.scoreGained shouldBe scoreConfig.tripleLine * 3
  }

  it should "clear tetris (4 lines) and calculate correct score" in {
    val filled = Cell.Filled(TetrominoShape.I)

    // 下から4行を完全に埋める
    val grid = (for
      y <- (gridHeight - 4) until gridHeight
      x <- 0 until gridWidth
    yield Position(x, y)).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, pos) =>
      g.place(pos, filled)
    }

    val result = LineClearing.clearLines(grid, level = 5, scoreConfig)

    result.linesCleared shouldBe 4
    result.scoreGained shouldBe scoreConfig.tetris * 5
  }

  // ============================================================
  // calculateScoreのエッジケース
  // ============================================================

  "LineClearing.calculateScore" should "return 0 for 0 lines" in {
    LineClearing.calculateScore(0, 1, scoreConfig) shouldBe 0
  }

  it should "return 0 for negative lines" in {
    LineClearing.calculateScore(-1, 1, scoreConfig) shouldBe 0
  }

  it should "return 0 for more than 4 lines" in {
    LineClearing.calculateScore(5, 1, scoreConfig) shouldBe 0
    LineClearing.calculateScore(10, 1, scoreConfig) shouldBe 0
  }

  it should "scale correctly with level" in {
    val level1Score  = LineClearing.calculateScore(1, 1, scoreConfig)
    val level10Score = LineClearing.calculateScore(1, 10, scoreConfig)

    level10Score shouldBe level1Score * 10
  }

  it should "return correct scores for all line counts at level 1" in {
    LineClearing.calculateScore(1, 1, scoreConfig) shouldBe scoreConfig.singleLine
    LineClearing.calculateScore(2, 1, scoreConfig) shouldBe scoreConfig.doubleLine
    LineClearing.calculateScore(3, 1, scoreConfig) shouldBe scoreConfig.tripleLine
    LineClearing.calculateScore(4, 1, scoreConfig) shouldBe scoreConfig.tetris
  }

  // ============================================================
  // calculateLevelのエッジケース
  // ============================================================

  "LineClearing.calculateLevel" should "start at level 1 by default" in {
    LineClearing.calculateLevel(0, levelConfig) shouldBe 1
  }

  it should "increase level every 10 lines" in {
    LineClearing.calculateLevel(9, levelConfig) shouldBe 1
    LineClearing.calculateLevel(10, levelConfig) shouldBe 2
    LineClearing.calculateLevel(19, levelConfig) shouldBe 2
    LineClearing.calculateLevel(20, levelConfig) shouldBe 3
  }

  it should "respect start level" in {
    LineClearing.calculateLevel(0, levelConfig, startLevel = 5) shouldBe 5
    LineClearing.calculateLevel(10, levelConfig, startLevel = 5) shouldBe 6
  }

  it should "handle large line counts" in {
    LineClearing.calculateLevel(100, levelConfig) shouldBe 11
    LineClearing.calculateLevel(200, levelConfig) shouldBe 21
  }

  // ============================================================
  // dropIntervalのエッジケース
  // ============================================================

  "LineClearing.dropInterval" should "start at base interval for level 1" in {
    LineClearing.dropInterval(1, speedConfig) shouldBe speedConfig.baseDropIntervalMs
  }

  it should "decrease as level increases" in {
    val level1  = LineClearing.dropInterval(1, speedConfig)
    val level5  = LineClearing.dropInterval(5, speedConfig)
    val level10 = LineClearing.dropInterval(10, speedConfig)

    level5 should be < level1
    level10 should be < level5
  }

  it should "not go below minimum interval" in {
    val highLevel = LineClearing.dropInterval(100, speedConfig)
    highLevel shouldBe speedConfig.minDropIntervalMs
  }

  it should "return minimum for very high levels" in {
    LineClearing.dropInterval(50, speedConfig) shouldBe speedConfig.minDropIntervalMs
    LineClearing.dropInterval(1000, speedConfig) shouldBe speedConfig.minDropIntervalMs
  }

  it should "decrease by correct amount per level" in {
    val level1 = LineClearing.dropInterval(1, speedConfig)
    val level2 = LineClearing.dropInterval(2, speedConfig)

    (level1 - level2) shouldBe speedConfig.decreasePerLevelMs
  }

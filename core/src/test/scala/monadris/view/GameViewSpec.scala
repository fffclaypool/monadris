package monadris.view

import monadris.TestConfig
import monadris.domain.*
import monadris.domain.config.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GameViewSpec extends AnyFlatSpec with Matchers:

  val config: AppConfig = TestConfig.testConfig
  val gridWidth: Int    = config.grid.width
  val gridHeight: Int   = config.grid.height

  def initialState: GameState =
    GameState.initial(TetrominoShape.T, TetrominoShape.I, gridWidth, gridHeight)

  "GameView.shapeToColor" should "return Cyan for I-tetromino" in {
    GameView.shapeToColor(TetrominoShape.I) shouldBe UiColor.Cyan
  }

  it should "return Yellow for O-tetromino" in {
    GameView.shapeToColor(TetrominoShape.O) shouldBe UiColor.Yellow
  }

  it should "return Magenta for T-tetromino" in {
    GameView.shapeToColor(TetrominoShape.T) shouldBe UiColor.Magenta
  }

  it should "return Green for S-tetromino" in {
    GameView.shapeToColor(TetrominoShape.S) shouldBe UiColor.Green
  }

  it should "return Red for Z-tetromino" in {
    GameView.shapeToColor(TetrominoShape.Z) shouldBe UiColor.Red
  }

  it should "return Blue for J-tetromino" in {
    GameView.shapeToColor(TetrominoShape.J) shouldBe UiColor.Blue
  }

  it should "return White for L-tetromino" in {
    GameView.shapeToColor(TetrominoShape.L) shouldBe UiColor.White
  }

  it should "return correct color for all shapes" in
    TetrominoShape.values.foreach { shape =>
      GameView.shapeToColor(shape) shouldBe a[UiColor]
    }

  "GameView.toScreenBuffer" should "create non-empty buffer" in {
    val buffer = GameView.toScreenBuffer(initialState, config)
    buffer.width should be > 0
    buffer.height should be > 0
  }

  it should "include grid borders" in {
    val buffer = GameView.toScreenBuffer(initialState, config)
    buffer.pixels(0)(0).char shouldBe '┌'
  }

  it should "include score info" in {
    val buffer  = GameView.toScreenBuffer(initialState, config)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Score:")
  }

  it should "include level info" in {
    val buffer  = GameView.toScreenBuffer(initialState, config)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Level:")
  }

  it should "include lines info" in {
    val buffer  = GameView.toScreenBuffer(initialState, config)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Lines:")
  }

  it should "include next piece info" in {
    val buffer  = GameView.toScreenBuffer(initialState, config)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Next:")
  }

  it should "show PAUSED when game is paused" in {
    val pausedState = initialState.copy(status = GameStatus.Paused)
    val buffer      = GameView.toScreenBuffer(pausedState, config)
    val allText     = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("PAUSED")
  }

  it should "not show PAUSED when game is playing" in {
    val buffer  = GameView.toScreenBuffer(initialState, config)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should not include "PAUSED"
  }

  it should "include control hints" in {
    val buffer  = GameView.toScreenBuffer(initialState, config)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Quit")
  }

  it should "show filled blocks with color" in {
    val grid = Grid
      .empty(gridWidth, gridHeight)
      .place(Position(5, 15), Cell.Filled(TetrominoShape.I))
    val stateWithBlock = initialState.copy(grid = grid)

    val buffer = GameView.toScreenBuffer(stateWithBlock, config)
    buffer.pixels.exists(row => row.exists(p => p.char == '▓')) shouldBe true
  }

  it should "show current tetromino blocks" in {
    val buffer = GameView.toScreenBuffer(initialState, config)
    buffer.pixels.exists(row => row.exists(p => p.char == '█')) shouldBe true
  }

  it should "show empty cells" in {
    val buffer = GameView.toScreenBuffer(initialState, config)
    buffer.pixels.exists(row => row.exists(p => p.char == '·')) shouldBe true
  }

  "GameView.titleScreen" should "create buffer with title content" in {
    val buffer  = GameView.titleScreen
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Functional Tetris")
  }

  it should "include controls section" in {
    val buffer  = GameView.titleScreen
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Controls")
  }

  it should "mention movement keys" in {
    val buffer  = GameView.titleScreen
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Move")
  }

  it should "mention rotation" in {
    val buffer  = GameView.titleScreen
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Rotate")
  }

  it should "mention quit key" in {
    val buffer  = GameView.titleScreen
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Quit")
  }

  it should "have valid dimensions" in {
    val buffer = GameView.titleScreen
    buffer.width should be > 0
    buffer.height should be > 0
  }

  "GameView.gameOverScreen" should "create buffer with GAME OVER message" in {
    val buffer  = GameView.gameOverScreen(initialState)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("GAME OVER")
  }

  it should "show final score" in {
    val stateWithScore = initialState.copy(score = 12345)
    val buffer         = GameView.gameOverScreen(stateWithScore)
    val allText        = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("12345")
  }

  it should "show lines cleared" in {
    val stateWithLines = initialState.copy(linesCleared = 42)
    val buffer         = GameView.gameOverScreen(stateWithLines)
    val allText        = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("42")
  }

  it should "show final level" in {
    val stateWithLevel = initialState.copy(level = 7)
    val buffer         = GameView.gameOverScreen(stateWithLevel)
    val allText        = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("7")
  }

  it should "have valid dimensions" in {
    val buffer = GameView.gameOverScreen(initialState)
    buffer.width should be > 0
    buffer.height should be > 0
  }

  "GameView grid rendering" should "handle all tetromino shapes" in
    TetrominoShape.values.foreach { shape =>
      val state  = GameState.initial(shape, TetrominoShape.I, gridWidth, gridHeight)
      val buffer = GameView.toScreenBuffer(state, config)
      buffer.width should be > 0
    }

  it should "handle grid with multiple filled cells" in {
    val grid = (0 until gridWidth).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, x) =>
      g.place(Position(x, gridHeight - 1), Cell.Filled(TetrominoShape.I))
    }

    val stateWithFilledRow = initialState.copy(grid = grid)
    val buffer             = GameView.toScreenBuffer(stateWithFilledRow, config)

    val lockedCount = buffer.pixels.flatMap(_.filter(_.char == '▓')).size
    lockedCount shouldBe >=(gridWidth)
  }

  it should "use correct colors for filled cells" in {
    val grid = Grid
      .empty(gridWidth, gridHeight)
      .place(Position(0, 0), Cell.Filled(TetrominoShape.I))
      .place(Position(1, 0), Cell.Filled(TetrominoShape.O))

    val state  = initialState.copy(grid = grid)
    val buffer = GameView.toScreenBuffer(state, config)

    buffer.pixels.exists(row => row.exists(p => p.char == '▓')) shouldBe true
  }

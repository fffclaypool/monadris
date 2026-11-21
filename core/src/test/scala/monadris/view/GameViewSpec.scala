package monadris.view

import monadris.TestConfig
import monadris.config.*
import monadris.domain.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GameViewSpec extends AnyFlatSpec with Matchers:

  private object SmallGrid:
    val Width  = 6
    val Height = 8

  val config: GameConfig = TestConfig.testConfig
  val gridWidth: Int     = config.grid.width
  val gridHeight: Int    = config.grid.height

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
    val buffer = GameView.toScreenBuffer(initialState)
    buffer.width should be > 0
    buffer.height should be > 0
  }

  it should "include grid borders" in {
    val buffer = GameView.toScreenBuffer(initialState)
    buffer.pixels(0)(0).char shouldBe '┌'
  }

  it should "include score info" in {
    val buffer  = GameView.toScreenBuffer(initialState)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Score:")
  }

  it should "include level info" in {
    val buffer  = GameView.toScreenBuffer(initialState)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Level:")
  }

  it should "include lines info" in {
    val buffer  = GameView.toScreenBuffer(initialState)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Lines:")
  }

  it should "include next piece info" in {
    val buffer  = GameView.toScreenBuffer(initialState)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Next:")
  }

  it should "show PAUSED when game is paused" in {
    val pausedState = initialState.copy(status = GameStatus.Paused)
    val buffer      = GameView.toScreenBuffer(pausedState)
    val allText     = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("PAUSED")
  }

  it should "not show PAUSED when game is playing" in {
    val buffer  = GameView.toScreenBuffer(initialState)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should not include "PAUSED"
  }

  it should "include control hints" in {
    val buffer  = GameView.toScreenBuffer(initialState)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Quit")
  }

  it should "show filled blocks with color" in {
    val grid = Grid
      .empty(gridWidth, gridHeight)
      .place(Position(5, 15), Cell.Filled(TetrominoShape.I))
    val stateWithBlock = initialState.copy(grid = grid)

    val buffer = GameView.toScreenBuffer(stateWithBlock)
    buffer.pixels.exists(row => row.exists(p => p.char == '▓')) shouldBe true
  }

  it should "show current tetromino blocks" in {
    val buffer = GameView.toScreenBuffer(initialState)
    buffer.pixels.exists(row => row.exists(p => p.char == '█')) shouldBe true
  }

  it should "show empty cells" in {
    val buffer = GameView.toScreenBuffer(initialState)
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
      val buffer = GameView.toScreenBuffer(state)
      buffer.width should be > 0
    }

  it should "handle grid with multiple filled cells" in {
    val grid = (0 until gridWidth).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, x) =>
      g.place(Position(x, gridHeight - 1), Cell.Filled(TetrominoShape.I))
    }

    val stateWithFilledRow = initialState.copy(grid = grid)
    val buffer             = GameView.toScreenBuffer(stateWithFilledRow)

    val lockedCount = buffer.pixels.flatMap(_.filter(_.char == '▓')).size
    lockedCount shouldBe >=(gridWidth)
  }

  it should "use correct colors for filled cells" in {
    val grid = Grid
      .empty(gridWidth, gridHeight)
      .place(Position(0, 0), Cell.Filled(TetrominoShape.I))
      .place(Position(1, 0), Cell.Filled(TetrominoShape.O))

    val state  = initialState.copy(grid = grid)
    val buffer = GameView.toScreenBuffer(state)

    buffer.pixels.exists(row => row.exists(p => p.char == '▓')) shouldBe true
  }

  "Ghost piece rendering" should "show ghost blocks in the buffer" in {
    val buffer = GameView.toScreenBuffer(initialState)
    buffer.pixels.exists(row => row.exists(p => p.char == '░')) shouldBe true
  }

  it should "use the same color as the current piece" in {
    val buffer        = GameView.toScreenBuffer(initialState)
    val expectedColor = GameView.shapeToColor(TetrominoShape.T)
    val ghostPixels   = buffer.pixels.flatMap(_.filter(_.char == '░'))
    ghostPixels should not be empty
    all(ghostPixels.map(_.color)) shouldBe expectedColor
  }

  it should "render ghost below the current piece" in {
    val buffer  = GameView.toScreenBuffer(initialState)
    val ghostYs = for
      (row, y) <- buffer.pixels.zipWithIndex
      pixel    <- row
      if pixel.char == '░'
    yield y
    val filledYs = for
      (row, y) <- buffer.pixels.zipWithIndex
      pixel    <- row
      if pixel.char == '█'
    yield y
    ghostYs should not be empty
    filledYs should not be empty
    ghostYs.min should be >= filledYs.max
  }

  it should "stop ghost above filled cells on the grid" in {
    val bottomRow = (0 until gridWidth).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, x) =>
      g.place(Position(x, gridHeight - 1), Cell.Filled(TetrominoShape.I))
    }
    val state   = initialState.copy(grid = bottomRow)
    val buffer  = GameView.toScreenBuffer(state)
    val ghostYs = for
      (row, y) <- buffer.pixels.zipWithIndex
      pixel    <- row
      if pixel.char == '░'
    yield y
    ghostYs should not be empty
    // Ghost should be above the locked bottom row (gridHeight-1)
    // In buffer coordinates, y is offset by +1 for the top border
    val lockedRowBufferY = gridHeight - 1 + 1
    all(ghostYs) should be < lockedRowBufferY
  }

  it should "not show ghost when piece is already at landing position" in {
    // Manually move piece to the bottom by repeating moveDown
    val tetromino = (0 until gridHeight).foldLeft(initialState.currentTetromino) { (t, _) =>
      val next  = t.moveDown
      val valid = next.currentBlocks.forall(p => p.x >= 0 && p.x < gridWidth && p.y >= 0 && p.y < gridHeight)
      if valid then next else t
    }
    val stateAtBottom = initialState.copy(currentTetromino = tetromino)
    val buffer        = GameView.toScreenBuffer(stateAtBottom)
    // Ghost overlaps with the piece, so '░' is not shown (FilledBlock takes priority)
    val ghostPixels = buffer.pixels.flatMap(_.filter(_.char == '░'))
    ghostPixels shouldBe empty
  }

  "Next piece graphic rendering" should "show filled blocks in the info panel" in {
    val buffer          = GameView.toScreenBuffer(initialState)
    val infoPanelStartX = gridWidth + 4
    val nextPieceBlocks = for
      (row, y)   <- buffer.pixels.zipWithIndex
      (pixel, x) <- row.zipWithIndex
      if pixel.char == '█' && x >= infoPanelStartX
    yield pixel
    nextPieceBlocks should not be empty
  }

  it should "render graphic for all tetromino shapes" in
    TetrominoShape.values.foreach { shape =>
      val state           = GameState.initial(TetrominoShape.T, shape, gridWidth, gridHeight)
      val buffer          = GameView.toScreenBuffer(state)
      val infoPanelStartX = gridWidth + 4
      val nextPieceBlocks = for
        (row, y)   <- buffer.pixels.zipWithIndex
        (pixel, x) <- row.zipWithIndex
        if pixel.char == '█' && x >= infoPanelStartX
      yield pixel
      nextPieceBlocks should not be empty
    }

  it should "use correct color for next piece graphic" in
    TetrominoShape.values.foreach { shape =>
      val state           = GameState.initial(TetrominoShape.T, shape, gridWidth, gridHeight)
      val buffer          = GameView.toScreenBuffer(state)
      val expectedColor   = GameView.shapeToColor(shape)
      val infoPanelStartX = gridWidth + 4
      val nextPiecePixels = for
        (row, y)   <- buffer.pixels.zipWithIndex
        (pixel, x) <- row.zipWithIndex
        if pixel.char == '█' && x >= infoPanelStartX
      yield pixel
      nextPiecePixels should not be empty
      all(nextPiecePixels.map(_.color)) shouldBe expectedColor
    }

  "GameView.toScreenBuffer" should "size the screen from the grid of the state" in {
    val smallState = GameState.initial(TetrominoShape.T, TetrominoShape.I, SmallGrid.Width, SmallGrid.Height)
    val regular    = GameView.toScreenBuffer(initialState)
    val small      = GameView.toScreenBuffer(smallState)

    (regular.width - small.width) shouldBe (gridWidth - SmallGrid.Width)
    (regular.height - small.height) shouldBe (gridHeight - SmallGrid.Height)
  }

  "GameView.gameOverScreen" should "be the game screen with the result box drawn over it" in {
    val screen   = GameView.toScreenBuffer(initialState)
    val gameOver = GameView.gameOverScreen(initialState)

    gameOver.width shouldBe screen.width
    gameOver.height shouldBe screen.height
    gameOver.getRow(0) shouldBe screen.getRow(0)
    gameOver should not be screen
  }

package monadris.domain

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import GameConfig.Grid as GridConfig

/**
 * Extended tests for Grid to improve branch coverage
 */
class GridExtendedSpec extends AnyFlatSpec with Matchers:

  // ============================================================
  // Grid.empty with default parameters
  // ============================================================

  "Grid.empty" should "use default width and height when no parameters given" in {
    val grid = Grid.empty()
    grid.width shouldBe GridConfig.DefaultWidth
    grid.height shouldBe GridConfig.DefaultHeight
  }

  it should "use custom width when specified" in {
    val grid = Grid.empty(width = 15)
    grid.width shouldBe 15
    grid.height shouldBe GridConfig.DefaultHeight
  }

  it should "use custom height when specified" in {
    val grid = Grid.empty(height = 25)
    grid.width shouldBe GridConfig.DefaultWidth
    grid.height shouldBe 25
  }

  it should "use both custom dimensions when specified" in {
    val grid = Grid.empty(width = 12, height = 24)
    grid.width shouldBe 12
    grid.height shouldBe 24
  }

  // ============================================================
  // place with out-of-bounds positions
  // ============================================================

  "Grid.place" should "return unchanged grid for out-of-bounds position (negative x)" in {
    val grid = Grid.empty()
    val filled = Cell.Filled(TetrominoShape.T)
    val newGrid = grid.place(Position(-1, 5), filled)
    newGrid shouldBe grid
  }

  it should "return unchanged grid for out-of-bounds position (negative y)" in {
    val grid = Grid.empty()
    val filled = Cell.Filled(TetrominoShape.T)
    val newGrid = grid.place(Position(5, -1), filled)
    newGrid shouldBe grid
  }

  it should "return unchanged grid for out-of-bounds position (x >= width)" in {
    val grid = Grid.empty()
    val filled = Cell.Filled(TetrominoShape.T)
    val newGrid = grid.place(Position(GridConfig.DefaultWidth, 5), filled)
    newGrid shouldBe grid
  }

  it should "return unchanged grid for out-of-bounds position (y >= height)" in {
    val grid = Grid.empty()
    val filled = Cell.Filled(TetrominoShape.T)
    val newGrid = grid.place(Position(5, GridConfig.DefaultHeight), filled)
    newGrid shouldBe grid
  }

  // ============================================================
  // isEmpty behavior
  // ============================================================

  "Grid.isEmpty" should "return false for filled position" in {
    val grid = Grid.empty()
    val filled = Cell.Filled(TetrominoShape.I)
    val newGrid = grid.place(Position(5, 5), filled)
    newGrid.isEmpty(Position(5, 5)) shouldBe false
  }

  it should "return true for empty position" in {
    val grid = Grid.empty()
    grid.isEmpty(Position(5, 5)) shouldBe true
  }

  it should "return false for out-of-bounds position" in {
    val grid = Grid.empty()
    grid.isEmpty(Position(-1, 5)) shouldBe false
    grid.isEmpty(Position(5, -1)) shouldBe false
    grid.isEmpty(Position(GridConfig.DefaultWidth, 5)) shouldBe false
    grid.isEmpty(Position(5, GridConfig.DefaultHeight)) shouldBe false
  }

  // ============================================================
  // placeTetromino behavior
  // ============================================================

  "Grid.placeTetromino" should "place all 4 blocks of a tetromino" in {
    val grid = Grid.empty()
    val tetromino = Tetromino.spawn(TetrominoShape.O, GridConfig.DefaultWidth)
    val newGrid = grid.placeTetromino(tetromino)

    val filledCount = tetromino.currentBlocks.count { pos =>
      !newGrid.isEmpty(pos)
    }
    filledCount shouldBe 4
  }

  it should "fill cells with correct shape identifier" in {
    val grid = Grid.empty()
    val tetromino = Tetromino.spawn(TetrominoShape.T, GridConfig.DefaultWidth)
    val newGrid = grid.placeTetromino(tetromino)

    tetromino.currentBlocks.foreach { pos =>
      newGrid.get(pos) shouldBe Some(Cell.Filled(TetrominoShape.T))
    }
  }

  // ============================================================
  // completedRows behavior
  // ============================================================

  "Grid.completedRows" should "return empty list for empty grid" in {
    val grid = Grid.empty()
    grid.completedRows shouldBe empty
  }

  it should "return empty list for partially filled row" in {
    val grid = Grid.empty()
    val filled = Cell.Filled(TetrominoShape.I)
    val partialGrid = (0 until GridConfig.DefaultWidth - 1).foldLeft(grid) { (g, x) =>
      g.place(Position(x, GridConfig.DefaultHeight - 1), filled)
    }
    partialGrid.completedRows shouldBe empty
  }

  it should "detect non-consecutive completed rows" in {
    val grid = Grid.empty()
    val filled = Cell.Filled(TetrominoShape.I)
    // Fill rows 15 and 19 (with gap in between)
    val filledGrid = (0 until GridConfig.DefaultWidth).foldLeft(grid) { (g, x) =>
      g.place(Position(x, 15), filled).place(Position(x, 19), filled)
    }
    val completed = filledGrid.completedRows
    completed should contain allOf (15, 19)
    completed.size shouldBe 2
  }

  // ============================================================
  // clearRows behavior
  // ============================================================

  "Grid.clearRows" should "preserve blocks above cleared rows" in {
    val grid = Grid.empty()
    val filled = Cell.Filled(TetrominoShape.I)

    // Place a block at row 10
    val gridWithBlock = grid.place(Position(5, 10), filled)

    // Fill and clear bottom row
    val filledGrid = (0 until GridConfig.DefaultWidth).foldLeft(gridWithBlock) { (g, x) =>
      g.place(Position(x, GridConfig.DefaultHeight - 1), filled)
    }
    val clearedGrid = filledGrid.clearRows(List(GridConfig.DefaultHeight - 1))

    // Block should have shifted down by 1
    clearedGrid.isEmpty(Position(5, 10)) shouldBe true
    clearedGrid.isEmpty(Position(5, 11)) shouldBe false
  }

  it should "handle clearing multiple rows" in {
    val grid = Grid.empty()
    val filled = Cell.Filled(TetrominoShape.I)

    // Fill bottom 2 rows
    val filledGrid = (0 until GridConfig.DefaultWidth).foldLeft(grid) { (g, x) =>
      g.place(Position(x, GridConfig.DefaultHeight - 1), filled)
        .place(Position(x, GridConfig.DefaultHeight - 2), filled)
    }
    val clearedGrid = filledGrid.clearRows(List(GridConfig.DefaultHeight - 1, GridConfig.DefaultHeight - 2))

    // Both rows should be empty now (filled with new empty rows from top)
    clearedGrid.isEmpty(Position(0, GridConfig.DefaultHeight - 1)) shouldBe true
    clearedGrid.isEmpty(Position(0, GridConfig.DefaultHeight - 2)) shouldBe true
  }

  it should "handle clearing all rows" in {
    val grid = Grid.empty()
    val filled = Cell.Filled(TetrominoShape.I)

    // Fill all rows
    var filledGrid = grid
    for
      y <- 0 until GridConfig.DefaultHeight
      x <- 0 until GridConfig.DefaultWidth
    do
      filledGrid = filledGrid.place(Position(x, y), filled)

    val allRows = (0 until GridConfig.DefaultHeight).toList
    val clearedGrid = filledGrid.clearRows(allRows)

    // All cells should be empty
    val allEmpty = (for
      x <- 0 until GridConfig.DefaultWidth
      y <- 0 until GridConfig.DefaultHeight
    yield clearedGrid.isEmpty(Position(x, y))).forall(identity)

    allEmpty shouldBe true
  }

  // ============================================================
  // Different cell types
  // ============================================================

  "Grid" should "distinguish between different tetromino shapes in cells" in {
    val grid = Grid.empty()
    val cell1 = Cell.Filled(TetrominoShape.I)
    val cell2 = Cell.Filled(TetrominoShape.O)

    val newGrid = grid
      .place(Position(0, 0), cell1)
      .place(Position(1, 0), cell2)

    newGrid.get(Position(0, 0)) shouldBe Some(cell1)
    newGrid.get(Position(1, 0)) shouldBe Some(cell2)
    newGrid.get(Position(0, 0)) should not be newGrid.get(Position(1, 0))
  }

package monadris.domain

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Extended tests for Grid to improve branch coverage
 */
class GridExtendedSpec extends AnyFlatSpec with Matchers:

  val DefaultWidth  = 10
  val DefaultHeight = 20

  // ============================================================
  // Grid.empty with explicit parameters
  // ============================================================

  "Grid.empty" should "create grid with specified dimensions" in {
    val grid = Grid.empty(DefaultWidth, DefaultHeight)
    grid.width shouldBe DefaultWidth
    grid.height shouldBe DefaultHeight
  }

  it should "create grid with custom width" in {
    val grid = Grid.empty(15, DefaultHeight)
    grid.width shouldBe 15
    grid.height shouldBe DefaultHeight
  }

  it should "create grid with custom height" in {
    val grid = Grid.empty(DefaultWidth, 25)
    grid.width shouldBe DefaultWidth
    grid.height shouldBe 25
  }

  it should "create grid with custom dimensions" in {
    val grid = Grid.empty(12, 24)
    grid.width shouldBe 12
    grid.height shouldBe 24
  }

  // ============================================================
  // place with out-of-bounds positions
  // ============================================================

  "Grid.place" should "return unchanged grid for out-of-bounds position (negative x)" in {
    val grid    = Grid.empty(DefaultWidth, DefaultHeight)
    val filled  = Cell.Filled(TetrominoShape.T)
    val newGrid = grid.place(Position(-1, 5), filled)
    newGrid shouldBe grid
  }

  it should "return unchanged grid for out-of-bounds position (negative y)" in {
    val grid    = Grid.empty(DefaultWidth, DefaultHeight)
    val filled  = Cell.Filled(TetrominoShape.T)
    val newGrid = grid.place(Position(5, -1), filled)
    newGrid shouldBe grid
  }

  it should "return unchanged grid for out-of-bounds position (x >= width)" in {
    val grid    = Grid.empty(DefaultWidth, DefaultHeight)
    val filled  = Cell.Filled(TetrominoShape.T)
    val newGrid = grid.place(Position(DefaultWidth, 5), filled)
    newGrid shouldBe grid
  }

  it should "return unchanged grid for out-of-bounds position (y >= height)" in {
    val grid    = Grid.empty(DefaultWidth, DefaultHeight)
    val filled  = Cell.Filled(TetrominoShape.T)
    val newGrid = grid.place(Position(5, DefaultHeight), filled)
    newGrid shouldBe grid
  }

  // ============================================================
  // isEmpty behavior
  // ============================================================

  "Grid.isEmpty" should "return false for filled position" in {
    val grid    = Grid.empty(DefaultWidth, DefaultHeight)
    val filled  = Cell.Filled(TetrominoShape.I)
    val newGrid = grid.place(Position(5, 5), filled)
    newGrid.isEmpty(Position(5, 5)) shouldBe false
  }

  it should "return true for empty position" in {
    val grid = Grid.empty(DefaultWidth, DefaultHeight)
    grid.isEmpty(Position(5, 5)) shouldBe true
  }

  it should "return false for out-of-bounds position" in {
    val grid = Grid.empty(DefaultWidth, DefaultHeight)
    grid.isEmpty(Position(-1, 5)) shouldBe false
    grid.isEmpty(Position(5, -1)) shouldBe false
    grid.isEmpty(Position(DefaultWidth, 5)) shouldBe false
    grid.isEmpty(Position(5, DefaultHeight)) shouldBe false
  }

  // ============================================================
  // placeTetromino behavior
  // ============================================================

  "Grid.placeTetromino" should "place all 4 blocks of a tetromino" in {
    val grid      = Grid.empty(DefaultWidth, DefaultHeight)
    val tetromino = Tetromino.spawn(TetrominoShape.O, DefaultWidth)
    val newGrid   = grid.placeTetromino(tetromino)

    val filledCount = tetromino.currentBlocks.count { pos =>
      !newGrid.isEmpty(pos)
    }
    filledCount shouldBe 4
  }

  it should "fill cells with correct shape identifier" in {
    val grid      = Grid.empty(DefaultWidth, DefaultHeight)
    val tetromino = Tetromino.spawn(TetrominoShape.T, DefaultWidth)
    val newGrid   = grid.placeTetromino(tetromino)

    tetromino.currentBlocks.foreach { pos =>
      newGrid.get(pos) shouldBe Some(Cell.Filled(TetrominoShape.T))
    }
  }

  // ============================================================
  // completedRows behavior
  // ============================================================

  "Grid.completedRows" should "return empty list for empty grid" in {
    val grid = Grid.empty(DefaultWidth, DefaultHeight)
    grid.completedRows shouldBe empty
  }

  it should "return empty list for partially filled row" in {
    val grid        = Grid.empty(DefaultWidth, DefaultHeight)
    val filled      = Cell.Filled(TetrominoShape.I)
    val partialGrid = (0 until DefaultWidth - 1).foldLeft(grid) { (g, x) =>
      g.place(Position(x, DefaultHeight - 1), filled)
    }
    partialGrid.completedRows shouldBe empty
  }

  it should "detect non-consecutive completed rows" in {
    val grid   = Grid.empty(DefaultWidth, DefaultHeight)
    val filled = Cell.Filled(TetrominoShape.I)
    // Fill rows 15 and 19 (with gap in between)
    val filledGrid = (0 until DefaultWidth).foldLeft(grid) { (g, x) =>
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
    val grid   = Grid.empty(DefaultWidth, DefaultHeight)
    val filled = Cell.Filled(TetrominoShape.I)

    // Place a block at row 10
    val gridWithBlock = grid.place(Position(5, 10), filled)

    // Fill and clear bottom row
    val filledGrid = (0 until DefaultWidth).foldLeft(gridWithBlock) { (g, x) =>
      g.place(Position(x, DefaultHeight - 1), filled)
    }
    val clearedGrid = filledGrid.clearRows(List(DefaultHeight - 1))

    // Block should have shifted down by 1
    clearedGrid.isEmpty(Position(5, 10)) shouldBe true
    clearedGrid.isEmpty(Position(5, 11)) shouldBe false
  }

  it should "handle clearing multiple rows" in {
    val grid   = Grid.empty(DefaultWidth, DefaultHeight)
    val filled = Cell.Filled(TetrominoShape.I)

    // Fill bottom 2 rows
    val filledGrid = (0 until DefaultWidth).foldLeft(grid) { (g, x) =>
      g.place(Position(x, DefaultHeight - 1), filled)
        .place(Position(x, DefaultHeight - 2), filled)
    }
    val clearedGrid = filledGrid.clearRows(List(DefaultHeight - 1, DefaultHeight - 2))

    // Both rows should be empty now (filled with new empty rows from top)
    clearedGrid.isEmpty(Position(0, DefaultHeight - 1)) shouldBe true
    clearedGrid.isEmpty(Position(0, DefaultHeight - 2)) shouldBe true
  }

  it should "handle clearing all rows" in {
    val grid   = Grid.empty(DefaultWidth, DefaultHeight)
    val filled = Cell.Filled(TetrominoShape.I)

    // Fill all rows
    val filledGrid = (for
      y <- 0 until DefaultHeight
      x <- 0 until DefaultWidth
    yield Position(x, y)).foldLeft(grid) { (g, pos) =>
      g.place(pos, filled)
    }

    val allRows     = (0 until DefaultHeight).toList
    val clearedGrid = filledGrid.clearRows(allRows)

    // All cells should be empty
    val allEmpty = (for
      x <- 0 until DefaultWidth
      y <- 0 until DefaultHeight
    yield clearedGrid.isEmpty(Position(x, y))).forall(identity)

    allEmpty shouldBe true
  }

  // ============================================================
  // Different cell types
  // ============================================================

  "Grid" should "distinguish between different tetromino shapes in cells" in {
    val grid  = Grid.empty(DefaultWidth, DefaultHeight)
    val cell1 = Cell.Filled(TetrominoShape.I)
    val cell2 = Cell.Filled(TetrominoShape.O)

    val newGrid = grid
      .place(Position(0, 0), cell1)
      .place(Position(1, 0), cell2)

    newGrid.get(Position(0, 0)) shouldBe Some(cell1)
    newGrid.get(Position(1, 0)) shouldBe Some(cell2)
    newGrid.get(Position(0, 0)) should not be newGrid.get(Position(1, 0))
  }

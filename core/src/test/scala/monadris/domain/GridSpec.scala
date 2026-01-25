package monadris.domain

import monadris.TestConfig

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GridSpec extends AnyFlatSpec with Matchers:

  val gridWidth: Int  = TestConfig.testConfig.grid.width
  val gridHeight: Int = TestConfig.testConfig.grid.height

  val origin: Position            = Position(0, 0)
  val centerPosition: Position    = Position(gridWidth / 2, gridHeight / 2)
  val lastValidPosition: Position = Position(gridWidth - 1, gridHeight - 1)

  val outOfBoundsNegative: Int = -1

  "Grid" should "create empty grid with correct dimensions" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    grid.width shouldBe gridWidth
    grid.height shouldBe gridHeight
  }

  it should "return Empty for all cells in new grid" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    for
      x <- 0 until gridWidth
      y <- 0 until gridHeight
    do grid.get(Position(x, y)) shouldBe Some(Cell.Empty)
  }

  it should "return None for out-of-bounds positions" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    grid.get(Position(outOfBoundsNegative, 0)) shouldBe None
    grid.get(Position(gridWidth, 0)) shouldBe None
    grid.get(Position(0, outOfBoundsNegative)) shouldBe None
    grid.get(Position(0, gridHeight)) shouldBe None
  }

  it should "correctly identify in-bounds positions" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    grid.isInBounds(origin) shouldBe true
    grid.isInBounds(lastValidPosition) shouldBe true
    grid.isInBounds(Position(outOfBoundsNegative, 0)) shouldBe false
    grid.isInBounds(Position(gridWidth, 0)) shouldBe false
  }

  it should "correctly identify empty positions" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    grid.isEmpty(centerPosition) shouldBe true
  }

  it should "place cells correctly" in {
    val grid    = Grid.empty(gridWidth, gridHeight)
    val filled  = Cell.Filled(TetrominoShape.T)
    val newGrid = grid.place(centerPosition, filled)

    newGrid.get(centerPosition) shouldBe Some(filled)
    newGrid.isEmpty(centerPosition) shouldBe false
    grid.isEmpty(centerPosition) shouldBe true
  }

  it should "place tetromino correctly" in {
    val grid      = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino.spawn(TetrominoShape.T, gridWidth)
    val newGrid   = grid.placeTetromino(tetromino)

    tetromino.currentBlocks.foreach { pos =>
      newGrid.isEmpty(pos) shouldBe false
    }
  }

  it should "detect completed rows" in {
    val grid      = Grid.empty(gridWidth, gridHeight)
    val filled    = Cell.Filled(TetrominoShape.I)
    val bottomRow = gridHeight - 1

    val filledGrid = (0 until gridWidth).foldLeft(grid) { (g, x) =>
      g.place(Position(x, bottomRow), filled)
    }

    filledGrid.completedRows shouldBe List(bottomRow)
  }

  it should "detect multiple completed rows" in {
    val grid            = Grid.empty(gridWidth, gridHeight)
    val filled          = Cell.Filled(TetrominoShape.I)
    val bottomRow       = gridHeight - 1
    val secondBottomRow = gridHeight - 2

    val filledGrid = (0 until gridWidth).foldLeft(grid) { (g, x) =>
      g.place(Position(x, secondBottomRow), filled).place(Position(x, bottomRow), filled)
    }

    filledGrid.completedRows should contain allOf (secondBottomRow, bottomRow)
  }

  it should "clear rows and add empty rows at top" in {
    val grid      = Grid.empty(gridWidth, gridHeight)
    val filled    = Cell.Filled(TetrominoShape.I)
    val bottomRow = gridHeight - 1

    val filledGrid = (0 until gridWidth).foldLeft(grid) { (g, x) =>
      g.place(Position(x, bottomRow), filled)
    }

    val clearedGrid = filledGrid.clearRows(List(bottomRow))

    clearedGrid.isEmpty(Position(0, bottomRow)) shouldBe true
    clearedGrid.isEmpty(origin) shouldBe true
  }

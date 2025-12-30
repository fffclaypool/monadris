package monadris.domain

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GridSpec extends AnyFlatSpec with Matchers:

  val DefaultWidth = 10
  val DefaultHeight = 20

  "Grid" should "create empty grid with correct dimensions" in {
    val grid = Grid.empty(DefaultWidth, DefaultHeight)
    grid.width shouldBe DefaultWidth
    grid.height shouldBe DefaultHeight
  }

  it should "return Empty for all cells in new grid" in {
    val grid = Grid.empty(DefaultWidth, DefaultHeight)
    for
      x <- 0 until DefaultWidth
      y <- 0 until DefaultHeight
    do
      grid.get(Position(x, y)) shouldBe Some(Cell.Empty)
  }

  it should "return None for out-of-bounds positions" in {
    val grid = Grid.empty(DefaultWidth, DefaultHeight)
    grid.get(Position(-1, 0)) shouldBe None
    grid.get(Position(DefaultWidth, 0)) shouldBe None
    grid.get(Position(0, -1)) shouldBe None
    grid.get(Position(0, DefaultHeight)) shouldBe None
  }

  it should "correctly identify in-bounds positions" in {
    val grid = Grid.empty(DefaultWidth, DefaultHeight)
    grid.isInBounds(Position(0, 0)) shouldBe true
    grid.isInBounds(Position(DefaultWidth - 1, DefaultHeight - 1)) shouldBe true
    grid.isInBounds(Position(-1, 0)) shouldBe false
    grid.isInBounds(Position(DefaultWidth, 0)) shouldBe false
  }

  it should "correctly identify empty positions" in {
    val grid = Grid.empty(DefaultWidth, DefaultHeight)
    grid.isEmpty(Position(5, 5)) shouldBe true
  }

  it should "place cells correctly" in {
    val grid = Grid.empty(DefaultWidth, DefaultHeight)
    val filled = Cell.Filled(TetrominoShape.T)
    val newGrid = grid.place(Position(5, 5), filled)

    newGrid.get(Position(5, 5)) shouldBe Some(filled)
    newGrid.isEmpty(Position(5, 5)) shouldBe false
    // Original grid should be unchanged (immutability)
    grid.isEmpty(Position(5, 5)) shouldBe true
  }

  it should "place tetromino correctly" in {
    val grid = Grid.empty(DefaultWidth, DefaultHeight)
    val tetromino = Tetromino.spawn(TetrominoShape.T, DefaultWidth)
    val newGrid = grid.placeTetromino(tetromino)

    tetromino.currentBlocks.foreach { pos =>
      newGrid.isEmpty(pos) shouldBe false
    }
  }

  it should "detect completed rows" in {
    val grid = Grid.empty(DefaultWidth, DefaultHeight)
    val filled = Cell.Filled(TetrominoShape.I)
    val bottomRow = DefaultHeight - 1

    // 最下行を埋める
    val filledGrid = (0 until DefaultWidth).foldLeft(grid) { (g, x) =>
      g.place(Position(x, bottomRow), filled)
    }

    filledGrid.completedRows shouldBe List(bottomRow)
  }

  it should "detect multiple completed rows" in {
    val grid = Grid.empty(DefaultWidth, DefaultHeight)
    val filled = Cell.Filled(TetrominoShape.I)
    val bottomRow = DefaultHeight - 1
    val secondBottomRow = DefaultHeight - 2

    // 2行埋める
    val filledGrid = (0 until DefaultWidth).foldLeft(grid) { (g, x) =>
      g.place(Position(x, secondBottomRow), filled).place(Position(x, bottomRow), filled)
    }

    filledGrid.completedRows should contain allOf (secondBottomRow, bottomRow)
  }

  it should "clear rows and add empty rows at top" in {
    val grid = Grid.empty(DefaultWidth, DefaultHeight)
    val filled = Cell.Filled(TetrominoShape.I)
    val bottomRow = DefaultHeight - 1

    // 最下行を埋める
    val filledGrid = (0 until DefaultWidth).foldLeft(grid) { (g, x) =>
      g.place(Position(x, bottomRow), filled)
    }

    val clearedGrid = filledGrid.clearRows(List(bottomRow))

    // 最下行は空になるはず
    clearedGrid.isEmpty(Position(0, bottomRow)) shouldBe true
    // 最上行も空
    clearedGrid.isEmpty(Position(0, 0)) shouldBe true
  }

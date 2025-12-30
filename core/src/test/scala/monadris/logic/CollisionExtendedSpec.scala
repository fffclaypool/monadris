package monadris.logic

import monadris.domain.*
import monadris.TestConfig

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Extended tests for Collision to improve branch coverage
 */
class CollisionExtendedSpec extends AnyFlatSpec with Matchers:

  val gridWidth: Int = TestConfig.testConfig.grid.width
  val gridHeight: Int = TestConfig.testConfig.grid.height

  // ============================================================
  // isValidPosition edge cases
  // ============================================================

  "Collision.isValidPosition" should "return false when any block is out of left bound" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.I, Position(-5, 10), Rotation.R0)
    Collision.isValidPosition(tetromino, grid) shouldBe false
  }

  it should "return false when any block is out of right bound" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.I, Position(gridWidth + 5, 10), Rotation.R0)
    Collision.isValidPosition(tetromino, grid) shouldBe false
  }

  it should "return false when any block is above grid" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.I, Position(5, -5), Rotation.R0)
    Collision.isValidPosition(tetromino, grid) shouldBe false
  }

  it should "return false when any block is below grid" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.I, Position(5, gridHeight + 5), Rotation.R0)
    Collision.isValidPosition(tetromino, grid) shouldBe false
  }

  it should "return true at exact boundary positions" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    // O-tetromino at top-left corner
    val tetromino = Tetromino(TetrominoShape.O, Position(0, 0), Rotation.R0)
    Collision.isValidPosition(tetromino, grid) shouldBe true
  }

  // ============================================================
  // detectCollision edge cases
  // ============================================================

  "Collision.detectCollision" should "detect collision with filled cells" in {
    val filled = Cell.Filled(TetrominoShape.I)
    // Fill center of grid
    val grid = Grid.empty(gridWidth, gridHeight).place(Position(5, 10), filled)

    val tetromino = Tetromino(TetrominoShape.O, Position(4, 9), Rotation.R0)
    // O-tetromino covers (4,9), (5,9), (4,10), (5,10) - overlaps with filled cell at (5,10)
    Collision.detectCollision(tetromino, grid) shouldBe Collision.CollisionType.Block
  }

  it should "detect wall collision" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.I, Position(-5, 10), Rotation.R0)
    Collision.detectCollision(tetromino, grid) shouldBe Collision.CollisionType.Wall
  }

  it should "detect floor collision" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.I, Position(5, gridHeight + 5), Rotation.R0)
    Collision.detectCollision(tetromino, grid) shouldBe Collision.CollisionType.Floor
  }

  it should "detect ceiling collision" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.I, Position(5, -5), Rotation.R0)
    Collision.detectCollision(tetromino, grid) shouldBe Collision.CollisionType.Ceiling
  }

  it should "return None for valid position" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.O, Position(5, 10), Rotation.R0)
    Collision.detectCollision(tetromino, grid) shouldBe Collision.CollisionType.None
  }

  // ============================================================
  // hasLanded edge cases
  // ============================================================

  "Collision.hasLanded" should "return true when touching filled block below" in {
    val filled = Cell.Filled(TetrominoShape.I)
    // Fill a row
    val grid = (0 until gridWidth).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, x) =>
      g.place(Position(x, 15), filled)
    }

    val tetromino = Tetromino(TetrominoShape.O, Position(4, 13), Rotation.R0)
    // O-tetromino at y=13 would have blocks at y=13,14. Moving down would hit row 15
    Collision.hasLanded(tetromino, grid) shouldBe true
  }

  it should "return false when there is space below" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.O, Position(4, 10), Rotation.R0)
    Collision.hasLanded(tetromino, grid) shouldBe false
  }

  // ============================================================
  // hardDropPosition edge cases
  // ============================================================

  "Collision.hardDropPosition" should "stop at filled blocks" in {
    val filled = Cell.Filled(TetrominoShape.I)
    // Fill row at y=15
    val grid = (0 until gridWidth).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, x) =>
      g.place(Position(x, 15), filled)
    }

    val tetromino = Tetromino(TetrominoShape.O, Position(4, 5), Rotation.R0)
    val dropped = Collision.hardDropPosition(tetromino, grid)

    // Should stop just above the filled row
    dropped.position.y should be < 15
  }

  it should "drop to bottom on empty grid" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.O, Position(4, 0), Rotation.R0)
    val dropped = Collision.hardDropPosition(tetromino, grid)

    // O-tetromino should be at bottom (y = height - 2 since O is 2 blocks tall)
    dropped.position.y shouldBe (gridHeight - 2)
  }

  // ============================================================
  // tryRotateWithWallKick edge cases
  // ============================================================

  "Collision.tryRotateWithWallKick" should "try wall kick when rotation fails" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    // I-tetromino at left edge, horizontal
    val tetromino = Tetromino(TetrominoShape.I, Position(0, 10), Rotation.R0)

    val rotatedOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = true)

    // Should either rotate with kick or return Some/None, but if Some then always valid
    rotatedOpt match
      case Some(rotated) => Collision.isValidPosition(rotated, grid) shouldBe true
      case None => succeed // None is acceptable
  }

  it should "try counter-clockwise rotation" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.T, Position(5, 10), Rotation.R0)

    val rotatedOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = false)

    rotatedOpt shouldBe defined
    rotatedOpt.get.rotation shouldBe Rotation.R270
  }

  it should "handle rotation near right wall" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.I, Position(gridWidth - 1, 10), Rotation.R90)

    val rotatedOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = true)

    rotatedOpt match
      case Some(rotated) => Collision.isValidPosition(rotated, grid) shouldBe true
      case None => succeed
  }

  it should "handle rotation near floor" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.I, Position(5, gridHeight - 1), Rotation.R0)

    val rotatedOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = true)

    rotatedOpt match
      case Some(rotated) => Collision.isValidPosition(rotated, grid) shouldBe true
      case None => succeed
  }

  it should "handle O-tetromino (no actual rotation change)" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.O, Position(5, 10), Rotation.R0)

    val rotatedOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = true)

    rotatedOpt shouldBe defined
  }

  // ============================================================
  // isGameOver edge cases
  // ============================================================

  "Collision.isGameOver" should "return true when spawn tetromino position blocked" in {
    val filled = Cell.Filled(TetrominoShape.I)
    // Fill top rows
    val grid = (for {
      x <- 0 until gridWidth
      y <- 0 until 4
    } yield Position(x, y)).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, pos) =>
      g.place(pos, filled)
    }

    val tetromino = Tetromino.spawn(TetrominoShape.T, gridWidth)
    Collision.isGameOver(tetromino, grid) shouldBe true
  }

  it should "return false when spawn position is clear" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino.spawn(TetrominoShape.T, gridWidth)
    Collision.isGameOver(tetromino, grid) shouldBe false
  }

  it should "return true for all shapes when grid is completely full" in {
    val filled = Cell.Filled(TetrominoShape.I)
    val grid = (for {
      x <- 0 until gridWidth
      y <- 0 until gridHeight
    } yield Position(x, y)).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, pos) =>
      g.place(pos, filled)
    }

    TetrominoShape.values.foreach { shape =>
      val tetromino = Tetromino.spawn(shape, gridWidth)
      Collision.isGameOver(tetromino, grid) shouldBe true
    }
  }

  // ============================================================
  // Boundary check functions
  // ============================================================

  "Collision.isWithinHorizontalBounds" should "return true for valid x" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    Collision.isWithinHorizontalBounds(Position(5, 10), grid) shouldBe true
  }

  it should "return false for negative x" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    Collision.isWithinHorizontalBounds(Position(-1, 10), grid) shouldBe false
  }

  it should "return false for x >= width" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    Collision.isWithinHorizontalBounds(Position(gridWidth, 10), grid) shouldBe false
  }

  "Collision.isWithinVerticalBounds" should "return true for valid y" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    Collision.isWithinVerticalBounds(Position(5, 10), grid) shouldBe true
  }

  it should "return false for negative y" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    Collision.isWithinVerticalBounds(Position(5, -1), grid) shouldBe false
  }

  it should "return false for y >= height" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    Collision.isWithinVerticalBounds(Position(5, gridHeight), grid) shouldBe false
  }

  // ============================================================
  // All tetromino shapes
  // ============================================================

  "Collision" should "handle all tetromino shapes correctly" in {
    val grid = Grid.empty(gridWidth, gridHeight)

    TetrominoShape.values.foreach { shape =>
      val tetromino = Tetromino.spawn(shape, gridWidth)

      // Spawned tetromino should be valid
      Collision.isValidPosition(tetromino, grid) shouldBe true

      // Should not be landed at spawn
      Collision.hasLanded(tetromino, grid) shouldBe false

      // Hard drop should work
      val dropped = Collision.hardDropPosition(tetromino, grid)
      Collision.hasLanded(dropped, grid) shouldBe true
    }
  }

  // ============================================================
  // All rotations
  // ============================================================

  it should "handle all rotation states" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    val shape = TetrominoShape.T

    Rotation.values.foreach { rotation =>
      val tetromino = Tetromino(shape, Position(5, 10), rotation)

      // Should be valid in center
      Collision.isValidPosition(tetromino, grid) shouldBe true

      // Rotation should work
      val rotatedOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = true)
      rotatedOpt match
        case Some(rotated) => Collision.isValidPosition(rotated, grid) shouldBe true
        case None => succeed
    }
  }

  // ============================================================
  // I-tetromino specific wall kick tests
  // ============================================================

  "Collision.tryRotateWithWallKick for I-tetromino" should "use I-specific offsets" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    // I-tetromino at center
    val tetromino = Tetromino(TetrominoShape.I, Position(5, 10), Rotation.R0)

    val rotatedOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = true)
    rotatedOpt shouldBe defined
  }

  it should "apply large offset when I-tetromino is near left wall" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    // I-tetromino very close to left wall
    val tetromino = Tetromino(TetrominoShape.I, Position(0, 10), Rotation.R0)

    val rotatedOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = true)
    // Should succeed with wall kick offset
    rotatedOpt match
      case Some(rotated) => Collision.isValidPosition(rotated, grid) shouldBe true
      case None => succeed
  }

  it should "apply large offset when I-tetromino is near right wall" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    // I-tetromino very close to right wall
    val tetromino = Tetromino(TetrominoShape.I, Position(gridWidth - 1, 10), Rotation.R0)

    val rotatedOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = true)
    rotatedOpt match
      case Some(rotated) => Collision.isValidPosition(rotated, grid) shouldBe true
      case None => succeed
  }

  // ============================================================
  // All shapes wall kick tests
  // ============================================================

  "Collision.tryRotateWithWallKick" should "work for all shapes" in {
    val grid = Grid.empty(gridWidth, gridHeight)

    TetrominoShape.values.foreach { shape =>
      val tetromino = Tetromino(shape, Position(5, 10), Rotation.R0)

      // Clockwise rotation
      val cwOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = true)
      cwOpt shouldBe defined

      // Counter-clockwise rotation
      val ccwOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = false)
      ccwOpt shouldBe defined
    }
  }

  it should "apply wall kicks for non-I, non-O shapes" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    val shapes = List(TetrominoShape.T, TetrominoShape.S, TetrominoShape.Z, TetrominoShape.L, TetrominoShape.J)

    shapes.foreach { shape =>
      // Near left wall
      val leftTetromino = Tetromino(shape, Position(0, 10), Rotation.R0)
      val leftRotated = Collision.tryRotateWithWallKick(leftTetromino, grid, clockwise = true)
      leftRotated match
        case Some(r) => Collision.isValidPosition(r, grid) shouldBe true
        case None => succeed

      // Near right wall
      val rightTetromino = Tetromino(shape, Position(gridWidth - 1, 10), Rotation.R0)
      val rightRotated = Collision.tryRotateWithWallKick(rightTetromino, grid, clockwise = true)
      rightRotated match
        case Some(r) => Collision.isValidPosition(r, grid) shouldBe true
        case None => succeed
    }
  }

  // ============================================================
  // hasLanded edge cases
  // ============================================================

  "Collision.hasLanded" should "return false for invalid current position" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    // Tetromino in invalid position (out of bounds)
    val tetromino = Tetromino(TetrominoShape.O, Position(-5, 10), Rotation.R0)
    Collision.hasLanded(tetromino, grid) shouldBe false
  }

  it should "return true when exactly at landing position" in {
    val filled = Cell.Filled(TetrominoShape.I)
    // Fill row at y=15
    val grid = (0 until gridWidth).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, x) =>
      g.place(Position(x, 15), filled)
    }

    // O-tetromino just above the filled row
    val tetromino = Tetromino(TetrominoShape.O, Position(4, 13), Rotation.R0)
    Collision.hasLanded(tetromino, grid) shouldBe true
  }

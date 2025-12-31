package monadris.logic

import monadris.domain.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CollisionSpec extends AnyFlatSpec with Matchers:

  "Collision.isValidPosition" should "return true for tetromino in valid position" in {
    val grid      = Grid.empty(10, 20)
    val tetromino = Tetromino.spawn(TetrominoShape.T, 10)

    Collision.isValidPosition(tetromino, grid) shouldBe true
  }

  it should "return false when tetromino is out of left bound" in {
    val grid      = Grid.empty(10, 20)
    val tetromino = Tetromino(TetrominoShape.I, Position(-2, 5), Rotation.R0)

    Collision.isValidPosition(tetromino, grid) shouldBe false
  }

  it should "return false when tetromino is out of right bound" in {
    val grid      = Grid.empty(10, 20)
    val tetromino = Tetromino(TetrominoShape.I, Position(10, 5), Rotation.R0)

    Collision.isValidPosition(tetromino, grid) shouldBe false
  }

  it should "return false when tetromino is below floor" in {
    val grid      = Grid.empty(10, 20)
    val tetromino = Tetromino(TetrominoShape.T, Position(5, 20), Rotation.R0)

    Collision.isValidPosition(tetromino, grid) shouldBe false
  }

  it should "return false when tetromino overlaps with filled cell" in {
    val grid = Grid
      .empty(10, 20)
      .place(Position(5, 10), Cell.Filled(TetrominoShape.I))
    val tetromino = Tetromino(TetrominoShape.T, Position(5, 10), Rotation.R0)

    Collision.isValidPosition(tetromino, grid) shouldBe false
  }

  "Collision.detectCollision" should "return None for valid position" in {
    val grid      = Grid.empty(10, 20)
    val tetromino = Tetromino.spawn(TetrominoShape.T, 10)

    Collision.detectCollision(tetromino, grid) shouldBe Collision.CollisionType.None
  }

  it should "detect wall collision" in {
    val grid      = Grid.empty(10, 20)
    val tetromino = Tetromino(TetrominoShape.I, Position(-2, 5), Rotation.R0)

    Collision.detectCollision(tetromino, grid) shouldBe Collision.CollisionType.Wall
  }

  it should "detect floor collision" in {
    val grid      = Grid.empty(10, 20)
    val tetromino = Tetromino(TetrominoShape.T, Position(5, 20), Rotation.R0)

    Collision.detectCollision(tetromino, grid) shouldBe Collision.CollisionType.Floor
  }

  it should "detect block collision" in {
    val grid = Grid
      .empty(10, 20)
      .place(Position(5, 10), Cell.Filled(TetrominoShape.I))
    val tetromino = Tetromino(TetrominoShape.T, Position(5, 10), Rotation.R0)

    Collision.detectCollision(tetromino, grid) shouldBe Collision.CollisionType.Block
  }

  "Collision.hasLanded" should "return true when tetromino cannot move down" in {
    val grid = Grid.empty(10, 20)
    // 床の1つ上に配置（T型の底が床に接する位置）
    val tetromino = Tetromino(TetrominoShape.T, Position(5, 19), Rotation.R0)

    Collision.hasLanded(tetromino, grid) shouldBe true
  }

  it should "return false when tetromino can move down" in {
    val grid      = Grid.empty(10, 20)
    val tetromino = Tetromino.spawn(TetrominoShape.T, 10)

    Collision.hasLanded(tetromino, grid) shouldBe false
  }

  "Collision.hardDropPosition" should "drop tetromino to lowest valid position" in {
    val grid      = Grid.empty(10, 20)
    val tetromino = Tetromino.spawn(TetrominoShape.T, 10)

    val dropped = Collision.hardDropPosition(tetromino, grid)

    // ドロップ後は着地しているはず
    Collision.hasLanded(dropped, grid) shouldBe true
    // 元の位置より下にあるはず
    dropped.position.y should be > tetromino.position.y
  }

  it should "stop at filled blocks" in {
    val grid = Grid
      .empty(10, 20)
      .place(Position(5, 15), Cell.Filled(TetrominoShape.I))
    val tetromino = Tetromino.spawn(TetrominoShape.T, 10)

    val dropped = Collision.hardDropPosition(tetromino, grid)

    // ブロックの上で止まるはず
    dropped.position.y should be < 15
  }

  "Collision.tryRotateWithWallKick" should "rotate when space is available" in {
    val grid      = Grid.empty(10, 20)
    val tetromino = Tetromino(TetrominoShape.T, Position(5, 10), Rotation.R0)

    val result = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = true)

    result shouldBe defined
    result.get.rotation shouldBe Rotation.R90
  }

  it should "apply wall kick when near left wall" in {
    val grid      = Grid.empty(10, 20)
    val tetromino = Tetromino(TetrominoShape.T, Position(0, 10), Rotation.R0)

    val result = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = true)

    result shouldBe defined
  }

  "Collision.isGameOver" should "return true when spawn position is blocked" in {
    val grid = Grid
      .empty(10, 20)
      .place(Position(5, 1), Cell.Filled(TetrominoShape.I))
    val tetromino = Tetromino.spawn(TetrominoShape.T, 10)

    Collision.isGameOver(tetromino, grid) shouldBe true
  }

  it should "return false when spawn position is clear" in {
    val grid      = Grid.empty(10, 20)
    val tetromino = Tetromino.spawn(TetrominoShape.T, 10)

    Collision.isGameOver(tetromino, grid) shouldBe false
  }

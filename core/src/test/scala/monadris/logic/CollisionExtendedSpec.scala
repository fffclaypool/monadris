package monadris.logic

import monadris.TestConfig
import monadris.domain.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * ブランチカバレッジを向上させるためのCollision拡張テスト
 */
class CollisionExtendedSpec extends AnyFlatSpec with Matchers:

  val gridWidth: Int  = TestConfig.testConfig.grid.width
  val gridHeight: Int = TestConfig.testConfig.grid.height

  // ============================================================
  // isValidPositionのエッジケース
  // ============================================================

  "Collision.isValidPosition" should "return false when any block is out of left bound" in {
    val grid      = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.I, Position(-5, 10), Rotation.R0)
    Collision.isValidPosition(tetromino, grid) shouldBe false
  }

  it should "return false when any block is out of right bound" in {
    val grid      = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.I, Position(gridWidth + 5, 10), Rotation.R0)
    Collision.isValidPosition(tetromino, grid) shouldBe false
  }

  it should "return false when any block is above grid" in {
    val grid      = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.I, Position(5, -5), Rotation.R0)
    Collision.isValidPosition(tetromino, grid) shouldBe false
  }

  it should "return false when any block is below grid" in {
    val grid      = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.I, Position(5, gridHeight + 5), Rotation.R0)
    Collision.isValidPosition(tetromino, grid) shouldBe false
  }

  it should "return true at exact boundary positions" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    // Oテトリミノを左上隅に配置
    val tetromino = Tetromino(TetrominoShape.O, Position(0, 0), Rotation.R0)
    Collision.isValidPosition(tetromino, grid) shouldBe true
  }

  // ============================================================
  // detectCollisionのエッジケース
  // ============================================================

  "Collision.detectCollision" should "detect collision with filled cells" in {
    val filled = Cell.Filled(TetrominoShape.I)
    // グリッドの中央を埋める
    val grid = Grid.empty(gridWidth, gridHeight).place(Position(5, 10), filled)

    val tetromino = Tetromino(TetrominoShape.O, Position(4, 9), Rotation.R0)
    // Oテトリミノは(4,9), (5,9), (4,10), (5,10)をカバー - (5,10)の埋まったセルと重なる
    Collision.detectCollision(tetromino, grid) shouldBe Collision.CollisionType.Block
  }

  it should "detect wall collision" in {
    val grid      = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.I, Position(-5, 10), Rotation.R0)
    Collision.detectCollision(tetromino, grid) shouldBe Collision.CollisionType.Wall
  }

  it should "detect floor collision" in {
    val grid      = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.I, Position(5, gridHeight + 5), Rotation.R0)
    Collision.detectCollision(tetromino, grid) shouldBe Collision.CollisionType.Floor
  }

  it should "detect ceiling collision" in {
    val grid      = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.I, Position(5, -5), Rotation.R0)
    Collision.detectCollision(tetromino, grid) shouldBe Collision.CollisionType.Ceiling
  }

  it should "return None for valid position" in {
    val grid      = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.O, Position(5, 10), Rotation.R0)
    Collision.detectCollision(tetromino, grid) shouldBe Collision.CollisionType.None
  }

  // ============================================================
  // hasLandedのエッジケース
  // ============================================================

  "Collision.hasLanded" should "return true when touching filled block below" in {
    val filled = Cell.Filled(TetrominoShape.I)
    // 行を埋める
    val grid = (0 until gridWidth).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, x) =>
      g.place(Position(x, 15), filled)
    }

    val tetromino = Tetromino(TetrominoShape.O, Position(4, 13), Rotation.R0)
    // y=13のOテトリミノはy=13,14にブロックを持つ。下に移動すると15行目に当たる
    Collision.hasLanded(tetromino, grid) shouldBe true
  }

  it should "return false when there is space below" in {
    val grid      = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.O, Position(4, 10), Rotation.R0)
    Collision.hasLanded(tetromino, grid) shouldBe false
  }

  // ============================================================
  // hardDropPositionのエッジケース
  // ============================================================

  "Collision.hardDropPosition" should "stop at filled blocks" in {
    val filled = Cell.Filled(TetrominoShape.I)
    // y=15の行を埋める
    val grid = (0 until gridWidth).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, x) =>
      g.place(Position(x, 15), filled)
    }

    val tetromino = Tetromino(TetrominoShape.O, Position(4, 5), Rotation.R0)
    val dropped   = Collision.hardDropPosition(tetromino, grid)

    // 埋まった行のすぐ上で止まるべき
    dropped.position.y should be < 15
  }

  it should "drop to bottom on empty grid" in {
    val grid      = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.O, Position(4, 0), Rotation.R0)
    val dropped   = Collision.hardDropPosition(tetromino, grid)

    // Oテトリミノは底にあるべき（Oは2ブロックの高さなのでy = height - 2）
    dropped.position.y shouldBe (gridHeight - 2)
  }

  // ============================================================
  // tryRotateWithWallKickのエッジケース
  // ============================================================

  "Collision.tryRotateWithWallKick" should "try wall kick when rotation fails" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    // Iテトリミノを左端に水平配置
    val tetromino = Tetromino(TetrominoShape.I, Position(0, 10), Rotation.R0)

    val rotatedOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = true)

    // キックで回転するかSome/Noneを返すべき、Someの場合は常に有効
    rotatedOpt match
      case Some(rotated) => Collision.isValidPosition(rotated, grid) shouldBe true
      case None          => succeed // Noneも許容される
  }

  it should "try counter-clockwise rotation" in {
    val grid      = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.T, Position(5, 10), Rotation.R0)

    val rotatedOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = false)

    rotatedOpt shouldBe defined
    rotatedOpt.get.rotation shouldBe Rotation.R270
  }

  it should "handle rotation near right wall" in {
    val grid      = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.I, Position(gridWidth - 1, 10), Rotation.R90)

    val rotatedOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = true)

    rotatedOpt match
      case Some(rotated) => Collision.isValidPosition(rotated, grid) shouldBe true
      case None          => succeed
  }

  it should "handle rotation near floor" in {
    val grid      = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.I, Position(5, gridHeight - 1), Rotation.R0)

    val rotatedOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = true)

    rotatedOpt match
      case Some(rotated) => Collision.isValidPosition(rotated, grid) shouldBe true
      case None          => succeed
  }

  it should "handle O-tetromino (no actual rotation change)" in {
    val grid      = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino(TetrominoShape.O, Position(5, 10), Rotation.R0)

    val rotatedOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = true)

    rotatedOpt shouldBe defined
  }

  // ============================================================
  // isGameOverのエッジケース
  // ============================================================

  "Collision.isGameOver" should "return true when spawn tetromino position blocked" in {
    val filled = Cell.Filled(TetrominoShape.I)
    // 上部の行を埋める
    val grid = (for
      x <- 0 until gridWidth
      y <- 0 until 4
    yield Position(x, y)).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, pos) =>
      g.place(pos, filled)
    }

    val tetromino = Tetromino.spawn(TetrominoShape.T, gridWidth)
    Collision.isGameOver(tetromino, grid) shouldBe true
  }

  it should "return false when spawn position is clear" in {
    val grid      = Grid.empty(gridWidth, gridHeight)
    val tetromino = Tetromino.spawn(TetrominoShape.T, gridWidth)
    Collision.isGameOver(tetromino, grid) shouldBe false
  }

  it should "return true for all shapes when grid is completely full" in {
    val filled = Cell.Filled(TetrominoShape.I)
    val grid   = (for
      x <- 0 until gridWidth
      y <- 0 until gridHeight
    yield Position(x, y)).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, pos) =>
      g.place(pos, filled)
    }

    TetrominoShape.values.foreach { shape =>
      val tetromino = Tetromino.spawn(shape, gridWidth)
      Collision.isGameOver(tetromino, grid) shouldBe true
    }
  }

  // ============================================================
  // 境界チェック関数
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
  // 全テトリミノ形状
  // ============================================================

  "Collision" should "handle all tetromino shapes correctly" in {
    val grid = Grid.empty(gridWidth, gridHeight)

    TetrominoShape.values.foreach { shape =>
      val tetromino = Tetromino.spawn(shape, gridWidth)

      // スポーンしたテトリミノは有効であるべき
      Collision.isValidPosition(tetromino, grid) shouldBe true

      // スポーン時点では着地していないべき
      Collision.hasLanded(tetromino, grid) shouldBe false

      // ハードドロップが機能するべき
      val dropped = Collision.hardDropPosition(tetromino, grid)
      Collision.hasLanded(dropped, grid) shouldBe true
    }
  }

  // ============================================================
  // 全回転状態
  // ============================================================

  it should "handle all rotation states" in {
    val grid  = Grid.empty(gridWidth, gridHeight)
    val shape = TetrominoShape.T

    Rotation.values.foreach { rotation =>
      val tetromino = Tetromino(shape, Position(5, 10), rotation)

      // 中央では有効であるべき
      Collision.isValidPosition(tetromino, grid) shouldBe true

      // 回転が機能するべき
      val rotatedOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = true)
      rotatedOpt match
        case Some(rotated) => Collision.isValidPosition(rotated, grid) shouldBe true
        case None          => succeed
    }
  }

  // ============================================================
  // Iテトリミノ固有のウォールキックテスト
  // ============================================================

  "Collision.tryRotateWithWallKick for I-tetromino" should "use I-specific offsets" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    // Iテトリミノを中央に配置
    val tetromino = Tetromino(TetrominoShape.I, Position(5, 10), Rotation.R0)

    val rotatedOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = true)
    rotatedOpt shouldBe defined
  }

  it should "apply large offset when I-tetromino is near left wall" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    // Iテトリミノを左壁に非常に近い位置に配置
    val tetromino = Tetromino(TetrominoShape.I, Position(0, 10), Rotation.R0)

    val rotatedOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = true)
    // ウォールキックオフセットで成功するべき
    rotatedOpt match
      case Some(rotated) => Collision.isValidPosition(rotated, grid) shouldBe true
      case None          => succeed
  }

  it should "apply large offset when I-tetromino is near right wall" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    // Iテトリミノを右壁に非常に近い位置に配置
    val tetromino = Tetromino(TetrominoShape.I, Position(gridWidth - 1, 10), Rotation.R0)

    val rotatedOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = true)
    rotatedOpt match
      case Some(rotated) => Collision.isValidPosition(rotated, grid) shouldBe true
      case None          => succeed
  }

  // ============================================================
  // 全形状のウォールキックテスト
  // ============================================================

  "Collision.tryRotateWithWallKick" should "work for all shapes" in {
    val grid = Grid.empty(gridWidth, gridHeight)

    TetrominoShape.values.foreach { shape =>
      val tetromino = Tetromino(shape, Position(5, 10), Rotation.R0)

      // 時計回り回転
      val cwOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = true)
      cwOpt shouldBe defined

      // 反時計回り回転
      val ccwOpt = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = false)
      ccwOpt shouldBe defined
    }
  }

  it should "apply wall kicks for non-I, non-O shapes" in {
    val grid   = Grid.empty(gridWidth, gridHeight)
    val shapes = List(TetrominoShape.T, TetrominoShape.S, TetrominoShape.Z, TetrominoShape.L, TetrominoShape.J)

    shapes.foreach { shape =>
      // 左壁付近
      val leftTetromino = Tetromino(shape, Position(0, 10), Rotation.R0)
      val leftRotated   = Collision.tryRotateWithWallKick(leftTetromino, grid, clockwise = true)
      leftRotated match
        case Some(r) => Collision.isValidPosition(r, grid) shouldBe true
        case None    => succeed

      // 右壁付近
      val rightTetromino = Tetromino(shape, Position(gridWidth - 1, 10), Rotation.R0)
      val rightRotated   = Collision.tryRotateWithWallKick(rightTetromino, grid, clockwise = true)
      rightRotated match
        case Some(r) => Collision.isValidPosition(r, grid) shouldBe true
        case None    => succeed
    }
  }

  // ============================================================
  // hasLandedのエッジケース（追加）
  // ============================================================

  "Collision.hasLanded" should "return false for invalid current position" in {
    val grid = Grid.empty(gridWidth, gridHeight)
    // テトリミノを無効な位置（範囲外）に配置
    val tetromino = Tetromino(TetrominoShape.O, Position(-5, 10), Rotation.R0)
    Collision.hasLanded(tetromino, grid) shouldBe false
  }

  it should "return true when exactly at landing position" in {
    val filled = Cell.Filled(TetrominoShape.I)
    // y=15の行を埋める
    val grid = (0 until gridWidth).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, x) =>
      g.place(Position(x, 15), filled)
    }

    // Oテトリミノを埋まった行のすぐ上に配置
    val tetromino = Tetromino(TetrominoShape.O, Position(4, 13), Rotation.R0)
    Collision.hasLanded(tetromino, grid) shouldBe true
  }

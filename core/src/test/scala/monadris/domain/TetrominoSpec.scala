package monadris.domain

import monadris.TestConfig

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TetrominoSpec extends AnyFlatSpec with Matchers:

  // Use configuration values
  val gridWidth: Int = TestConfig.testConfig.grid.width

  // Domain constants
  val blocksPerTetromino: Int = 4
  val spawnYPosition: Int = 1  // Tetrominoes spawn at y=1

  "Position" should "support addition" in {
    val p1 = Position(1, 2)
    val p2 = Position(3, 4)
    val expectedSum = Position(p1.x + p2.x, p1.y + p2.y)
    (p1 + p2) shouldBe expectedSum
  }

  it should "support subtraction" in {
    val p1 = Position(5, 7)
    val p2 = Position(2, 3)
    val expectedDiff = Position(p1.x - p2.x, p1.y - p2.y)
    (p1 - p2) shouldBe expectedDiff
  }

  "Rotation" should "rotate clockwise correctly" in {
    Rotation.R0.rotateClockwise shouldBe Rotation.R90
    Rotation.R90.rotateClockwise shouldBe Rotation.R180
    Rotation.R180.rotateClockwise shouldBe Rotation.R270
    Rotation.R270.rotateClockwise shouldBe Rotation.R0
  }

  it should "rotate counter-clockwise correctly" in {
    Rotation.R0.rotateCounterClockwise shouldBe Rotation.R270
    Rotation.R90.rotateCounterClockwise shouldBe Rotation.R0
    Rotation.R180.rotateCounterClockwise shouldBe Rotation.R90
    Rotation.R270.rotateCounterClockwise shouldBe Rotation.R180
  }

  "TetrominoShape" should "have correct block counts" in {
    TetrominoShape.values.foreach { shape =>
      shape.blocks.size shouldBe blocksPerTetromino
    }
  }

  "Tetromino" should "spawn at center of grid" in {
    val tetromino = Tetromino.spawn(TetrominoShape.I, gridWidth)
    val expectedCenterX = gridWidth / 2
    tetromino.position.x shouldBe expectedCenterX
    tetromino.position.y shouldBe spawnYPosition
    tetromino.rotation shouldBe Rotation.R0
  }

  it should "move left correctly" in {
    val tetromino = Tetromino.spawn(TetrominoShape.T, gridWidth)
    val moved = tetromino.moveLeft
    moved.position.x shouldBe (tetromino.position.x - 1)
    moved.position.y shouldBe tetromino.position.y
  }

  it should "move right correctly" in {
    val tetromino = Tetromino.spawn(TetrominoShape.T, gridWidth)
    val moved = tetromino.moveRight
    moved.position.x shouldBe (tetromino.position.x + 1)
    moved.position.y shouldBe tetromino.position.y
  }

  it should "move down correctly" in {
    val tetromino = Tetromino.spawn(TetrominoShape.T, gridWidth)
    val moved = tetromino.moveDown
    moved.position.x shouldBe tetromino.position.x
    moved.position.y shouldBe (tetromino.position.y + 1)
  }

  it should "rotate and produce 4 blocks" in {
    val tetromino = Tetromino.spawn(TetrominoShape.T, gridWidth)

    val r0 = tetromino.currentBlocks
    val r90 = tetromino.rotateClockwise.currentBlocks
    val r180 = tetromino.rotateClockwise.rotateClockwise.currentBlocks
    val r270 = tetromino.rotateCounterClockwise.currentBlocks

    r0.size shouldBe blocksPerTetromino
    r90.size shouldBe blocksPerTetromino
    r180.size shouldBe blocksPerTetromino
    r270.size shouldBe blocksPerTetromino
  }

  it should "have different block positions after rotation (except O)" in {
    val tetromino = Tetromino.spawn(TetrominoShape.T, gridWidth)
    val rotated = tetromino.rotateClockwise

    tetromino.currentBlocks.toSet should not be rotated.currentBlocks.toSet
  }

  "O-shaped Tetromino" should "have same relative shape after rotation" in {
    val tetromino = Tetromino.spawn(TetrominoShape.O, gridWidth)
    val rotated = tetromino.rotateClockwise

    // O型は回転しても相対的な形状は同じ（2x2の正方形）
    // ブロック間の相対位置を確認
    def relativePositions(blocks: List[Position]): Set[(Int, Int)] =
      val minX = blocks.map(_.x).min
      val minY = blocks.map(_.y).min
      blocks.map(p => (p.x - minX, p.y - minY)).toSet

    relativePositions(tetromino.currentBlocks) shouldBe relativePositions(rotated.currentBlocks)
  }

  // ============================================================
  // Direct blocks() method tests for each shape
  // ============================================================

  "TetrominoShape.I" should "have correct block positions" in {
    val blocks = TetrominoShape.I.blocks
    blocks.size shouldBe blocksPerTetromino
    blocks should contain(Position(-1, 0))
    blocks should contain(Position(0, 0))
    blocks should contain(Position(1, 0))
    blocks should contain(Position(2, 0))
  }

  "TetrominoShape.O" should "have correct block positions" in {
    val blocks = TetrominoShape.O.blocks
    blocks.size shouldBe blocksPerTetromino
    blocks should contain(Position(0, 0))
    blocks should contain(Position(1, 0))
    blocks should contain(Position(0, 1))
    blocks should contain(Position(1, 1))
  }

  "TetrominoShape.T" should "have correct block positions" in {
    val blocks = TetrominoShape.T.blocks
    blocks.size shouldBe blocksPerTetromino
    blocks should contain(Position(-1, 0))
    blocks should contain(Position(0, 0))
    blocks should contain(Position(1, 0))
    blocks should contain(Position(0, -1))
  }

  "TetrominoShape.S" should "have correct block positions" in {
    val blocks = TetrominoShape.S.blocks
    blocks.size shouldBe blocksPerTetromino
    blocks should contain(Position(-1, 0))
    blocks should contain(Position(0, 0))
    blocks should contain(Position(0, -1))
    blocks should contain(Position(1, -1))
  }

  "TetrominoShape.Z" should "have correct block positions" in {
    val blocks = TetrominoShape.Z.blocks
    blocks.size shouldBe blocksPerTetromino
    blocks should contain(Position(-1, -1))
    blocks should contain(Position(0, -1))
    blocks should contain(Position(0, 0))
    blocks should contain(Position(1, 0))
  }

  "TetrominoShape.J" should "have correct block positions" in {
    val blocks = TetrominoShape.J.blocks
    blocks.size shouldBe blocksPerTetromino
    blocks should contain(Position(-1, -1))
    blocks should contain(Position(-1, 0))
    blocks should contain(Position(0, 0))
    blocks should contain(Position(1, 0))
  }

  "TetrominoShape.L" should "have correct block positions" in {
    val blocks = TetrominoShape.L.blocks
    blocks.size shouldBe blocksPerTetromino
    blocks should contain(Position(-1, 0))
    blocks should contain(Position(0, 0))
    blocks should contain(Position(1, 0))
    blocks should contain(Position(1, -1))
  }

  // ============================================================
  // Rotation transformation tests for all shapes
  // ============================================================

  "All TetrominoShapes" should "have 4 blocks in all rotation states" in {
    for
      shape <- TetrominoShape.values
      rotation <- Rotation.values
    do
      val tetromino = Tetromino(shape, Position(5, 5), rotation)
      tetromino.currentBlocks.size shouldBe blocksPerTetromino
  }

  it should "produce different absolute positions for different rotations (except O)" in {
    val nonOShapes = TetrominoShape.values.filterNot(_ == TetrominoShape.O)
    for shape <- nonOShapes do
      val t0 = Tetromino(shape, Position(5, 5), Rotation.R0)
      val t90 = Tetromino(shape, Position(5, 5), Rotation.R90)
      t0.currentBlocks.toSet should not be t90.currentBlocks.toSet
  }

  "Tetromino rotateBlocks" should "apply R90 rotation correctly" in {
    val tetromino = Tetromino.spawn(TetrominoShape.I, gridWidth)
    val r0Blocks = tetromino.currentBlocks
    val r90Blocks = tetromino.rotateClockwise.currentBlocks
    // I-piece rotates from horizontal to vertical
    r0Blocks.toSet should not be r90Blocks.toSet
  }

  it should "apply R180 rotation correctly" in {
    val tetromino = Tetromino.spawn(TetrominoShape.T, gridWidth)
    val r180 = tetromino.rotateClockwise.rotateClockwise
    r180.rotation shouldBe Rotation.R180
    r180.currentBlocks.size shouldBe blocksPerTetromino
  }

  it should "apply R270 rotation correctly" in {
    val tetromino = Tetromino.spawn(TetrominoShape.T, gridWidth)
    val r270 = tetromino.rotateCounterClockwise
    r270.rotation shouldBe Rotation.R270
    r270.currentBlocks.size shouldBe blocksPerTetromino
  }

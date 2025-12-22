package monadris.domain

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TetrominoSpec extends AnyFlatSpec with Matchers:

  "Position" should "support addition" in {
    val p1 = Position(1, 2)
    val p2 = Position(3, 4)
    (p1 + p2) shouldBe Position(4, 6)
  }

  it should "support subtraction" in {
    val p1 = Position(5, 7)
    val p2 = Position(2, 3)
    (p1 - p2) shouldBe Position(3, 4)
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
      shape.blocks.size shouldBe 4
    }
  }

  "Tetromino" should "spawn at center of grid" in {
    val tetromino = Tetromino.spawn(TetrominoShape.I, 10)
    tetromino.position.x shouldBe 5
    tetromino.position.y shouldBe 1
    tetromino.rotation shouldBe Rotation.R0
  }

  it should "move left correctly" in {
    val tetromino = Tetromino.spawn(TetrominoShape.T, 10)
    val moved = tetromino.moveLeft
    moved.position.x shouldBe (tetromino.position.x - 1)
    moved.position.y shouldBe tetromino.position.y
  }

  it should "move right correctly" in {
    val tetromino = Tetromino.spawn(TetrominoShape.T, 10)
    val moved = tetromino.moveRight
    moved.position.x shouldBe (tetromino.position.x + 1)
    moved.position.y shouldBe tetromino.position.y
  }

  it should "move down correctly" in {
    val tetromino = Tetromino.spawn(TetrominoShape.T, 10)
    val moved = tetromino.moveDown
    moved.position.x shouldBe tetromino.position.x
    moved.position.y shouldBe (tetromino.position.y + 1)
  }

  it should "rotate and produce 4 blocks" in {
    val tetromino = Tetromino.spawn(TetrominoShape.T, 10)

    val r0 = tetromino.currentBlocks
    val r90 = tetromino.rotateClockwise.currentBlocks
    val r180 = tetromino.rotateClockwise.rotateClockwise.currentBlocks
    val r270 = tetromino.rotateCounterClockwise.currentBlocks

    r0.size shouldBe 4
    r90.size shouldBe 4
    r180.size shouldBe 4
    r270.size shouldBe 4
  }

  it should "have different block positions after rotation (except O)" in {
    val tetromino = Tetromino.spawn(TetrominoShape.T, 10)
    val rotated = tetromino.rotateClockwise

    tetromino.currentBlocks.toSet should not be rotated.currentBlocks.toSet
  }

  "O-shaped Tetromino" should "have same relative shape after rotation" in {
    val tetromino = Tetromino.spawn(TetrominoShape.O, 10)
    val rotated = tetromino.rotateClockwise

    // O型は回転しても相対的な形状は同じ（2x2の正方形）
    // ブロック間の相対位置を確認
    def relativePositions(blocks: List[Position]): Set[(Int, Int)] =
      val minX = blocks.map(_.x).min
      val minY = blocks.map(_.y).min
      blocks.map(p => (p.x - minX, p.y - minY)).toSet

    relativePositions(tetromino.currentBlocks) shouldBe relativePositions(rotated.currentBlocks)
  }

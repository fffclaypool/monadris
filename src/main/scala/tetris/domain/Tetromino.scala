package tetris.domain

/**
 * 回転状態（4方向）
 */
enum Rotation:
  case R0, R90, R180, R270

  def rotateClockwise: Rotation = this match
    case R0   => R90
    case R90  => R180
    case R180 => R270
    case R270 => R0

  def rotateCounterClockwise: Rotation = this match
    case R0   => R270
    case R90  => R0
    case R180 => R90
    case R270 => R180

/**
 * テトリミノの種類（7種類）
 * 各形状はR0（初期状態）での相対座標として定義
 */
enum TetrominoShape:
  case I, O, T, S, Z, J, L

  /**
   * R0状態での相対座標（ピボット基準）
   * Standard Rotation System (SRS) に準拠
   */
  def blocks: List[Position] = this match
    case I => List(Position(-1, 0), Position(0, 0), Position(1, 0), Position(2, 0))
    case O => List(Position(0, 0), Position(1, 0), Position(0, 1), Position(1, 1))
    case T => List(Position(-1, 0), Position(0, 0), Position(1, 0), Position(0, -1))
    case S => List(Position(-1, 0), Position(0, 0), Position(0, -1), Position(1, -1))
    case Z => List(Position(-1, -1), Position(0, -1), Position(0, 0), Position(1, 0))
    case J => List(Position(-1, -1), Position(-1, 0), Position(0, 0), Position(1, 0))
    case L => List(Position(-1, 0), Position(0, 0), Position(1, 0), Position(1, -1))

/**
 * 現在のテトリミノの状態
 * shape: テトリミノの種類
 * position: グリッド上の位置（ピボット位置）
 * rotation: 現在の回転状態
 */
final case class Tetromino(
  shape: TetrominoShape,
  position: Position,
  rotation: Rotation
):
  /**
   * 回転を考慮した現在のブロック座標を計算
   */
  def currentBlocks: List[Position] =
    val baseBlocks = shape.blocks
    val rotatedBlocks = rotateBlocks(baseBlocks, rotation)
    rotatedBlocks.map(_ + position)

  /**
   * ブロックを指定された回転状態に変換
   */
  private def rotateBlocks(blocks: List[Position], rot: Rotation): List[Position] =
    rot match
      case Rotation.R0   => blocks
      case Rotation.R90  => blocks.map(p => Position(-p.y, p.x))
      case Rotation.R180 => blocks.map(p => Position(-p.x, -p.y))
      case Rotation.R270 => blocks.map(p => Position(p.y, -p.x))

  def moveLeft: Tetromino = copy(position = position + Position(-1, 0))
  def moveRight: Tetromino = copy(position = position + Position(1, 0))
  def moveDown: Tetromino = copy(position = position + Position(0, 1))
  def rotateClockwise: Tetromino = copy(rotation = rotation.rotateClockwise)
  def rotateCounterClockwise: Tetromino = copy(rotation = rotation.rotateCounterClockwise)

object Tetromino:
  /**
   * 初期位置でテトリミノを生成
   */
  def spawn(shape: TetrominoShape, gridWidth: Int = 10): Tetromino =
    Tetromino(
      shape = shape,
      position = Position(gridWidth / 2, 1),
      rotation = Rotation.R0
    )

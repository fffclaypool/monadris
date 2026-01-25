package monadris.domain

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

enum TetrominoShape:
  case I, O, T, S, Z, J, L

  /** SRS準拠のR0状態での相対座標 */
  def blocks: List[Position] = this match
    case I => List(Position(-1, 0), Position(0, 0), Position(1, 0), Position(2, 0))
    case O => List(Position(0, 0), Position(1, 0), Position(0, 1), Position(1, 1))
    case T => List(Position(-1, 0), Position(0, 0), Position(1, 0), Position(0, -1))
    case S => List(Position(-1, 0), Position(0, 0), Position(0, -1), Position(1, -1))
    case Z => List(Position(-1, -1), Position(0, -1), Position(0, 0), Position(1, 0))
    case J => List(Position(-1, -1), Position(-1, 0), Position(0, 0), Position(1, 0))
    case L => List(Position(-1, 0), Position(0, 0), Position(1, 0), Position(1, -1))

final case class Tetromino(
  shape: TetrominoShape,
  position: Position,
  rotation: Rotation
):
  def currentBlocks: List[Position] =
    val baseBlocks    = shape.blocks
    val rotatedBlocks = rotateBlocks(baseBlocks, rotation)
    rotatedBlocks.map(_ + position)

  private def rotateBlocks(blocks: List[Position], rot: Rotation): List[Position] =
    rot match
      case Rotation.R0   => blocks
      case Rotation.R90  => blocks.map(p => Position(-p.y, p.x))
      case Rotation.R180 => blocks.map(p => Position(-p.x, -p.y))
      case Rotation.R270 => blocks.map(p => Position(p.y, -p.x))

  def moveLeft: Tetromino               = copy(position = position + Position(-1, 0))
  def moveRight: Tetromino              = copy(position = position + Position(1, 0))
  def moveDown: Tetromino               = copy(position = position + Position(0, 1))
  def rotateClockwise: Tetromino        = copy(rotation = rotation.rotateClockwise)
  def rotateCounterClockwise: Tetromino = copy(rotation = rotation.rotateCounterClockwise)

object Tetromino:
  def spawn(shape: TetrominoShape, gridWidth: Int = 10): Tetromino =
    Tetromino(
      shape = shape,
      position = Position(gridWidth / 2, 1),
      rotation = Rotation.R0
    )

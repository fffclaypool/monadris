package monadris.domain.model.piece

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

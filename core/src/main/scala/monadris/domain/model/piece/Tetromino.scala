package monadris.domain.model.piece

import monadris.domain.model.board.Position

/**
 * テトリミノの種類（7種類）
 * 各形状はR0（初期状態）での相対座標として定義
 * Standard Rotation System (SRS) に準拠
 */
enum TetrominoShape:
  case I, O, T, S, Z, J, L

  /**
   * R0状態での相対座標（ピボット基準）
   */
  def blocks: List[Position] = this match
    case I => List(Position(-1, 0), Position(0, 0), Position(1, 0), Position(2, 0))
    case O => List(Position(0, 0), Position(1, 0), Position(0, 1), Position(1, 1))
    case T => List(Position(-1, 0), Position(0, 0), Position(1, 0), Position(0, -1))
    case S => List(Position(-1, 0), Position(0, 0), Position(0, -1), Position(1, -1))
    case Z => List(Position(-1, -1), Position(0, -1), Position(0, 0), Position(1, 0))
    case J => List(Position(-1, -1), Position(-1, 0), Position(0, 0), Position(1, 0))
    case L => List(Position(-1, 0), Position(0, 0), Position(1, 0), Position(1, -1))

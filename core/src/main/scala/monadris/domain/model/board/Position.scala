package monadris.domain.model.board

/**
 * 2次元座標を表す値オブジェクト
 */
final case class Position(x: Int, y: Int):
  def +(other: Position): Position = Position(x + other.x, y + other.y)
  def -(other: Position): Position = Position(x - other.x, y - other.y)

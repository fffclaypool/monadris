package monadris.domain

/**
 * 2次元座標を表す不変データ構造
 * x: 列（左が0）
 * y: 行（上が0）
 */
final case class Position(x: Int, y: Int):
  def +(other: Position): Position = Position(x + other.x, y + other.y)
  def -(other: Position): Position = Position(x - other.x, y - other.y)

object Position:
  val Origin: Position = Position(0, 0)

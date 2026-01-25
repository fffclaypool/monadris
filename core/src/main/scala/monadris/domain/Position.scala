package monadris.domain

final case class Position(x: Int, y: Int):
  def +(other: Position): Position = Position(x + other.x, y + other.y)
  def -(other: Position): Position = Position(x - other.x, y - other.y)

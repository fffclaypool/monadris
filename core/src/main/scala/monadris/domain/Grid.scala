package monadris.domain

enum Cell:
  case Empty
  case Filled(shape: TetrominoShape)

final case class Grid private (
  cells: Vector[Vector[Cell]],
  width: Int,
  height: Int
):
  def get(pos: Position): Option[Cell] =
    if isInBounds(pos) then Some(cells(pos.y)(pos.x))
    else None

  def isInBounds(pos: Position): Boolean =
    pos.x >= 0 && pos.x < width && pos.y >= 0 && pos.y < height

  def isEmpty(pos: Position): Boolean =
    get(pos).contains(Cell.Empty)

  def place(pos: Position, cell: Cell): Grid =
    if isInBounds(pos) then copy(cells = cells.updated(pos.y, cells(pos.y).updated(pos.x, cell)))
    else this

  def placeTetromino(tetromino: Tetromino): Grid =
    val cell = Cell.Filled(tetromino.shape)
    tetromino.currentBlocks.foldLeft(this)((grid, pos) => grid.place(pos, cell))

  def completedRows: List[Int] =
    cells.zipWithIndex.collect {
      case (row, idx) if row.forall(_ != Cell.Empty) => idx
    }.toList

  def clearRows(rowIndices: List[Int]): Grid =
    val sortedIndices = rowIndices.sorted.reverse
    val clearedCells  = sortedIndices.foldLeft(cells) { (acc, idx) =>
      acc.patch(idx, Nil, 1)
    }
    val emptyRow = Vector.fill(width)(Cell.Empty)
    val newRows  = Vector.fill(rowIndices.size)(emptyRow)
    copy(cells = newRows ++ clearedCells)

object Grid:
  def empty(width: Int, height: Int): Grid =
    val cells = Vector.fill(height)(Vector.fill(width)(Cell.Empty))
    Grid(cells, width, height)

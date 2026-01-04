package monadris.domain

/**
 * セルの状態
 */
enum Cell:
  case Empty
  case Filled(shape: TetrominoShape)

/**
 * 10x20のグリッド（盤面）を表す不変データ構造
 * cells: 行優先の2次元配列（Vector[Vector[Cell]]）
 */
final case class Grid private (
  cells: Vector[Vector[Cell]],
  width: Int,
  height: Int
):
  /**
   * 指定座標のセルを取得
   */
  def get(pos: Position): Option[Cell] =
    if isInBounds(pos) then Some(cells(pos.y)(pos.x))
    else None

  /**
   * 座標が盤面内かどうか
   */
  def isInBounds(pos: Position): Boolean =
    pos.x >= 0 && pos.x < width && pos.y >= 0 && pos.y < height

  /**
   * 指定座標が空かどうか
   */
  def isEmpty(pos: Position): Boolean =
    get(pos).contains(Cell.Empty)

  /**
   * 指定座標にブロックを配置した新しいグリッドを返す
   */
  def place(pos: Position, cell: Cell): Grid =
    if isInBounds(pos) then copy(cells = cells.updated(pos.y, cells(pos.y).updated(pos.x, cell)))
    else this

  /**
   * テトリミノを固定配置
   */
  def placeTetromino(tetromino: Tetromino): Grid =
    val cell = Cell.Filled(tetromino.shape)
    tetromino.currentBlocks.foldLeft(this)((grid, pos) => grid.place(pos, cell))

  /**
   * 揃った行のインデックスを取得
   */
  def completedRows: List[Int] =
    cells.zipWithIndex.collect {
      case (row, idx) if row.forall(_ != Cell.Empty) => idx
    }.toList

  /**
   * 指定行を削除し、上から空行を追加
   */
  def clearRows(rowIndices: List[Int]): Grid =
    val sortedIndices = rowIndices.sorted.reverse
    val clearedCells  = sortedIndices.foldLeft(cells) { (acc, idx) =>
      acc.patch(idx, Nil, 1)
    }
    val emptyRow = Vector.fill(width)(Cell.Empty)
    val newRows  = Vector.fill(rowIndices.size)(emptyRow)
    copy(cells = newRows ++ clearedCells)

object Grid:
  /**
   * 空のグリッドを生成
   */
  def empty(width: Int, height: Int): Grid =
    val cells = Vector.fill(height)(Vector.fill(width)(Cell.Empty))
    Grid(cells, width, height)

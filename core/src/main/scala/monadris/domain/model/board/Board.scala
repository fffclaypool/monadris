package monadris.domain.model.board

import monadris.domain.model.piece.TetrominoShape

/**
 * 盤面を表す集約
 * Grid + Collision のロジックを統合
 */
final case class Board private (
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
   * ブロック群を配置可能か判定
   * (旧 Collision.isValidPosition)
   */
  def canPlace(blocks: List[Position]): Boolean =
    blocks.forall { pos =>
      pos.x >= 0 && pos.x < width &&
      pos.y >= 0 && pos.y < height &&
      isEmpty(pos)
    }

  /**
   * 指定座標にブロックを配置した新しいBoardを返す
   */
  def placeCell(pos: Position, cell: Cell): Board =
    if isInBounds(pos) then copy(cells = cells.updated(pos.y, cells(pos.y).updated(pos.x, cell)))
    else this

  /**
   * ブロック群を配置した新しいBoardを返す
   */
  def place(blocks: List[Position], shape: TetrominoShape): Board =
    val cell = Cell.Filled(shape)
    blocks.foldLeft(this)((board, pos) => board.placeCell(pos, cell))

  /**
   * ハードドロップ時の落下距離を計算
   * (旧 Collision.hardDropPosition の一部)
   */
  def dropDistance(blocks: List[Position]): Int =
    @annotation.tailrec
    def findDistance(distance: Int): Int =
      val movedBlocks = blocks.map(p => Position(p.x, p.y + distance + 1))
      if canPlace(movedBlocks) then findDistance(distance + 1)
      else distance
    findDistance(0)

  /**
   * 完成した行のインデックスを取得
   */
  def completedRows: List[Int] =
    cells.zipWithIndex
      .collect:
        case (row, idx) if row.forall(_ != Cell.Empty) => idx
      .toList

  /**
   * 完成した行を消去し、消去行数を返す
   */
  def clearCompletedRows(): (Board, Int) =
    val rowsToClear = completedRows
    val count       = rowsToClear.size
    if count == 0 then (this, 0)
    else
      val sortedIndices = rowsToClear.sorted.reverse
      val clearedCells = sortedIndices.foldLeft(cells): (acc, idx) =>
        acc.patch(idx, Nil, 1)
      val emptyRow = Vector.fill(width)(Cell.Empty)
      val newRows  = Vector.fill(count)(emptyRow)
      (copy(cells = newRows ++ clearedCells), count)

  /**
   * スポーン位置でのゲームオーバー判定
   */
  def isBlocked(blocks: List[Position]): Boolean =
    !canPlace(blocks)

object Board:
  /**
   * 空の盤面を生成
   */
  def empty(width: Int, height: Int): Board =
    val cells = Vector.fill(height)(Vector.fill(width)(Cell.Empty))
    Board(cells, width, height)

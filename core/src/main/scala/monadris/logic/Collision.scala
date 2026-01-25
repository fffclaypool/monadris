package monadris.logic

import monadris.domain.*

object Collision:

  enum CollisionType:
    case None, Wall, Floor, Block, Ceiling

  def isValidPosition(tetromino: Tetromino, grid: Grid): Boolean =
    tetromino.currentBlocks.forall { pos =>
      isWithinHorizontalBounds(pos, grid) &&
      isWithinVerticalBounds(pos, grid) &&
      grid.isEmpty(pos)
    }

  def isWithinHorizontalBounds(pos: Position, grid: Grid): Boolean =
    pos.x >= 0 && pos.x < grid.width

  def isWithinVerticalBounds(pos: Position, grid: Grid): Boolean =
    pos.y >= 0 && pos.y < grid.height

  def detectCollision(tetromino: Tetromino, grid: Grid): CollisionType =
    val blocks = tetromino.currentBlocks
    if blocks.exists(p => p.x < 0 || p.x >= grid.width) then CollisionType.Wall
    else if blocks.exists(p => p.y >= grid.height) then CollisionType.Floor
    else if blocks.exists(p => p.y < 0) then CollisionType.Ceiling
    else if blocks.exists(p => !grid.isEmpty(p)) then CollisionType.Block
    else CollisionType.None

  /** 現在位置は有効だが、1つ下に移動すると衝突する場合にtrue */
  def hasLanded(tetromino: Tetromino, grid: Grid): Boolean =
    isValidPosition(tetromino, grid) &&
      !isValidPosition(tetromino.moveDown, grid)

  def hardDropPosition(tetromino: Tetromino, grid: Grid): Tetromino =
    @annotation.tailrec
    def drop(t: Tetromino): Tetromino =
      val next = t.moveDown
      if isValidPosition(next, grid) then drop(next)
      else t
    drop(tetromino)

  /** SRSウォールキック: 回転が直接できない場合、位置をずらして試行 */
  def tryRotateWithWallKick(
    tetromino: Tetromino,
    grid: Grid,
    clockwise: Boolean
  ): Option[Tetromino] =
    val rotated =
      if clockwise then tetromino.rotateClockwise
      else tetromino.rotateCounterClockwise

    val offsets = tetromino.shape match
      case TetrominoShape.I =>
        List(
          Position(0, 0),
          Position(-2, 0),
          Position(2, 0),
          Position(-2, 1),
          Position(2, -1)
        )
      case TetrominoShape.O => List(Position(0, 0))
      case _                =>
        List(
          Position(0, 0),
          Position(-1, 0),
          Position(1, 0),
          Position(0, -1),
          Position(-1, -1),
          Position(1, -1)
        )

    offsets.view
      .map(offset => rotated.copy(position = rotated.position + offset))
      .find(isValidPosition(_, grid))

  def isGameOver(tetromino: Tetromino, grid: Grid): Boolean =
    !isValidPosition(tetromino, grid)

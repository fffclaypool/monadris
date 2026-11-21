package monadris.game

import monadris.domain.*

object Collision:

  def isValidPosition(tetromino: Tetromino, grid: Grid): Boolean =
    grid.canPlace(tetromino)

  def isWithinHorizontalBounds(pos: Position, grid: Grid): Boolean =
    pos.x >= 0 && pos.x < grid.width

  def isWithinVerticalBounds(pos: Position, grid: Grid): Boolean =
    pos.y >= 0 && pos.y < grid.height

  def hardDropPosition(tetromino: Tetromino, grid: Grid): Tetromino =
    grid.dropToBottom(tetromino)

  /**
   * Wall kick: when the rotated piece does not fit, tries the offsets below in order.
   * This is a simplified table (the same for every rotation state), not the SRS kick data.
   */
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

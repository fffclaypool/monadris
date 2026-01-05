package monadris.domain.model.piece

import monadris.domain.model.board.Board
import monadris.domain.model.board.Position

/**
 * 落下中のピースを表す集約
 * 移動・回転ロジックを内包
 */
final case class ActivePiece(
  shape: TetrominoShape,
  position: Position,
  rotation: Rotation
):
  /**
   * 回転を考慮した現在のブロック座標を計算
   */
  def blocks: List[Position] =
    val baseBlocks    = shape.blocks
    val rotatedBlocks = rotateBlocks(baseBlocks, rotation)
    rotatedBlocks.map(_ + position)

  /**
   * ブロックを指定された回転状態に変換
   */
  private def rotateBlocks(blocks: List[Position], rot: Rotation): List[Position] =
    rot match
      case Rotation.R0   => blocks
      case Rotation.R90  => blocks.map(p => Position(-p.y, p.x))
      case Rotation.R180 => blocks.map(p => Position(-p.x, -p.y))
      case Rotation.R270 => blocks.map(p => Position(p.y, -p.x))

  def moveLeft: ActivePiece  = copy(position = position + Position(-1, 0))
  def moveRight: ActivePiece = copy(position = position + Position(1, 0))
  def moveDown: ActivePiece  = copy(position = position + Position(0, 1))

  /**
   * Board上で移動可能かをチェックして移動
   */
  def tryMoveLeft(board: Board): Option[ActivePiece] =
    val moved = moveLeft
    if board.canPlace(moved.blocks) then Some(moved) else None

  def tryMoveRight(board: Board): Option[ActivePiece] =
    val moved = moveRight
    if board.canPlace(moved.blocks) then Some(moved) else None

  def tryMoveDown(board: Board): Option[ActivePiece] =
    val moved = moveDown
    if board.canPlace(moved.blocks) then Some(moved) else None

  /**
   * 着地判定（現在位置は有効だが、1つ下は無効）
   */
  def hasLanded(board: Board): Boolean =
    board.canPlace(blocks) && !board.canPlace(moveDown.blocks)

  /**
   * SRSウォールキック付き回転
   * (旧 Collision.tryRotateWithWallKick)
   */
  def rotateOn(board: Board, clockwise: Boolean): Option[ActivePiece] =
    val rotated =
      if clockwise then copy(rotation = rotation.rotateClockwise)
      else copy(rotation = rotation.rotateCounterClockwise)

    val offsets = wallKickOffsets
    offsets.view
      .map(offset => rotated.copy(position = rotated.position + offset))
      .find(piece => board.canPlace(piece.blocks))

  /**
   * ウォールキックオフセット（簡易版SRS）
   */
  private def wallKickOffsets: List[Position] =
    shape match
      case TetrominoShape.I =>
        List(
          Position(0, 0),
          Position(-2, 0),
          Position(2, 0),
          Position(-2, 1),
          Position(2, -1)
        )
      case TetrominoShape.O =>
        List(Position(0, 0)) // O型は回転しても形が同じ
      case _ =>
        List(
          Position(0, 0),
          Position(-1, 0),
          Position(1, 0),
          Position(0, -1),
          Position(-1, -1),
          Position(1, -1)
        )

  /**
   * ハードドロップ後の位置
   */
  def hardDropOn(board: Board): ActivePiece =
    val distance = board.dropDistance(blocks)
    copy(position = position + Position(0, distance))

object ActivePiece:
  /**
   * 初期位置でピースを生成
   */
  def spawn(shape: TetrominoShape, boardWidth: Int): ActivePiece =
    ActivePiece(
      shape = shape,
      position = Position(boardWidth / 2, 1),
      rotation = Rotation.R0
    )

package monadris.logic

import monadris.domain.*

/**
 * 当たり判定を行う純粋関数群
 * すべての関数は副作用を持たない
 */
object Collision:

  /**
   * 衝突の種類
   */
  enum CollisionType:
    case None    // 衝突なし
    case Wall    // 壁との衝突
    case Floor   // 床との衝突
    case Block   // 固定済みブロックとの衝突
    case Ceiling // 天井（ゲームオーバー判定用）

  /**
   * テトリミノが有効な位置にあるかをチェック
   * @return 衝突していなければ true
   */
  def isValidPosition(tetromino: Tetromino, grid: Grid): Boolean =
    tetromino.currentBlocks.forall { pos =>
      isWithinHorizontalBounds(pos, grid) &&
      isWithinVerticalBounds(pos, grid) &&
      grid.isEmpty(pos)
    }

  /**
   * 水平方向の境界チェック（壁）
   */
  def isWithinHorizontalBounds(pos: Position, grid: Grid): Boolean =
    pos.x >= 0 && pos.x < grid.width

  /**
   * 垂直方向の境界チェック（天井・床）
   */
  def isWithinVerticalBounds(pos: Position, grid: Grid): Boolean =
    pos.y >= 0 && pos.y < grid.height

  /**
   * テトリミノの衝突タイプを検出
   */
  def detectCollision(tetromino: Tetromino, grid: Grid): CollisionType =
    val blocks = tetromino.currentBlocks

    // 壁との衝突
    if blocks.exists(p => p.x < 0 || p.x >= grid.width) then CollisionType.Wall
    // 床との衝突
    else if blocks.exists(p => p.y >= grid.height) then CollisionType.Floor
    // 天井より上（通常は問題なし、ゲームオーバー判定で使用）
    else if blocks.exists(p => p.y < 0) then CollisionType.Ceiling
    // 固定済みブロックとの衝突
    else if blocks.exists(p => !grid.isEmpty(p)) then CollisionType.Block
    else CollisionType.None

  /**
   * テトリミノが着地可能かどうか
   * 現在位置は有効だが、1つ下に移動すると衝突する場合に true
   */
  def hasLanded(tetromino: Tetromino, grid: Grid): Boolean =
    isValidPosition(tetromino, grid) &&
      !isValidPosition(tetromino.moveDown, grid)

  /**
   * ハードドロップ時の最終位置を計算
   * テトリミノを可能な限り下に移動
   */
  def hardDropPosition(tetromino: Tetromino, grid: Grid): Tetromino =
    @annotation.tailrec
    def drop(t: Tetromino): Tetromino =
      val next = t.moveDown
      if isValidPosition(next, grid) then drop(next)
      else t
    drop(tetromino)

  /**
   * スーパーローテーションシステム（SRS）のウォールキック
   * 回転が直接できない場合、位置をずらして回転を試みる
   */
  def tryRotateWithWallKick(
    tetromino: Tetromino,
    grid: Grid,
    clockwise: Boolean
  ): Option[Tetromino] =
    val rotated =
      if clockwise then tetromino.rotateClockwise
      else tetromino.rotateCounterClockwise

    // ウォールキックのオフセット（簡易版SRS）
    val offsets = tetromino.shape match
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

    offsets.view
      .map(offset => rotated.copy(position = rotated.position + offset))
      .find(isValidPosition(_, grid))

  /**
   * ゲームオーバー判定
   * 新しいテトリミノがスポーン位置で衝突する場合
   */
  def isGameOver(tetromino: Tetromino, grid: Grid): Boolean =
    !isValidPosition(tetromino, grid)

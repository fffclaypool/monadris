package monadris.domain.model.game

import monadris.domain.config.LevelConfig
import monadris.domain.config.ScoreConfig
import monadris.domain.model.board.Board
import monadris.domain.model.piece.ActivePiece
import monadris.domain.model.piece.TetrominoShape
import monadris.domain.model.scoring.ScoreState
import monadris.domain.service.PieceQueue

/**
 * テトリスゲームの集約ルート
 * 全てのゲーム状態とコマンド処理を統合
 */
final case class TetrisGame(
  board: Board,
  activePiece: ActivePiece,
  pieceQueue: PieceQueue,
  scoreState: ScoreState,
  phase: GamePhase,
  scoreConfig: ScoreConfig,
  levelConfig: LevelConfig
):
  /**
   * 次のピース形状（プレビュー用）
   */
  def nextShape: TetrominoShape = pieceQueue.peek

  /**
   * プレイ中かどうか
   */
  def isPlaying: Boolean = phase == GamePhase.Playing

  /**
   * ゲームオーバーかどうか
   */
  def isOver: Boolean = phase == GamePhase.Over

  /**
   * コマンドを処理し、新しい状態とイベントリストを返す
   */
  def handle(cmd: GameCommand): (TetrisGame, List[DomainEvent]) =
    phase match
      case GamePhase.Over =>
        (this, Nil)

      case GamePhase.Paused =>
        cmd match
          case GameCommand.TogglePause =>
            (copy(phase = GamePhase.Playing), List(DomainEvent.GameResumed))
          case _ =>
            (this, Nil)

      case GamePhase.Playing =>
        cmd match
          case GameCommand.MoveLeft    => handleMove(activePiece.tryMoveLeft(board))
          case GameCommand.MoveRight   => handleMove(activePiece.tryMoveRight(board))
          case GameCommand.SoftDrop    => handleSoftDrop()
          case GameCommand.HardDrop    => handleHardDrop()
          case GameCommand.RotateCW    => handleRotation(clockwise = true)
          case GameCommand.RotateCCW   => handleRotation(clockwise = false)
          case GameCommand.TogglePause => (copy(phase = GamePhase.Paused), List(DomainEvent.GamePaused))
          case GameCommand.Tick        => handleTick()

  /**
   * 移動処理
   */
  private def handleMove(result: Option[ActivePiece]): (TetrisGame, List[DomainEvent]) =
    result match
      case Some(moved) =>
        (copy(activePiece = moved), List(DomainEvent.PieceMoved(moved)))
      case None =>
        (this, Nil)

  /**
   * 回転処理（ウォールキック付き）
   */
  private def handleRotation(clockwise: Boolean): (TetrisGame, List[DomainEvent]) =
    activePiece.rotateOn(board, clockwise) match
      case Some(rotated) =>
        (copy(activePiece = rotated), List(DomainEvent.PieceRotated(rotated)))
      case None =>
        (this, Nil)

  /**
   * ソフトドロップ処理
   */
  private def handleSoftDrop(): (TetrisGame, List[DomainEvent]) =
    activePiece.tryMoveDown(board) match
      case Some(moved) =>
        (copy(activePiece = moved), List(DomainEvent.PieceMoved(moved)))
      case None =>
        lockPiece()

  /**
   * Tick処理（自動落下）
   */
  private def handleTick(): (TetrisGame, List[DomainEvent]) =
    handleSoftDrop()

  /**
   * ハードドロップ処理
   */
  private def handleHardDrop(): (TetrisGame, List[DomainEvent]) =
    val dropped       = activePiece.hardDropOn(board)
    val distance      = dropped.position.y - activePiece.position.y
    val newScoreState = scoreState.addHardDropBonus(distance)
    val gameWithDrop = copy(
      activePiece = dropped,
      scoreState = newScoreState
    )
    val (lockedGame, lockEvents) = gameWithDrop.lockPiece()
    (lockedGame, DomainEvent.PieceMoved(dropped) :: lockEvents)

  /**
   * ピースを盤面に固定し、次のピースを生成
   */
  private def lockPiece(): (TetrisGame, List[DomainEvent]) =
    // ピースを盤面に固定
    val newBoard  = board.place(activePiece.blocks, activePiece.shape)
    val lockEvent = DomainEvent.PieceLocked(activePiece)

    // ライン消去
    val (clearedBoard, linesCount) = newBoard.clearCompletedRows()

    // スコア更新
    val oldLevel      = scoreState.level
    val newScoreState = scoreState.addLines(linesCount, scoreConfig, levelConfig)

    // ライン消去イベント
    val clearEvents =
      if linesCount > 0 then
        val scoreGained = newScoreState.score - scoreState.score
        List(DomainEvent.LinesCleared(linesCount, scoreGained))
      else Nil

    // レベルアップイベント
    val levelUpEvents =
      if newScoreState.level > oldLevel then List(DomainEvent.LevelUp(newScoreState.level))
      else Nil

    // 次のピースを生成
    val (nextShape, newQueue) = pieceQueue.next
    val newPiece              = ActivePiece.spawn(nextShape, board.width)

    // ゲームオーバー判定
    if clearedBoard.isBlocked(newPiece.blocks) then
      val gameOver = copy(
        board = clearedBoard,
        scoreState = newScoreState,
        phase = GamePhase.Over
      )
      val allEvents = lockEvent :: clearEvents ::: levelUpEvents ::: List(DomainEvent.GameOver(newScoreState.score))
      (gameOver, allEvents)
    else
      val spawnEvent = DomainEvent.PieceSpawned(newPiece, newQueue.peek)
      val newGame = copy(
        board = clearedBoard,
        activePiece = newPiece,
        pieceQueue = newQueue,
        scoreState = newScoreState
      )
      val allEvents = lockEvent :: clearEvents ::: levelUpEvents ::: List(spawnEvent)
      (newGame, allEvents)

object TetrisGame:
  /**
   * 新しいゲームを作成
   */
  def create(
    seed: Long,
    boardWidth: Int,
    boardHeight: Int,
    scoreConfig: ScoreConfig,
    levelConfig: LevelConfig
  ): TetrisGame =
    val queue                = PieceQueue.fromSeed(seed)
    val (firstShape, queue2) = queue.next
    TetrisGame(
      board = Board.empty(boardWidth, boardHeight),
      activePiece = ActivePiece.spawn(firstShape, boardWidth),
      pieceQueue = queue2,
      scoreState = ScoreState.initial,
      phase = GamePhase.Playing,
      scoreConfig = scoreConfig,
      levelConfig = levelConfig
    )

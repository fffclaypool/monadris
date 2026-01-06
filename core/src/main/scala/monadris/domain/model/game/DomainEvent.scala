package monadris.domain.model.game

import monadris.domain.model.piece.ActivePiece
import monadris.domain.model.piece.TetrominoShape

/**
 * ドメインイベント
 * ゲーム状態遷移の結果として発生するイベント
 */
enum DomainEvent:
  /** ピースが移動した */
  case PieceMoved(piece: ActivePiece)

  /** ピースが回転した */
  case PieceRotated(piece: ActivePiece)

  /** ピースが盤面に固定された */
  case PieceLocked(piece: ActivePiece)

  /** ラインが消去された */
  case LinesCleared(count: Int, scoreGained: Int)

  /** レベルアップ */
  case LevelUp(newLevel: Int)

  /** 新しいピースがスポーン */
  case PieceSpawned(piece: ActivePiece, nextShape: TetrominoShape)

  /** ゲームオーバー */
  case GameOver(finalScore: Int)

  /** ゲームが一時停止 */
  case GamePaused

  /** ゲームが再開 */
  case GameResumed

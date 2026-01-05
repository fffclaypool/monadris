package monadris.domain.service

import scala.util.Random

import monadris.domain.model.piece.TetrominoShape

/**
 * 7-bag アルゴリズムによるピースキュー
 * seedベースで決定論的に生成
 */
final case class PieceQueue private (
  bag: List[TetrominoShape],
  peeked: TetrominoShape,
  rng: Random
):
  /**
   * 次のピースを取得し、新しいキューを返す
   */
  def next: (TetrominoShape, PieceQueue) =
    bag match
      case nextPeeked :: remaining =>
        (peeked, copy(bag = remaining, peeked = nextPeeked))
      case Nil =>
        val newBag = shuffleNewBag()
        newBag match
          case nextPeeked :: remaining =>
            (peeked, copy(bag = remaining, peeked = nextPeeked))
          case Nil =>
            // 理論上ここには到達しない（7種類のピースがある）
            (peeked, this)

  /**
   * プレビュー用の次のピース
   */
  def peek: TetrominoShape = peeked

  private def shuffleNewBag(): List[TetrominoShape] =
    rng.shuffle(TetrominoShape.values.toList)

object PieceQueue:
  /**
   * seedから決定論的にキューを生成
   */
  def fromSeed(seed: Long): PieceQueue =
    val rng = Random(seed)
    val bag = rng.shuffle(TetrominoShape.values.toList)
    bag match
      case first :: rest => PieceQueue(rest, first, rng)
      case Nil           => PieceQueue(Nil, TetrominoShape.I, rng) // 理論上到達しない

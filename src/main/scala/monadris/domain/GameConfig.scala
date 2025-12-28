package monadris.domain

/**
 * ゲーム全体の定数設定
 * マジックナンバーを一元管理し、可読性と変更容易性を向上
 */
object GameConfig:

  /**
   * グリッド設定
   */
  object Grid:
    /** グリッドの幅（セル数） */
    final val DefaultWidth: Int = 10

    /** グリッドの高さ（セル数） */
    final val DefaultHeight: Int = 20

  /**
   * スコア設定（オリジナルテトリスの得点システムに準拠）
   */
  object Score:
    /** 1ライン消去時の基本スコア */
    final val SingleLine: Int = 100

    /** 2ライン消去時の基本スコア */
    final val DoubleLine: Int = 300

    /** 3ライン消去時の基本スコア */
    final val TripleLine: Int = 500

    /** 4ライン消去時の基本スコア（テトリス） */
    final val Tetris: Int = 800

  /**
   * レベル設定
   */
  object Level:
    /** レベルアップに必要なライン数 */
    final val LinesPerLevel: Int = 10

  /**
   * 速度設定（ミリ秒単位）
   */
  object Speed:
    /** 基本落下速度 */
    final val BaseDropIntervalMs: Long = 1000L

    /** 最小落下速度（これ以上速くならない） */
    final val MinDropIntervalMs: Long = 100L

    /** レベルごとの短縮時間 */
    final val DecreasePerLevelMs: Long = 50L

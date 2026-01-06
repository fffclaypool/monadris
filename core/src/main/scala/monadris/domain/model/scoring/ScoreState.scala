package monadris.domain.model.scoring

import monadris.domain.config.LevelConfig
import monadris.domain.config.ScoreConfig
import monadris.domain.config.SpeedConfig

/**
 * スコア状態を表す値オブジェクト
 * スコア計算・レベル計算ロジックを内包
 */
final case class ScoreState(
  score: Int,
  level: Int,
  linesCleared: Int
):
  /**
   * ライン消去によるスコア・レベル更新
   * (旧 LineClearing.calculateScore/calculateLevel)
   */
  def addLines(count: Int, scoreConfig: ScoreConfig, levelConfig: LevelConfig): ScoreState =
    if count == 0 then this
    else
      val gained          = calculateScoreGain(count, scoreConfig)
      val newLinesCleared = linesCleared + count
      val newLevel        = calculateLevel(newLinesCleared, levelConfig)
      ScoreState(
        score = score + gained,
        level = newLevel,
        linesCleared = newLinesCleared
      )

  /**
   * ハードドロップボーナス加算
   */
  def addHardDropBonus(distance: Int): ScoreState =
    copy(score = score + distance * 2)

  /**
   * スコア計算（オリジナルテトリスの得点システムに準拠）
   * 1ライン: 100 × レベル
   * 2ライン: 300 × レベル
   * 3ライン: 500 × レベル
   * 4ライン（テトリス）: 800 × レベル
   */
  private def calculateScoreGain(count: Int, config: ScoreConfig): Int =
    val baseScore = count match
      case 1 => config.singleLine
      case 2 => config.doubleLine
      case 3 => config.tripleLine
      case 4 => config.tetris
      case _ => 0
    baseScore * level

  /**
   * レベル計算（10ライン消去ごとにレベルアップ）
   */
  private def calculateLevel(totalLines: Int, config: LevelConfig): Int =
    1 + (totalLines / config.linesPerLevel)

object ScoreState:
  val initial: ScoreState = ScoreState(0, 1, 0)

  /**
   * 落下速度計算（ミリ秒単位）
   * レベルが上がるほど速くなる
   */
  def dropInterval(level: Int, config: SpeedConfig): Long =
    val decrease = (level - 1) * config.decreasePerLevelMs
    Math.max(config.minDropIntervalMs, config.baseDropIntervalMs - decrease)

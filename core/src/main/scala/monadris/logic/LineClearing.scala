package monadris.logic

import monadris.domain.*
import monadris.domain.config.LevelConfig
import monadris.domain.config.ScoreConfig
import monadris.domain.config.SpeedConfig

/**
 * ライン消去とスコア計算を行う純粋関数群
 */
object LineClearing:

  /**
   * ライン消去の結果
   */
  final case class ClearResult(
    grid: Grid,
    linesCleared: Int,
    scoreGained: Int
  )

  /**
   * 揃った行を消去し、スコアを計算
   */
  def clearLines(grid: Grid, level: Int, config: ScoreConfig): ClearResult =
    val completedRows = grid.completedRows
    val linesCleared  = completedRows.size

    if linesCleared == 0 then ClearResult(grid, 0, 0)
    else
      val newGrid     = grid.clearRows(completedRows)
      val scoreGained = calculateScore(linesCleared, level, config)
      ClearResult(newGrid, linesCleared, scoreGained)

  /**
   * スコア計算（オリジナルテトリスの得点システムに準拠）
   * 1ライン: 100 × レベル
   * 2ライン: 300 × レベル
   * 3ライン: 500 × レベル
   * 4ライン（テトリス）: 800 × レベル
   */
  def calculateScore(linesCleared: Int, level: Int, config: ScoreConfig): Int =
    val baseScore = linesCleared match
      case 1 => config.singleLine
      case 2 => config.doubleLine
      case 3 => config.tripleLine
      case 4 => config.tetris
      case _ => 0
    baseScore * level

  /**
   * レベル計算（10ライン消去ごとにレベルアップ）
   */
  def calculateLevel(totalLinesCleared: Int, config: LevelConfig, startLevel: Int = 1): Int =
    startLevel + (totalLinesCleared / config.linesPerLevel)

  /**
   * 落下速度計算（ミリ秒単位）
   * レベルが上がるほど速くなる
   */
  def dropInterval(level: Int, config: SpeedConfig): Long =
    val decrease = (level - 1) * config.decreasePerLevelMs
    Math.max(config.minDropIntervalMs, config.baseDropIntervalMs - decrease)

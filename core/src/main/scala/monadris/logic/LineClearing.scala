package monadris.logic

import monadris.domain.*
import monadris.domain.config.LevelConfig
import monadris.domain.config.ScoreConfig
import monadris.domain.config.SpeedConfig

object LineClearing:

  final case class ClearResult(
    grid: Grid,
    linesCleared: Int,
    scoreGained: Int
  )

  def clearLines(grid: Grid, level: Int, config: ScoreConfig): ClearResult =
    val completedRows = grid.completedRows
    val linesCleared  = completedRows.size

    if linesCleared == 0 then ClearResult(grid, 0, 0)
    else
      val newGrid     = grid.clearRows(completedRows)
      val scoreGained = calculateScore(linesCleared, level, config)
      ClearResult(newGrid, linesCleared, scoreGained)

  /** baseScore × level (baseScoreは1〜4ラインで100/300/500/800) */
  def calculateScore(linesCleared: Int, level: Int, config: ScoreConfig): Int =
    val baseScore = linesCleared match
      case 1 => config.singleLine
      case 2 => config.doubleLine
      case 3 => config.tripleLine
      case 4 => config.tetris
      case _ => 0
    baseScore * level

  def calculateLevel(totalLinesCleared: Int, config: LevelConfig, startLevel: Int = 1): Int =
    startLevel + (totalLinesCleared / config.linesPerLevel)

  def dropInterval(level: Int, config: SpeedConfig): Long =
    val decrease = (level - 1) * config.decreasePerLevelMs
    Math.max(config.minDropIntervalMs, config.baseDropIntervalMs - decrease)

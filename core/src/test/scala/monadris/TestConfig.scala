package monadris

import monadris.config.*

/**
 * Test configuration for the core project.
 * Pure config values with no ZIO dependency.
 */
object TestConfig:
  val testConfig: GameConfig = GameConfig(
    grid = GridConfig(width = 10, height = 20),
    score = ScoreConfig(singleLine = 100, doubleLine = 300, tripleLine = 500, tetris = 800, hardDropPerCell = 2),
    level = LevelConfig(linesPerLevel = 10),
    speed = SpeedConfig(baseDropIntervalMs = 1000, minDropIntervalMs = 100, decreasePerLevelMs = 50)
  )

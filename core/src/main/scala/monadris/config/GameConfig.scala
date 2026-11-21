package monadris.config

/** The rules of the game: everything the pure game logic needs to know */
final case class GameConfig(
  grid: GridConfig,
  score: ScoreConfig,
  level: LevelConfig,
  speed: SpeedConfig
)

final case class GridConfig(
  width: Int,
  height: Int
)

final case class ScoreConfig(
  singleLine: Int,
  doubleLine: Int,
  tripleLine: Int,
  tetris: Int,
  hardDropPerCell: Int
)

final case class LevelConfig(
  linesPerLevel: Int
)

final case class SpeedConfig(
  baseDropIntervalMs: Long,
  minDropIntervalMs: Long,
  decreasePerLevelMs: Long
)

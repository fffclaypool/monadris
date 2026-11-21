package monadris.config

final case class AppConfig(
  grid: GridConfig,
  score: ScoreConfig,
  level: LevelConfig,
  speed: SpeedConfig,
  terminal: TerminalConfig,
  timing: TimingConfig,
  replay: ReplayConfig,
  database: DatabaseConfig
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

final case class TerminalConfig(
  escapeSequenceWaitMs: Int,
  escapeSequenceSecondWaitMs: Int,
  inputPollIntervalMs: Int
)

final case class TimingConfig(
  titleDelayMs: Long,
  outroDelayMs: Long
)

/** Where recorded replays are persisted */
enum ReplayStorage:
  case File, Postgres

object ReplayStorage:
  def fromString(value: String): Option[ReplayStorage] =
    ReplayStorage.values.find(_.toString.equalsIgnoreCase(value.trim))

final case class ReplayConfig(
  defaultSpeed: Double,
  storage: ReplayStorage
)

final case class DatabaseConfig(
  url: String,
  username: String,
  password: String,
  poolSize: Int,
  connectionTimeoutMs: Long
)

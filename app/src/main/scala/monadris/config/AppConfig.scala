package monadris.config

/**
 * The complete application configuration as laid out in `application.conf`.
 * The game rules are handed to the pure core as a [[GameConfig]]; the remaining
 * sections concern the terminal, replay storage and the database.
 */
final case class AppConfig(
  grid: GridConfig,
  score: ScoreConfig,
  level: LevelConfig,
  speed: SpeedConfig,
  terminal: TerminalConfig,
  timing: TimingConfig,
  replay: ReplayConfig,
  database: DatabaseConfig
):
  def game: GameConfig = GameConfig(grid, score, level, speed)

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
):
  /** Keeps the password out of logs and error messages */
  override def toString: String =
    s"DatabaseConfig($url, $username, ${DatabaseConfig.RedactedPassword}, $poolSize, $connectionTimeoutMs)"

object DatabaseConfig:
  private val RedactedPassword = "***"

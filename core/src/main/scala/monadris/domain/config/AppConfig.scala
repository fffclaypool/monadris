package monadris.domain.config

final case class AppConfig(
  grid: GridConfig,
  score: ScoreConfig,
  level: LevelConfig,
  speed: SpeedConfig,
  terminal: TerminalConfig,
  timing: TimingConfig
)

final case class GridConfig(
  width: Int,
  height: Int
)

final case class ScoreConfig(
  singleLine: Int,
  doubleLine: Int,
  tripleLine: Int,
  tetris: Int
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

package monadris

import monadris.config.*

/**
 * Core用テスト設定
 * ZIOに依存しない純粋な設定値
 */
object TestConfig:
  val testConfig: AppConfig = AppConfig(
    grid = GridConfig(width = 10, height = 20),
    score = ScoreConfig(singleLine = 100, doubleLine = 300, tripleLine = 500, tetris = 800),
    level = LevelConfig(linesPerLevel = 10),
    speed = SpeedConfig(baseDropIntervalMs = 1000, minDropIntervalMs = 100, decreasePerLevelMs = 50),
    terminal = TerminalConfig(escapeSequenceWaitMs = 20, escapeSequenceSecondWaitMs = 5, inputPollIntervalMs = 20),
    timing = TimingConfig(titleDelayMs = 1000, outroDelayMs = 2000),
    replay = ReplayConfig(defaultSpeed = 1.0, baseFrameIntervalMs = 50)
  )

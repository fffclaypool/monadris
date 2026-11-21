package monadris.config

/**
 * Checks that configured values are usable before the game starts.
 * Without this, values such as `linesPerLevel = 0` would only surface as an
 * arithmetic error in the middle of a game.
 */
object ConfigValidation:

  /** The I piece is four cells long and a piece spawns with blocks above its centre */
  val MinGridWidth: Int  = 4
  val MinGridHeight: Int = 4

  def validate(config: AppConfig): Either[Vector[String], AppConfig] =
    val errors = rules(config).collect { case (isValid, message) if !isValid => message }
    if errors.isEmpty then Right(config) else Left(errors)

  private def rules(config: AppConfig): Vector[(Boolean, String)] =
    Vector(
      (config.grid.width >= MinGridWidth, s"grid.width must be at least $MinGridWidth"),
      (config.grid.height >= MinGridHeight, s"grid.height must be at least $MinGridHeight"),
      (config.score.singleLine >= 0, "score.singleLine must not be negative"),
      (config.score.doubleLine >= 0, "score.doubleLine must not be negative"),
      (config.score.tripleLine >= 0, "score.tripleLine must not be negative"),
      (config.score.tetris >= 0, "score.tetris must not be negative"),
      (config.score.hardDropPerCell >= 0, "score.hardDropPerCell must not be negative"),
      (config.level.linesPerLevel > 0, "level.linesPerLevel must be positive"),
      (config.speed.baseDropIntervalMs > 0, "speed.baseDropIntervalMs must be positive"),
      (config.speed.minDropIntervalMs > 0, "speed.minDropIntervalMs must be positive"),
      (
        config.speed.minDropIntervalMs <= config.speed.baseDropIntervalMs,
        "speed.minDropIntervalMs must not exceed speed.baseDropIntervalMs"
      ),
      (config.speed.decreasePerLevelMs >= 0, "speed.decreasePerLevelMs must not be negative"),
      (config.terminal.escapeSequenceWaitMs >= 0, "terminal.escapeSequenceWaitMs must not be negative"),
      (config.terminal.escapeSequenceSecondWaitMs >= 0, "terminal.escapeSequenceSecondWaitMs must not be negative"),
      (config.terminal.inputPollIntervalMs > 0, "terminal.inputPollIntervalMs must be positive"),
      (config.timing.titleDelayMs >= 0, "timing.titleDelayMs must not be negative"),
      (config.timing.outroDelayMs >= 0, "timing.outroDelayMs must not be negative"),
      (config.replay.defaultSpeed > 0, "replay.defaultSpeed must be positive"),
      (config.database.poolSize > 0, "database.poolSize must be positive"),
      (config.database.connectionTimeoutMs > 0, "database.connectionTimeoutMs must be positive")
    )

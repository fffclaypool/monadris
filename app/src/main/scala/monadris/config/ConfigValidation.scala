package monadris.config

/** Checks the whole application configuration: the game rules plus the infrastructure settings. */
object ConfigValidation:

  def validate(config: AppConfig): Either[Vector[String], AppConfig] =
    val errors = GameConfigValidation.violations(GameConfigValidation.rules(config.game) ++ infrastructureRules(config))
    if errors.isEmpty then Right(config) else Left(errors)

  private def infrastructureRules(config: AppConfig): Vector[GameConfigValidation.Rule] =
    Vector(
      (config.terminal.escapeSequenceWaitMs >= 0, "terminal.escapeSequenceWaitMs must not be negative"),
      (config.terminal.escapeSequenceSecondWaitMs >= 0, "terminal.escapeSequenceSecondWaitMs must not be negative"),
      (config.terminal.inputPollIntervalMs > 0, "terminal.inputPollIntervalMs must be positive"),
      (config.timing.titleDelayMs >= 0, "timing.titleDelayMs must not be negative"),
      (config.timing.outroDelayMs >= 0, "timing.outroDelayMs must not be negative"),
      (config.replay.defaultSpeed > 0, "replay.defaultSpeed must be positive"),
      (config.database.poolSize > 0, "database.poolSize must be positive"),
      (config.database.connectionTimeoutMs > 0, "database.connectionTimeoutMs must be positive")
    )

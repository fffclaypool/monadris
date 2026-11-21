package monadris.config

/**
 * Checks that the game rules are usable before a game starts.
 * Without this, values such as `linesPerLevel = 0` would only surface as an
 * arithmetic error in the middle of a game.
 */
object GameConfigValidation:

  /** The I piece is four cells long and a piece spawns with blocks above its centre */
  val MinGridWidth: Int  = 4
  val MinGridHeight: Int = 4

  /** A rule is a condition that must hold, with the message reported when it does not */
  type Rule = (Boolean, String)

  def violations(rules: Vector[Rule]): Vector[String] =
    rules.collect { case (isValid, message) if !isValid => message }

  def validate(config: GameConfig): Either[Vector[String], GameConfig] =
    val errors = violations(rules(config))
    if errors.isEmpty then Right(config) else Left(errors)

  def rules(config: GameConfig): Vector[Rule] =
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
      (config.speed.decreasePerLevelMs >= 0, "speed.decreasePerLevelMs must not be negative")
    )

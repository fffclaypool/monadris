package monadris.replay

/**
 * Validation of user-supplied replay names.
 *
 * Names are used as file names and database keys, so only letters, digits,
 * '-' and '_' are accepted (no path separators, dots or whitespace).
 */
object ReplayName:

  val MaxLength: Int = 64

  private val AllowedSymbols: Set[Char] = Set('-', '_')

  def validate(raw: String): Either[String, String] =
    val name = raw.trim
    if name.isEmpty then Left("Replay name must not be empty.")
    else if name.length > MaxLength then Left(s"Replay name must be at most $MaxLength characters.")
    else if !name.forall(isAllowed) then Left("Replay name may only contain letters, digits, '-' and '_'.")
    else Right(name)

  def isValid(raw: String): Boolean = validate(raw).isRight

  private def isAllowed(c: Char): Boolean =
    Character.isLetterOrDigit(c) || AllowedSymbols.contains(c)

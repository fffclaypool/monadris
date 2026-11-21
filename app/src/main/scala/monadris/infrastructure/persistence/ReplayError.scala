package monadris.infrastructure.persistence

/** Everything that can go wrong when storing or loading replays */
enum ReplayError:
  case NotFound(name: String)
  case AlreadyExists(name: String)
  case InvalidName(reason: String)

  /** The stored data exists but cannot be decoded */
  case Corrupted(detail: String)

  /** The storage backend itself failed (I/O error, database unreachable, ...) */
  case StorageFailure(cause: Throwable)

  def message: String = this match
    case NotFound(name)        => s"Replay not found: $name"
    case AlreadyExists(name)   => s"A replay named '$name' already exists."
    case InvalidName(reason)   => reason
    case Corrupted(detail)     => s"Replay data is corrupted: $detail"
    case StorageFailure(cause) => Option(cause.getMessage).getOrElse(cause.getClass.getSimpleName)

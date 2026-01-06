package monadris.domain.model.game

/**
 * ゲームへのコマンド（入力）
 */
enum GameCommand:
  case MoveLeft
  case MoveRight
  case SoftDrop
  case HardDrop
  case RotateCW
  case RotateCCW
  case TogglePause
  case Tick

package monadris.domain

/**
 * プレイヤーの入力
 */
enum Input:
  case MoveLeft
  case MoveRight
  case MoveDown
  case RotateClockwise
  case RotateCounterClockwise
  case HardDrop
  case Pause
  case Quit
  case Tick // 時間経過による自動落下

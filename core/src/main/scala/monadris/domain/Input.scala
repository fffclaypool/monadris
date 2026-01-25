package monadris.domain

enum Input:
  case MoveLeft
  case MoveRight
  case MoveDown
  case RotateClockwise
  case RotateCounterClockwise
  case HardDrop
  case Pause
  case Quit
  case Tick

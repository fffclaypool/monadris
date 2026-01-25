package monadris.infrastructure.runtime

import monadris.domain.Input

enum GameCommand:
  case UserAction(input: Input)
  case TimeTick
  case Quit

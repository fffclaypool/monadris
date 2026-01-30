package monadris.input

import monadris.domain.Input

enum GameCommand:
  case UserAction(input: Input)
  case TimeTick
  case Quit

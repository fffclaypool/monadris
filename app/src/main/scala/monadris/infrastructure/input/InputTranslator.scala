package monadris.infrastructure.input

import monadris.domain.Input
import monadris.domain.model.game.GameCommand

/**
 * Input から GameCommand への変換（Anti-Corruption Layer）
 */
object InputTranslator:
  def translate(input: Input): Option[GameCommand] = input match
    case Input.MoveLeft               => Some(GameCommand.MoveLeft)
    case Input.MoveRight              => Some(GameCommand.MoveRight)
    case Input.MoveDown               => Some(GameCommand.SoftDrop)
    case Input.RotateClockwise        => Some(GameCommand.RotateCW)
    case Input.RotateCounterClockwise => Some(GameCommand.RotateCCW)
    case Input.HardDrop               => Some(GameCommand.HardDrop)
    case Input.Pause                  => Some(GameCommand.TogglePause)
    case Input.Tick                   => Some(GameCommand.Tick)
    case Input.Quit                   => None // Quit は特別処理

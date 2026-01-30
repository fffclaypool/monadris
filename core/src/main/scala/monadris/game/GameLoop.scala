package monadris.game

import monadris.config.AppConfig
import monadris.domain.*

object GameLoop:

  final case class Outcome(
    state: GameState,
    nextIntervalMs: Option[Long]
  ):
    def shouldContinue: Boolean = !state.isGameOver

  def handleInput(
    state: GameState,
    input: Input,
    nextShape: TetrominoShape,
    config: AppConfig
  ): Outcome =
    val newState     = GameLogic.update(state, input, () => nextShape, config)
    val interval     = LineClearing.dropInterval(newState.level, config.speed)
    val nextInterval = if newState.isGameOver then None else Some(interval)
    Outcome(newState, nextInterval)

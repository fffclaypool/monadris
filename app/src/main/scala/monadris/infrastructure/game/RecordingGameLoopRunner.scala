package monadris.infrastructure.game

import java.util.concurrent.TimeUnit

import zio.*

import monadris.config.AppConfig
import monadris.domain.*
import monadris.infrastructure.terminal.ConsoleService
import monadris.infrastructure.terminal.Renderer
import monadris.infrastructure.terminal.TtyService
import monadris.replay.*

/** Runs the regular game loop while recording every processed command into a replay. */
object RecordingGameLoopRunner:

  def recordingGameLoop(
    initialState: GameState,
    config: AppConfig,
    renderer: Renderer,
    randomPiece: GameRunner.RandomPiece
  ): ZIO[TtyService & ConsoleService, Throwable, (GameState, ReplayData)] =
    for
      startTime <- currentTimeMs
      initialBuilder = ReplayBuilder.create(
        timestamp = startTime,
        gridWidth = config.grid.width,
        gridHeight = config.grid.height,
        initialPiece = initialState.currentTetromino.shape,
        nextPiece = initialState.nextTetromino
      )
      result <- GameLoopRunner.observedGameLoop(initialState, config, renderer, randomPiece, initialBuilder) {
        (builder, step) => currentTimeMs.map(now => recordStep(builder, step, now - startTime))
      }
      (finalState, finalBuilder) = result
      endTime <- currentTimeMs
    yield (finalState, finalBuilder.build(finalState, endTime))

  private val currentTimeMs: UIO[Long] =
    Clock.currentTime(TimeUnit.MILLISECONDS)

  /** Each command is one frame, stamped with the time since the recording started. */
  private[game] def recordStep(builder: ReplayBuilder, step: GameLoopRunner.Step, elapsedMs: Long): ReplayBuilder =
    val timedBuilder     = builder.atElapsed(elapsedMs)
    val builderWithSpawn =
      if pieceWasLocked(step.before, step.after) then timedBuilder.recordPieceSpawn(step.nextShape)
      else timedBuilder
    builderWithSpawn.recordInput(step.input).advanceFrame

  private def pieceWasLocked(oldState: GameState, newState: GameState): Boolean =
    oldState.currentTetromino.shape != newState.currentTetromino.shape ||
      oldState.nextTetromino != newState.nextTetromino

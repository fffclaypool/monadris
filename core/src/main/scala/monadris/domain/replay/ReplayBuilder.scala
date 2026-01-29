package monadris.domain.replay

import monadris.domain.GameState
import monadris.domain.Input
import monadris.domain.TetrominoShape

final case class ReplayBuilder private (
  startTimestamp: Long,
  gridWidth: Int,
  gridHeight: Int,
  initialPiece: TetrominoShape,
  nextPiece: TetrominoShape,
  events: Vector[ReplayEvent],
  currentFrame: Long
):
  def recordInput(input: Input): ReplayBuilder =
    val event = ReplayEvent.PlayerInput(input, currentFrame)
    copy(events = events :+ event)

  def recordPieceSpawn(shape: TetrominoShape): ReplayBuilder =
    val event = ReplayEvent.PieceSpawn(shape, currentFrame)
    copy(events = events :+ event)

  def advanceFrame: ReplayBuilder =
    copy(currentFrame = currentFrame + 1)

  def build(finalState: GameState, endTimestamp: Long): ReplayData =
    val metadata = ReplayMetadata(
      version = ReplayMetadata.CurrentVersion,
      timestamp = startTimestamp,
      gridWidth = gridWidth,
      gridHeight = gridHeight,
      initialPiece = initialPiece,
      nextPiece = nextPiece,
      finalScore = finalState.score,
      finalLevel = finalState.level,
      finalLinesCleared = finalState.linesCleared,
      durationMs = endTimestamp - startTimestamp
    )
    ReplayData(metadata, events)

object ReplayBuilder:
  def create(
    timestamp: Long,
    gridWidth: Int,
    gridHeight: Int,
    initialPiece: TetrominoShape,
    nextPiece: TetrominoShape
  ): ReplayBuilder =
    ReplayBuilder(
      startTimestamp = timestamp,
      gridWidth = gridWidth,
      gridHeight = gridHeight,
      initialPiece = initialPiece,
      nextPiece = nextPiece,
      events = Vector.empty,
      currentFrame = 0
    )

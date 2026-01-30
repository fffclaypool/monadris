package monadris.replay

import monadris.domain.Input
import monadris.domain.TetrominoShape

enum ReplayEvent:
  case PlayerInput(input: Input, frameNumber: Long)
  case PieceSpawn(shape: TetrominoShape, frameNumber: Long)

final case class ReplayMetadata(
  version: String,
  timestamp: Long,
  gridWidth: Int,
  gridHeight: Int,
  initialPiece: TetrominoShape,
  nextPiece: TetrominoShape,
  finalScore: Int,
  finalLevel: Int,
  finalLinesCleared: Int,
  durationMs: Long
)

object ReplayMetadata:
  val CurrentVersion: String = "1.0"

final case class ReplayData(
  metadata: ReplayMetadata,
  events: Vector[ReplayEvent]
):
  def eventCount: Int = events.size

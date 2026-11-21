package monadris.replay

import monadris.domain.Input
import monadris.domain.TetrominoShape

enum ReplayEvent:
  /** Index of the processed command this event belongs to (one frame per command) */
  def frameNumber: Long

  /** Milliseconds since the start of the recording; drives playback timing */
  def elapsedMs: Long

  case PlayerInput(input: Input, frameNumber: Long, elapsedMs: Long = ReplayEvent.StartOfRecordingMs)
  case PieceSpawn(shape: TetrominoShape, frameNumber: Long, elapsedMs: Long = ReplayEvent.StartOfRecordingMs)

object ReplayEvent:
  val StartOfRecordingMs: Long = 0L

  /** Replays recorded before version 1.1 carry no timing and were played at a fixed rate */
  val LegacyFrameIntervalMs: Long = 50L

  def legacyElapsedMs(frameNumber: Long): Long =
    frameNumber * LegacyFrameIntervalMs

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
  /** 1.1: events carry `elapsedMs` */
  val CurrentVersion: String = "1.1"

final case class ReplayData(
  metadata: ReplayMetadata,
  events: Vector[ReplayEvent]
):
  def eventCount: Int = events.size

package monadris.replay

import monadris.config.AppConfig
import monadris.domain.*
import monadris.game.GameLogic

final case class ReplayPlayerState(
  gameState: GameState,
  replayData: ReplayData,
  currentFrame: Long,
  eventIndex: Int,
  isFinished: Boolean
):
  def progress: Double =
    if replayData.events.isEmpty then 1.0
    else eventIndex.toDouble / replayData.events.size.toDouble

  def nextEvent: Option[ReplayEvent] =
    replayData.events.lift(eventIndex)

  /** Recording time of the most recently played event */
  def playedElapsedMs: Long =
    replayData.events.lift(eventIndex - 1).fold(ReplayEvent.StartOfRecordingMs)(_.elapsedMs)

  /**
   * How long the recording waited before the events of the current frame.
   * Zero when the current frame has no events (nothing to wait for).
   */
  def delayUntilNextFrameMs: Long =
    nextEvent
      .filter(_.frameNumber <= currentFrame)
      .fold(0L)(event => Math.max(0L, event.elapsedMs - playedElapsedMs))

object ReplayPlayer:

  def initialize(replayData: ReplayData): ReplayPlayerState =
    val metadata     = replayData.metadata
    val initialState = GameState.initial(
      metadata.initialPiece,
      metadata.nextPiece,
      metadata.gridWidth,
      metadata.gridHeight
    )
    ReplayPlayerState(
      gameState = initialState,
      replayData = replayData,
      currentFrame = 0,
      eventIndex = 0,
      isFinished = false
    )

  def advanceFrame(state: ReplayPlayerState, config: AppConfig): ReplayPlayerState =
    if state.isFinished then state
    else processEventsAtFrame(state, config)

  final private case class FrameProcessingState(
    gameState: GameState,
    eventIndex: Int,
    nextShapeQueue: Vector[TetrominoShape]
  )

  private def processEventsAtFrame(
    state: ReplayPlayerState,
    config: AppConfig
  ): ReplayPlayerState =
    val events                 = state.replayData.events
    val currentFrame           = state.currentFrame
    val eventsForFrame         = collectEventsForFrame(events, state.eventIndex, currentFrame)
    val initialProcessingState = FrameProcessingState(
      gameState = state.gameState,
      eventIndex = state.eventIndex,
      nextShapeQueue = Vector.empty
    )

    val finalProcessingState = eventsForFrame.foldLeft(initialProcessingState) { (acc, event) =>
      processEvent(event, acc, config)
    }

    val newEventIndex = state.eventIndex + eventsForFrame.size
    val isFinished    = newEventIndex >= events.size || finalProcessingState.gameState.isGameOver

    ReplayPlayerState(
      gameState = finalProcessingState.gameState,
      replayData = state.replayData,
      currentFrame = currentFrame + 1,
      eventIndex = newEventIndex,
      isFinished = isFinished
    )

  private def collectEventsForFrame(
    events: Vector[ReplayEvent],
    startIndex: Int,
    frame: Long
  ): Vector[ReplayEvent] =
    // "<=" rather than "==": an event stamped with an already passed frame (malformed data)
    // is played immediately instead of stalling the replay forever.
    events.slice(startIndex, events.size).takeWhile(_.frameNumber <= frame)

  private def processEvent(
    event: ReplayEvent,
    state: FrameProcessingState,
    config: AppConfig
  ): FrameProcessingState =
    event match
      case ReplayEvent.PlayerInput(input, _, _) =>
        val nextShape    = state.nextShapeQueue.headOption.getOrElse(state.gameState.nextTetromino)
        val newQueue     = state.nextShapeQueue.drop(1)
        val newGameState = GameLogic.update(state.gameState, input, nextShape, config)
        state.copy(gameState = newGameState, nextShapeQueue = newQueue)
      case ReplayEvent.PieceSpawn(shape, _, _) =>
        state.copy(nextShapeQueue = state.nextShapeQueue :+ shape)

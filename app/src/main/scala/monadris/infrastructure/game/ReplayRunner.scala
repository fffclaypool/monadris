package monadris.infrastructure.game

import zio.*

import monadris.config.AppConfig
import monadris.infrastructure.terminal.ConsoleService
import monadris.infrastructure.terminal.Renderer
import monadris.infrastructure.terminal.TtyService
import monadris.replay.ReplayData
import monadris.replay.ReplayPlayer
import monadris.replay.ReplayPlayerState
import monadris.view.AnsiRenderer
import monadris.view.ScreenBuffer

object ReplayRunner:

  private object Speed:
    val Min: Double  = 0.25
    val Max: Double  = 4.0
    val Step: Double = 0.25

    def clamp(speed: Double): Double = speed.max(Min).min(Max)

  private object Timing:
    /** Upper bound for a single wait, so a corrupt timestamp cannot freeze playback */
    val MaxFrameDelayMs: Long = 5000L

    /** How often the keyboard is checked while playback is paused */
    val PausePollMs: Int = 100

    val NanosPerMilli: Long = 1000000L

  private object Keys:
    val SpeedUp: Int    = '+'.toInt
    val SpeedDown: Int  = '-'.toInt
    val Pause: Int      = 'p'.toInt
    val PauseUpper: Int = 'P'.toInt
    val Space: Int      = ' '.toInt
    val Quit: Int       = 'q'.toInt
    val QuitUpper: Int  = 'Q'.toInt

  private object Status:
    val FirstColumn    = 1
    val PercentScale   = 100
    val PausedLabel    = "[PAUSED]"
    val PlayingLabel   = "[REPLAY]"
    val FinishedNotice = "Replay finished. Press any key..."
    val KeyHelp        = "Q: Quit, +/-: Speed, P: Pause"

  /**
   * @param remainingDelayMs what is left of the current wait after it was interrupted by a
   *                         key press; `None` means the next frame's full delay applies
   */
  final case class PlaybackState(
    replayState: ReplayPlayerState,
    previousBuffer: Option[ScreenBuffer],
    speed: Double,
    paused: Boolean,
    remainingDelayMs: Option[Long] = None
  )

  def run(
    replayData: ReplayData,
    config: AppConfig,
    renderer: Renderer
  ): ZIO[TtyService & ConsoleService, Throwable, Unit] =
    val initialState = PlaybackState(
      replayState = ReplayPlayer.initialize(replayData),
      previousBuffer = None,
      speed = Speed.clamp(config.replay.defaultSpeed),
      paused = false
    )
    for
      rendered   <- render(initialState, config, renderer)
      finalState <- playbackLoop(rendered, config, renderer)
      _          <- printStatusLine(finalState, Status.FinishedNotice)
    yield ()

  private def playbackLoop(
    state: PlaybackState,
    config: AppConfig,
    renderer: Renderer
  ): ZIO[TtyService & ConsoleService, Throwable, PlaybackState] =
    if state.replayState.isFinished then ZIO.succeed(state)
    else if state.paused then
      TtyService.readByteWithTimeout(Timing.PausePollMs).flatMap {
        case Some(key) => handleKeyPress(key, state, config, renderer)
        case None      => playbackLoop(state, config, renderer)
      }
    else
      val delayMs = state.remainingDelayMs.getOrElse(frameDelayMs(state).toLong)
      for
        startedAt <- Clock.nanoTime
        inputOpt  <- TtyService.readByteWithTimeout(delayMs.toInt)
        result    <- inputOpt match
          case None      => advancePlayback(state, config, renderer)
          case Some(key) =>
            // Keep what is left of this wait, so key presses do not stretch the replay.
            Clock.nanoTime.flatMap { now =>
              val waitedMs  = (now - startedAt) / Timing.NanosPerMilli
              val remaining = Math.max(0L, delayMs - waitedMs)
              handleKeyPress(key, state.copy(remainingDelayMs = Some(remaining)), config, renderer)
            }
      yield result

  private def handleKeyPress(
    key: Int,
    state: PlaybackState,
    config: AppConfig,
    renderer: Renderer
  ): ZIO[TtyService & ConsoleService, Throwable, PlaybackState] =
    key match
      case Keys.Quit | Keys.QuitUpper =>
        ZIO.succeed(state.copy(replayState = state.replayState.copy(isFinished = true)))
      case Keys.SpeedUp =>
        redrawAndContinue(withSpeed(state, state.speed + Speed.Step), config, renderer)
      case Keys.SpeedDown =>
        redrawAndContinue(withSpeed(state, state.speed - Speed.Step), config, renderer)
      case Keys.Pause | Keys.PauseUpper | Keys.Space =>
        redrawAndContinue(state.copy(paused = !state.paused), config, renderer)
      case _ =>
        playbackLoop(state, config, renderer)

  /** Changes the speed and rescales the interrupted wait to the new speed. */
  private[game] def withSpeed(state: PlaybackState, requestedSpeed: Double): PlaybackState =
    val newSpeed = Speed.clamp(requestedSpeed)
    state.copy(
      speed = newSpeed,
      remainingDelayMs = state.remainingDelayMs.map(remaining => (remaining * state.speed / newSpeed).toLong)
    )

  private def redrawAndContinue(
    state: PlaybackState,
    config: AppConfig,
    renderer: Renderer
  ): ZIO[TtyService & ConsoleService, Throwable, PlaybackState] =
    render(state, config, renderer).flatMap(playbackLoop(_, config, renderer))

  private def advancePlayback(
    state: PlaybackState,
    config: AppConfig,
    renderer: Renderer
  ): ZIO[TtyService & ConsoleService, Throwable, PlaybackState] =
    val advanced = state.copy(
      replayState = ReplayPlayer.advanceFrame(state.replayState, config.game),
      remainingDelayMs = None
    )
    redrawAndContinue(advanced, config, renderer)

  /** Reproduces the pause the player made before the next frame, scaled by the playback speed. */
  private[game] def frameDelayMs(state: PlaybackState): Int =
    val recordedDelayMs = state.replayState.delayUntilNextFrameMs.min(Timing.MaxFrameDelayMs)
    (recordedDelayMs / state.speed).toInt

  private def render(
    state: PlaybackState,
    config: AppConfig,
    renderer: Renderer
  ): ZIO[ConsoleService, Throwable, PlaybackState] =
    for
      buffer <- renderer.render(state.replayState.gameState, config, state.previousBuffer)
      rendered = state.copy(previousBuffer = Some(buffer))
      _ <- printStatusLine(rendered, statusText(rendered))
    yield rendered

  private[game] def statusText(state: PlaybackState): String =
    val label    = if state.paused then Status.PausedLabel else Status.PlayingLabel
    val progress = (state.replayState.progress * Status.PercentScale).toInt
    s"$label Speed: ${state.speed}x | Progress: $progress% | ${Status.KeyHelp}"

  /** The status line sits directly below the game screen. */
  private def printStatusLine(state: PlaybackState, text: String): ZIO[ConsoleService, Throwable, Unit] =
    val statusRow = state.previousBuffer.fold(Status.FirstColumn)(_.height + 1)
    val position  = AnsiRenderer.Codes.moveTo(statusRow, Status.FirstColumn)
    ConsoleService.print(s"$position${AnsiRenderer.Codes.ClearLine}$text") *> ConsoleService.flush()

package monadris.infrastructure.game

import zio.*

import monadris.config.AppConfig
import monadris.infrastructure.terminal.ConsoleService
import monadris.infrastructure.terminal.Renderer
import monadris.infrastructure.terminal.TtyService
import monadris.replay.ReplayData
import monadris.replay.ReplayPlayer
import monadris.replay.ReplayPlayerState
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

  private object Ansi:
    val ClearLine: String                  = "\u001b[2K"
    def moveTo(row: Int, col: Int): String = s"\u001b[${row};${col}H"

  private object Keys:
    val SpeedUp: Int   = '+'.toInt
    val SpeedDown: Int = '-'.toInt
    val Quit: Int      = 'q'.toInt
    val QuitUpper: Int = 'Q'.toInt

  final case class PlaybackState(
    replayState: ReplayPlayerState,
    previousBuffer: Option[ScreenBuffer],
    speed: Double,
    paused: Boolean
  )

  def run(
    replayData: ReplayData,
    config: AppConfig,
    renderer: Renderer
  ): ZIO[TtyService & ConsoleService, Throwable, Unit] =
    for
      initialReplayState <- ZIO.succeed(ReplayPlayer.initialize(replayData))
      initialSpeed = Speed.clamp(config.replay.defaultSpeed)
      initialBuffer <- renderReplay(renderer, initialReplayState, config, None, initialSpeed)
      initialPlaybackState = PlaybackState(
        replayState = initialReplayState,
        previousBuffer = Some(initialBuffer),
        speed = initialSpeed,
        paused = false
      )
      finalState <- playbackLoop(initialPlaybackState, config, renderer)
      statusRow = initialBuffer.height + 1
      _ <- ConsoleService.print(s"${Ansi.moveTo(statusRow, 1)}${Ansi.ClearLine}Replay finished. Press any key...")
    yield ()

  private def playbackLoop(
    state: PlaybackState,
    config: AppConfig,
    renderer: Renderer
  ): ZIO[TtyService & ConsoleService, Throwable, PlaybackState] =
    if state.replayState.isFinished then ZIO.succeed(state)
    else
      for
        inputOpt <- TtyService.readByteWithTimeout(frameDelayMs(state))
        result   <- inputOpt match
          case Some(key) => handleKeyPress(key, state, config, renderer)
          case None      => advancePlayback(state, config, renderer)
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
        val newSpeed = (state.speed + Speed.Step).min(Speed.Max)
        for
          newBuffer <- renderReplay(renderer, state.replayState, config, state.previousBuffer, newSpeed)
          newState = state.copy(speed = newSpeed, previousBuffer = Some(newBuffer))
          result <- playbackLoop(newState, config, renderer)
        yield result

      case Keys.SpeedDown =>
        val newSpeed = (state.speed - Speed.Step).max(Speed.Min)
        for
          newBuffer <- renderReplay(renderer, state.replayState, config, state.previousBuffer, newSpeed)
          newState = state.copy(speed = newSpeed, previousBuffer = Some(newBuffer))
          result <- playbackLoop(newState, config, renderer)
        yield result

      case _ =>
        playbackLoop(state, config, renderer)

  private def advancePlayback(
    state: PlaybackState,
    config: AppConfig,
    renderer: Renderer
  ): ZIO[TtyService & ConsoleService, Throwable, PlaybackState] =
    val newReplayState = ReplayPlayer.advanceFrame(state.replayState, config)
    for
      newBuffer <- renderReplay(renderer, newReplayState, config, state.previousBuffer, state.speed)
      newState = state.copy(replayState = newReplayState, previousBuffer = Some(newBuffer))
      result <- playbackLoop(newState, config, renderer)
    yield result

  /** Reproduces the pause the player made before the next frame, scaled by the playback speed. */
  private[game] def frameDelayMs(state: PlaybackState): Int =
    val recordedDelayMs = state.replayState.delayUntilNextFrameMs.min(Timing.MaxFrameDelayMs)
    (recordedDelayMs / state.speed).toInt

  private def renderReplay(
    renderer: Renderer,
    replayState: ReplayPlayerState,
    config: AppConfig,
    previousBuffer: Option[ScreenBuffer],
    speed: Double
  ): ZIO[ConsoleService, Throwable, ScreenBuffer] =
    for
      buffer <- renderer.render(replayState.gameState, config, previousBuffer)
      progress   = (replayState.progress * 100).toInt
      statusRow  = buffer.height + 1
      statusLine = s"[REPLAY] Speed: ${speed}x | Progress: $progress% | Q: Quit, +/-: Speed"
      _ <- ConsoleService.print(s"${Ansi.moveTo(statusRow, 1)}${Ansi.ClearLine}$statusLine")
      _ <- ConsoleService.flush()
    yield buffer

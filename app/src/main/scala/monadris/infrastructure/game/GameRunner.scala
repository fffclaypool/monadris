package monadris.infrastructure.game

import zio.*

import monadris.config.AppConfig
import monadris.domain.*
import monadris.infrastructure.terminal.ConsoleService
import monadris.infrastructure.terminal.Renderer
import monadris.infrastructure.terminal.TtyService
import monadris.view.ScreenBuffer

object GameRunner:

  export monadris.input.GameCommand
  export monadris.infrastructure.terminal.Renderer

  private val renderer: Renderer = Renderer.live

  trait RandomPiece:
    def nextShape: UIO[TetrominoShape]

  object RandomPieceGenerator extends RandomPiece:
    private val shapes = TetrominoShape.values.toVector

    def nextShape: UIO[TetrominoShape] =
      Random.nextIntBounded(shapes.size).map(shapes(_))

  def showTitle: ZIO[ConsoleService, Throwable, Unit] =
    renderer.renderTitle

  def renderGame(
    state: GameState,
    config: AppConfig,
    previousBuffer: Option[ScreenBuffer] = None
  ): ZIO[ConsoleService, Throwable, ScreenBuffer] =
    renderer.render(state, config, previousBuffer)

  def renderGameOver(state: GameState): ZIO[ConsoleService, Throwable, Unit] =
    renderer.renderGameOver(state)

  def interactiveGameLoop(
    initialState: GameState
  ): ZIO[TtyService & ConsoleService & AppConfig, Throwable, GameState] =
    for
      config     <- ZIO.service[AppConfig]
      finalState <- GameLoopRunner.interactiveGameLoop(
        initialState,
        config,
        renderer,
        RandomPieceGenerator
      )
    yield finalState

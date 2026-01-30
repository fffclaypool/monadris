package monadris.infrastructure.terminal

import zio.*

import monadris.config.AppConfig
import monadris.domain.GameState
import monadris.view.GameView
import monadris.view.ScreenBuffer

trait Renderer:
  def render(
    state: GameState,
    config: AppConfig,
    previousBuffer: Option[ScreenBuffer]
  ): ZIO[ConsoleService, Throwable, ScreenBuffer]

  def renderGameOver(state: GameState): ZIO[ConsoleService, Throwable, Unit]

  def renderTitle: ZIO[ConsoleService, Throwable, Unit]

object Renderer:
  val live: Renderer = new Renderer:
    def render(
      state: GameState,
      config: AppConfig,
      previousBuffer: Option[ScreenBuffer]
    ): ZIO[ConsoleService, Throwable, ScreenBuffer] =
      val buffer = GameView.toScreenBuffer(state, config)
      ConsoleRenderer.render(buffer, previousBuffer).as(buffer)

    def renderGameOver(state: GameState): ZIO[ConsoleService, Throwable, Unit] =
      val buffer = GameView.gameOverScreen(state)
      ConsoleRenderer.renderWithoutClear(buffer)

    def renderTitle: ZIO[ConsoleService, Throwable, Unit] =
      val buffer = GameView.titleScreen
      ConsoleRenderer.renderWithoutClear(buffer)

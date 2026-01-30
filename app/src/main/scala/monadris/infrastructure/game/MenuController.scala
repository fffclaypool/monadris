package monadris.infrastructure.game

import zio.*

import monadris.config.AppConfig
import monadris.infrastructure.terminal.ConsoleRenderer
import monadris.infrastructure.terminal.ConsoleService
import monadris.infrastructure.terminal.TtyService
import monadris.view.GameView

object MenuController:

  private object Keys:
    val Enter: Int      = '\r'.toInt
    val EnterLF: Int    = '\n'.toInt
    val Escape: Int     = 27
    val ArrowUp: Char   = 'A'
    val ArrowDown: Char = 'B'
    val KeyK: Int       = 'k'.toInt
    val KeyKUpper: Int  = 'K'.toInt
    val KeyJ: Int       = 'j'.toInt
    val KeyJUpper: Int  = 'J'.toInt
    val KeyQ: Int       = 'q'.toInt
    val KeyQUpper: Int  = 'Q'.toInt

  enum MenuAction:
    case Select(index: Int)
    case Navigate(newIndex: Int)
    case Quit

  def run[R](
    executeMenuItem: Int => ZIO[R, Throwable, Boolean]
  ): ZIO[R & TtyService & ConsoleService & AppConfig, Throwable, Unit] =
    mainMenuLoop(0, executeMenuItem)

  private def mainMenuLoop[R](
    selectedIndex: Int,
    executeMenuItem: Int => ZIO[R, Throwable, Boolean]
  ): ZIO[R & TtyService & ConsoleService & AppConfig, Throwable, Unit] =
    for
      _      <- renderMenu(selectedIndex)
      action <- readMenuInput(selectedIndex)
      _      <- action match
        case MenuAction.Select(index) =>
          executeMenuItem(index).flatMap { shouldContinue =>
            if shouldContinue then mainMenuLoop(index, executeMenuItem)
            else ZIO.unit
          }
        case MenuAction.Navigate(index) =>
          mainMenuLoop(index, executeMenuItem)
        case MenuAction.Quit =>
          showGoodbye
    yield ()

  private def renderMenu(selectedIndex: Int): ZIO[ConsoleService, Throwable, Unit] =
    val buffer = GameView.menuScreen(selectedIndex)
    ConsoleRenderer.render(buffer)

  private def readMenuInput(currentIndex: Int): ZIO[TtyService & AppConfig, Throwable, MenuAction] =
    val menuSize = GameView.MenuItems.all.size
    TtyService.read().flatMap { key =>
      key match
        case Keys.Enter | Keys.EnterLF =>
          ZIO.succeed(MenuAction.Select(currentIndex))
        case Keys.Escape =>
          handleEscapeSequence(currentIndex, menuSize)
        case Keys.KeyK | Keys.KeyKUpper =>
          val newIndex = if currentIndex > 0 then currentIndex - 1 else menuSize - 1
          ZIO.succeed(MenuAction.Navigate(newIndex))
        case Keys.KeyJ | Keys.KeyJUpper =>
          val newIndex = if currentIndex < menuSize - 1 then currentIndex + 1 else 0
          ZIO.succeed(MenuAction.Navigate(newIndex))
        case Keys.KeyQ | Keys.KeyQUpper =>
          ZIO.succeed(MenuAction.Quit)
        case n if n >= '1' && n <= '4' =>
          ZIO.succeed(MenuAction.Select(n - '1'))
        case _ =>
          readMenuInput(currentIndex)
    }

  private def handleEscapeSequence(
    currentIndex: Int,
    menuSize: Int
  ): ZIO[TtyService & AppConfig, Throwable, MenuAction] =
    for
      config    <- ZIO.service[AppConfig]
      _         <- TtyService.sleep(config.terminal.escapeSequenceWaitMs)
      available <- TtyService.available()
      action    <-
        if available <= 0 then ZIO.succeed(MenuAction.Quit)
        else parseArrowKey(currentIndex, menuSize)
    yield action

  private def parseArrowKey(
    currentIndex: Int,
    menuSize: Int
  ): ZIO[TtyService, Throwable, MenuAction] =
    for
      bracket <- TtyService.read()
      action  <-
        if bracket != '[' then ZIO.succeed(MenuAction.Navigate(currentIndex))
        else
          TtyService.read().map { arrow =>
            arrow.toChar match
              case Keys.ArrowUp =>
                val newIndex = if currentIndex > 0 then currentIndex - 1 else menuSize - 1
                MenuAction.Navigate(newIndex)
              case Keys.ArrowDown =>
                val newIndex = if currentIndex < menuSize - 1 then currentIndex + 1 else 0
                MenuAction.Navigate(newIndex)
              case _ =>
                MenuAction.Navigate(currentIndex)
          }
    yield action

  private def showGoodbye: ZIO[ConsoleService, Throwable, Unit] =
    ConsoleRenderer.render(GameView.goodbyeScreen)

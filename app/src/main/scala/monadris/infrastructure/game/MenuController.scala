package monadris.infrastructure.game

import zio.*

import monadris.config.AppConfig
import monadris.infrastructure.terminal.ConsoleRenderer
import monadris.infrastructure.terminal.ConsoleService
import monadris.infrastructure.terminal.TtyService
import monadris.view.GameView

object MenuController:

  private object Keys:
    val Enter: Int       = '\r'.toInt
    val EnterLF: Int     = '\n'.toInt
    val Escape: Int      = 27
    val EndOfStream: Int = -1
    val Bracket: Int     = '['.toInt
    val ArrowUp: Int     = 'A'.toInt
    val ArrowDown: Int   = 'B'.toInt
    val KeyK: Int        = 'k'.toInt
    val KeyKUpper: Int   = 'K'.toInt
    val KeyJ: Int        = 'j'.toInt
    val KeyJUpper: Int   = 'J'.toInt
    val KeyQ: Int        = 'q'.toInt
    val KeyQUpper: Int   = 'Q'.toInt

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
    TtyService.read().flatMap {
      case Keys.Enter | Keys.EnterLF =>
        ZIO.succeed(MenuAction.Select(currentIndex))
      case Keys.Escape =>
        handleEscapeSequence(currentIndex)
      case Keys.KeyK | Keys.KeyKUpper =>
        ZIO.succeed(MenuAction.Navigate(previousIndex(currentIndex)))
      case Keys.KeyJ | Keys.KeyJUpper =>
        ZIO.succeed(MenuAction.Navigate(nextIndex(currentIndex)))
      case Keys.KeyQ | Keys.KeyQUpper | Keys.EndOfStream =>
        ZIO.succeed(MenuAction.Quit)
      case key =>
        shortcutIndex(key) match
          case Some(index) => ZIO.succeed(MenuAction.Select(index))
          case None        => readMenuInput(currentIndex)
    }

  private def menuSize: Int = GameView.MenuItems.all.size

  /** Navigation wraps around at both ends of the menu */
  private def previousIndex(currentIndex: Int): Int = Math.floorMod(currentIndex - 1, menuSize)
  private def nextIndex(currentIndex: Int): Int     = Math.floorMod(currentIndex + 1, menuSize)

  /** The menu item whose shortcut key (as shown in the menu) was pressed */
  private def shortcutIndex(key: Int): Option[Int] =
    Some(GameView.MenuItems.all.indexWhere(_.key.toInt == key)).filter(_ >= 0)

  /** A lone escape quits; an escape followed by more input is an arrow key sequence */
  private def handleEscapeSequence(currentIndex: Int): ZIO[TtyService & AppConfig, Throwable, MenuAction] =
    for
      config    <- ZIO.service[AppConfig]
      _         <- TtyService.sleep(config.terminal.escapeSequenceWaitMs)
      available <- TtyService.available()
      action    <-
        if available <= 0 then ZIO.succeed(MenuAction.Quit)
        else parseArrowKey(currentIndex)
    yield action

  private def parseArrowKey(currentIndex: Int): ZIO[TtyService, Throwable, MenuAction] =
    TtyService.read().flatMap {
      case Keys.Bracket => TtyService.read().map(arrow => MenuAction.Navigate(arrowTarget(arrow, currentIndex)))
      case _            => ZIO.succeed(MenuAction.Navigate(currentIndex))
    }

  private def arrowTarget(arrow: Int, currentIndex: Int): Int =
    arrow match
      case Keys.ArrowUp   => previousIndex(currentIndex)
      case Keys.ArrowDown => nextIndex(currentIndex)
      case _              => currentIndex

  private def showGoodbye: ZIO[ConsoleService, Throwable, Unit] =
    ConsoleRenderer.render(GameView.goodbyeScreen)

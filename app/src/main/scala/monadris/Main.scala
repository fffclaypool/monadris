package monadris

import zio.*
import zio.logging.backend.SLF4J

import monadris.domain.*
import monadris.domain.config.AppConfig
import monadris.infrastructure.io.ConsoleService
import monadris.infrastructure.io.GameEnv
import monadris.infrastructure.io.TerminalControl
import monadris.infrastructure.io.TtyService
import monadris.infrastructure.render.ConsoleRenderer
import monadris.infrastructure.render.Renderer
import monadris.infrastructure.replay.FileReplayRepository
import monadris.infrastructure.replay.ReplayRepository
import monadris.infrastructure.runtime.GameRunner
import monadris.infrastructure.runtime.RecordingGameLoopRunner
import monadris.infrastructure.runtime.ReplayRunner
import monadris.view.GameView

object Main extends ZIOAppDefault:

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

  override def run: Task[Unit] =
    program
      .provideLayer(Runtime.removeDefaultLoggers >>> SLF4J.slf4j ++ GameEnv.live ++ FileReplayRepository.layer)
      .catchAll {
        case error: Config.Error =>
          ZIO.logError(s"Configuration error: ${error.getMessage}")
        case error =>
          ZIO.logError(s"Application failed: $error")
      }

  val program: ZIO[GameEnv & ReplayRepository, Throwable, Unit] =
    ZIO.scoped {
      for
        _ <- showIntro
        _ <- TerminalControl.enableRawMode
        _ <- mainMenuLoop(0).ensuring(TerminalControl.disableRawMode.ignore)
        _ <- ZIO.logInfo("Monadris shutting down...")
      yield ()
    }

  private val showIntro: ZIO[GameEnv, Throwable, Unit] =
    for
      config <- ZIO.service[AppConfig]
      _      <- ZIO.logInfo("Monadris starting...")
      _      <- GameRunner.showTitle
      _      <- TtyService.sleep(config.timing.titleDelayMs)
    yield ()

  private def mainMenuLoop(selectedIndex: Int): ZIO[GameEnv & ReplayRepository, Throwable, Unit] =
    for
      _      <- renderMenu(selectedIndex)
      action <- readMenuInput(selectedIndex)
      result <- action match
        case MenuAction.Select(index)   => executeMenuItem(index)
        case MenuAction.Navigate(index) => mainMenuLoop(index)
        case MenuAction.Quit            => showGoodbye
    yield ()

  private enum MenuAction:
    case Select(index: Int)
    case Navigate(newIndex: Int)
    case Quit

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

  private def executeMenuItem(index: Int): ZIO[GameEnv & ReplayRepository, Throwable, Unit] =
    val menuItems = GameView.MenuItems.all
    if index >= 0 && index < menuItems.size then
      menuItems(index) match
        case GameView.MenuItems.PlayGame      => playGame *> mainMenuLoop(0)
        case GameView.MenuItems.PlayAndRecord => playAndRecord *> mainMenuLoop(1)
        case GameView.MenuItems.WatchReplay   => watchReplay *> mainMenuLoop(2)
        case GameView.MenuItems.ListReplays   => listReplays *> mainMenuLoop(3)
        case GameView.MenuItems.Quit          => showGoodbye
        case _                                => mainMenuLoop(index)
    else mainMenuLoop(0)

  private def showGoodbye: ZIO[ConsoleService, Throwable, Unit] =
    ConsoleRenderer.render(goodbyeScreen)

  private def goodbyeScreen: monadris.view.ScreenBuffer =
    val lines = List(
      "",
      "╔════════════════════════════════════════════════════════════════════╗",
      "║                                                                    ║",
      "║                      Thanks for playing!                           ║",
      "║                                                                    ║",
      "║                        See you again!                              ║",
      "║                                                                    ║",
      "╚════════════════════════════════════════════════════════════════════╝",
      ""
    )
    val width  = lines.map(_.length).maxOption.getOrElse(70)
    val height = lines.length

    lines.zipWithIndex.foldLeft(monadris.view.ScreenBuffer.empty(width, height)) { case (buf, (line, y)) =>
      buf.drawText(0, y, line)
    }

  private def playGame: ZIO[GameEnv, Throwable, Unit] =
    for
      initialState <- initializeGame
      finalState   <- GameRunner.interactiveGameLoop(initialState)
      _            <- showOutro(finalState)
    yield ()

  private def playAndRecord: ZIO[GameEnv & ReplayRepository, Throwable, Unit] =
    for
      _            <- TerminalControl.disableRawMode
      _            <- ConsoleService.print("\r\n\r\nEnter replay name: ")
      name         <- readLine
      _            <- TerminalControl.enableRawMode
      initialState <- initializeGame
      config       <- ZIO.service[AppConfig]
      result       <- RecordingGameLoopRunner.recordingGameLoop(
        initialState,
        config,
        Renderer.live,
        GameRunner.RandomPieceGenerator
      )
      (finalState, replayData) = result
      _ <- ReplayRepository.save(name, replayData)
      _ <- showOutro(finalState)
      _ <- TerminalControl.disableRawMode
      _ <- ConsoleService.print(s"\r\nReplay saved as: $name\r\n")
      _ <- ConsoleService.print("Press any key to continue...")
      _ <- TerminalControl.enableRawMode
      _ <- TtyService.read()
    yield ()

  private def watchReplay: ZIO[GameEnv & ReplayRepository, Throwable, Unit] =
    for
      replays <- ReplayRepository.list
      _       <-
        if replays.isEmpty then showNoReplaysMessage
        else selectAndPlayReplay(replays)
    yield ()

  private def showNoReplaysMessage: ZIO[GameEnv, Throwable, Unit] =
    for
      _ <- TerminalControl.disableRawMode
      _ <- ConsoleService.print("\r\n\r\nNo replays found.\r\n")
      _ <- ConsoleService.print("Press any key to continue...")
      _ <- TerminalControl.enableRawMode
      _ <- TtyService.read()
    yield ()

  private def selectAndPlayReplay(replays: Vector[String]): ZIO[GameEnv & ReplayRepository, Throwable, Unit] =
    for
      _ <- TerminalControl.disableRawMode
      _ <- ConsoleService.print("\r\n\r\nAvailable replays:\r\n")
      _ <- ZIO.foreach(replays.zipWithIndex) { case (name, idx) =>
        ConsoleService.print(s"  ${idx + 1}. $name\r\n")
      }
      _     <- ConsoleService.print("\r\nEnter replay number (or 0 to cancel): ")
      input <- readLine
      _     <- TerminalControl.enableRawMode
      _     <- input.toIntOption match
        case Some(n) if n > 0 && n <= replays.size =>
          val name = replays(n - 1)
          for
            _      <- TerminalControl.disableRawMode
            _      <- ConsoleService.print(s"\r\nLoading replay: $name\r\n")
            _      <- TerminalControl.enableRawMode
            replay <- ReplayRepository.load(name)
            config <- ZIO.service[AppConfig]
            _      <- ReplayRunner.run(replay, config, Renderer.live)
            _      <- TtyService.read()
          yield ()
        case Some(0) => ZIO.unit
        case _       =>
          for
            _ <- TerminalControl.disableRawMode
            _ <- ConsoleService.print("\r\nInvalid selection.\r\n")
            _ <- ConsoleService.print("Press any key to continue...")
            _ <- TerminalControl.enableRawMode
            _ <- TtyService.read()
          yield ()
    yield ()

  private def listReplays: ZIO[GameEnv & ReplayRepository, Throwable, Unit] =
    for
      replays <- ReplayRepository.list
      _       <-
        if replays.isEmpty then showNoReplaysMessage
        else printReplayList(replays)
    yield ()

  private def printReplayList(replays: Vector[String]): ZIO[GameEnv, Throwable, Unit] =
    for
      _ <- TerminalControl.disableRawMode
      _ <- ConsoleService.print("\r\n\r\nSaved replays:\r\n")
      _ <- ZIO.foreach(replays)(name => ConsoleService.print(s"  - $name\r\n"))
      _ <- ConsoleService.print("\r\nPress any key to continue...")
      _ <- TerminalControl.enableRawMode
      _ <- TtyService.read()
    yield ()

  private def initializeGame: ZIO[AppConfig, Nothing, GameState] =
    for
      config     <- ZIO.service[AppConfig]
      firstShape <- GameRunner.RandomPieceGenerator.nextShape
      nextShape  <- GameRunner.RandomPieceGenerator.nextShape
    yield GameState.initial(firstShape, nextShape, config.grid.width, config.grid.height)

  private def showOutro(finalState: GameState): ZIO[GameEnv, Throwable, Unit] =
    for
      config <- ZIO.service[AppConfig]
      _      <- ZIO.logInfo(
        s"Game finished - Score: ${finalState.score}, Lines: ${finalState.linesCleared}, Level: ${finalState.level}"
      )
      _ <- GameRunner.renderGameOver(finalState)
      _ <- TtyService.sleep(config.timing.outroDelayMs)
    yield ()

  private def readLine: ZIO[TtyService, Throwable, String] =
    def loop(acc: StringBuilder): ZIO[TtyService, Throwable, String] =
      TtyService.read().flatMap { ch =>
        if ch == '\n' || ch == '\r' then ZIO.succeed(acc.toString)
        else if ch == 127 || ch == 8 then
          if acc.nonEmpty then
            for
              _      <- ConsoleService.print("\b \b").provideLayer(monadris.infrastructure.io.ConsoleService.live)
              result <- loop(acc.deleteCharAt(acc.length - 1))
            yield result
          else loop(acc)
        else
          for
            _ <- ConsoleService.print(ch.toChar.toString).provideLayer(monadris.infrastructure.io.ConsoleService.live)
            result <- loop(acc.append(ch.toChar))
          yield result
      }
    loop(new StringBuilder)

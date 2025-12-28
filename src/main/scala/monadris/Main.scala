package monadris

import zio.*
import monadris.domain.*
import monadris.logic.*
import monadris.effect.*
import java.io.FileInputStream

/**
 * テトリスのメインエントリーポイント
 * ZIOAppを使用してエフェクトを実行
 */
object Main extends ZIOAppDefault:

  override def run: ZIO[Any, Any, Any] =
    program.catchAll { error =>
      Console.printLineError(s"Error: $error")
    }

  val program: ZIO[Any, Throwable, Unit] =
    ZIO.scoped {
      for
        _ <- printTitle
        _ <- ZIO.sleep(1.second)
        firstShape <- GameRunner.RandomPieceGenerator.nextShape
        nextShape <- GameRunner.RandomPieceGenerator.nextShape
        initialState = GameState.initial(firstShape, nextShape)
        _ <- enableRawMode
        finalState <- interactiveGameLoop(initialState).ensuring(disableRawMode.ignore)
        _ <- GameRunner.ConsoleRenderer.renderGameOver(finalState)
        _ <- Console.printLine("\nGame ended.")
        _ <- ZIO.sleep(2.seconds)
      yield ()
    }

  private val printTitle: ZIO[Any, Throwable, Unit] =
    val lines = List(
      "╔════════════════════════════════════╗",
      "║    🎮 Functional Tetris            ║",
      "║    Scala 3 + ZIO                   ║",
      "╠════════════════════════════════════╣",
      "║  Controls:                         ║",
      "║    ← → or H L : Move left/right    ║",
      "║    ↓ or J     : Soft drop          ║",
      "║    ↑ or K     : Rotate             ║",
      "║    Z          : Rotate CCW         ║",
      "║    Space      : Hard drop          ║",
      "║    P          : Pause              ║",
      "║    Q          : Quit               ║",
      "╚════════════════════════════════════╝",
      ""
    )
    ZIO.foreachDiscard(lines)(Console.printLine(_))

  val enableRawMode: ZIO[Any, Throwable, Unit] =
    execShellCommand("stty raw -echo < /dev/tty")

  val disableRawMode: ZIO[Any, Throwable, Unit] =
    execShellCommand("stty cooked echo < /dev/tty")

  private def execShellCommand(cmd: String): ZIO[Any, Throwable, Unit] =
    ZIO.attempt {
      java.lang.Runtime.getRuntime.exec(Array("/bin/sh", "-c", cmd)).waitFor()
    }.unit

  def interactiveGameLoop(initialState: GameState): ZIO[Any, Throwable, GameState] =
    for
      stateRef <- Ref.make(initialState)
      quitRef <- Ref.make(false)
      _ <- renderCurrentState(stateRef)
      tickFiber <- tickLoop(stateRef, quitRef).fork
      _ <- inputLoop(stateRef, quitRef)
      _ <- tickFiber.interrupt
      finalState <- stateRef.get
    yield finalState

  private def renderCurrentState(stateRef: Ref[GameState]): ZIO[Any, Nothing, Unit] =
    stateRef.get.flatMap(GameRunner.ConsoleRenderer.render)

  def tickLoop(stateRef: Ref[GameState], quitRef: Ref[Boolean]): ZIO[Any, Nothing, Unit] =
    val shouldContinue = checkGameActive(stateRef, quitRef)
    val processTick = processTickUpdate(stateRef)

    val tick = shouldContinue.flatMap {
      case false => ZIO.succeed(false)
      case true  => processTick
    }
    tick.repeatWhile(identity).unit

  private def checkGameActive(
    stateRef: Ref[GameState],
    quitRef: Ref[Boolean]
  ): ZIO[Any, Nothing, Boolean] =
    for
      quit <- quitRef.get
      state <- stateRef.get
    yield !quit && !state.isGameOver

  private def processTickUpdate(stateRef: Ref[GameState]): ZIO[Any, Nothing, Boolean] =
    for
      nextShape <- GameRunner.RandomPieceGenerator.nextShape
      _ <- stateRef.update(s => GameLogic.update(s, Input.Tick, () => nextShape))
      newState <- stateRef.get
      _ <- GameRunner.ConsoleRenderer.render(newState)
      interval = LineClearing.dropInterval(newState.level)
      _ <- ZIO.sleep(Duration.fromMillis(interval))
    yield !newState.isGameOver

  def inputLoop(stateRef: Ref[GameState], quitRef: Ref[Boolean]): ZIO[Any, Throwable, Unit] =
    ZIO.acquireReleaseWith(
      ZIO.attempt(new FileInputStream("/dev/tty"))
    )(fis => ZIO.succeed(fis.close())) { ttyIn =>
      processInputLoop(ttyIn, stateRef, quitRef)
    }

  private def processInputLoop(
    ttyIn: FileInputStream,
    stateRef: Ref[GameState],
    quitRef: Ref[Boolean]
  ): ZIO[Any, Throwable, Unit] =
    val step = checkGameActive(stateRef, quitRef).flatMap {
      case false => ZIO.succeed(false)
      case true  => readAndHandleKey(ttyIn, stateRef, quitRef)
    }
    step.repeatWhile(identity).unit

  private def readAndHandleKey(
    ttyIn: FileInputStream,
    stateRef: Ref[GameState],
    quitRef: Ref[Boolean]
  ): ZIO[Any, Throwable, Boolean] =
    for
      keyOpt <- ZIO.attemptBlocking(readKeyFromTty(ttyIn))
      result <- keyOpt match
        case None      => ZIO.sleep(20.millis).as(true)
        case Some(key) => handleKey(key, ttyIn, stateRef, quitRef)
    yield result

  private def readKeyFromTty(ttyIn: FileInputStream): Option[Int] =
    if ttyIn.available() > 0 then Some(ttyIn.read()) else None

  private def handleKey(
    key: Int,
    ttyIn: FileInputStream,
    stateRef: Ref[GameState],
    quitRef: Ref[Boolean]
  ): ZIO[Any, Throwable, Boolean] =
    if TerminalInput.isQuitKey(key) then
      quitRef.set(true).as(false)
    else
      parseAndApplyInput(key, ttyIn, stateRef)

  private def parseAndApplyInput(
    key: Int,
    ttyIn: FileInputStream,
    stateRef: Ref[GameState]
  ): ZIO[Any, Throwable, Boolean] =
    for
      inputOpt <- ZIO.attemptBlocking(parseKeyToInput(key, ttyIn))
      result <- inputOpt match
        case None        => ZIO.succeed(true)
        case Some(input) => applyInput(input, stateRef)
    yield result

  private def parseKeyToInput(key: Int, ttyIn: FileInputStream): Option[Input] =
    if key == 27 then TerminalInput.parseEscapeSequence(ttyIn)
    else TerminalInput.keyToInput(key)

  private def applyInput(input: Input, stateRef: Ref[GameState]): ZIO[Any, Nothing, Boolean] =
    for
      nextShape <- GameRunner.RandomPieceGenerator.nextShape
      _ <- stateRef.update(s => GameLogic.update(s, input, () => nextShape))
      newState <- stateRef.get
      _ <- GameRunner.ConsoleRenderer.render(newState)
    yield !newState.isGameOver

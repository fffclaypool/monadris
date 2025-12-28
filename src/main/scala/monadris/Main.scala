package monadris

import zio.*
import zio.logging.backend.SLF4J

import monadris.domain.*
import monadris.effect.*

/**
 * テトリスのメインエントリーポイント
 * ZIOAppを使用してエフェクトを実行
 * 構成と起動のみに集中し、ロジックはeffectパッケージに委譲
 */
object Main extends ZIOAppDefault:

  override def run: Task[Unit] =
    program
      .provideLayer(Runtime.removeDefaultLoggers >>> SLF4J.slf4j ++ GameEnv.live)
      .catchAll { error =>
        ZIO.logError(s"Application failed: $error")
      }

  val program: ZIO[GameEnv, Throwable, Unit] =
    ZIO.scoped {
      for
        _            <- showIntro
        initialState <- initializeGame
        finalState   <- runGameSession(initialState)
        _            <- showOutro(finalState)
      yield ()
    }

  private val showIntro: ZIO[GameEnv, Throwable, Unit] =
    for
      _ <- ZIO.logInfo("Monadris starting...")
      _ <- GameRunner.ServiceRenderer.showTitle
      _ <- TtyService.sleep(1000)
    yield ()

  private val initializeGame: UIO[GameState] =
    for
      firstShape <- GameRunner.RandomPieceGenerator.nextShape
      nextShape  <- GameRunner.RandomPieceGenerator.nextShape
    yield GameState.initial(firstShape, nextShape)

  private def runGameSession(initialState: GameState): ZIO[GameEnv, Throwable, GameState] =
    for
      _          <- TerminalControl.enableRawMode
      finalState <- GameRunner.interactiveGameLoop(initialState)
        .ensuring(TerminalControl.disableRawMode.ignore)
    yield finalState

  private def showOutro(finalState: GameState): ZIO[GameEnv, Throwable, Unit] =
    for
      _ <- ZIO.logInfo(s"Game finished - Score: ${finalState.score}, Lines: ${finalState.linesCleared}, Level: ${finalState.level}")
      _ <- GameRunner.ServiceRenderer.renderGameOver(finalState)
      _ <- ConsoleService.print("\nGame ended.\r\n")
      _ <- TtyService.sleep(2000)
      _ <- ZIO.logInfo("Monadris shutting down...")
    yield ()

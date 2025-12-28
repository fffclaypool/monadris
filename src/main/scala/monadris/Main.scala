package monadris

import zio.*

import monadris.domain.*
import monadris.effect.*

/**
 * テトリスのメインエントリーポイント
 * ZIOAppを使用してエフェクトを実行
 * 構成と起動のみに集中し、ロジックはeffectパッケージに委譲
 */
object Main extends ZIOAppDefault:

  override def run: Task[Unit] =
    program.catchAll { error =>
      Console.printLineError(s"Error: $error").orDie
    }

  val program: Task[Unit] =
    ZIO.scoped {
      (for
        _ <- GameRunner.ServiceRenderer.showTitle
        _ <- ZIO.sleep(1.second)
        firstShape <- GameRunner.RandomPieceGenerator.nextShape
        nextShape <- GameRunner.RandomPieceGenerator.nextShape
        initialState = GameState.initial(firstShape, nextShape)
        _ <- TerminalControl.enableRawMode
        finalState <- GameRunner.interactiveGameLoopZIO(initialState)
          .ensuring(TerminalControl.disableRawMode.ignore)
        _ <- GameRunner.ServiceRenderer.renderGameOver(finalState)
        _ <- ConsoleService.print("\nGame ended.\r\n")
        _ <- ZIO.sleep(2.seconds)
      yield ()).provideLayer(GameEnv.live)
    }

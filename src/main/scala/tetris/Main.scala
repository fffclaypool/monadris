package tetris

import zio.*
import tetris.domain.*
import tetris.logic.*
import tetris.effect.*
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
        _ <- Console.printLine("╔════════════════════════════════════╗")
        _ <- Console.printLine("║    🎮 Functional Tetris            ║")
        _ <- Console.printLine("║    Scala 3 + ZIO                   ║")
        _ <- Console.printLine("╠════════════════════════════════════╣")
        _ <- Console.printLine("║  Controls:                         ║")
        _ <- Console.printLine("║    ← → or H L : Move left/right    ║")
        _ <- Console.printLine("║    ↓ or J     : Soft drop          ║")
        _ <- Console.printLine("║    ↑ or K     : Rotate             ║")
        _ <- Console.printLine("║    Z          : Rotate CCW         ║")
        _ <- Console.printLine("║    Space      : Hard drop          ║")
        _ <- Console.printLine("║    P          : Pause              ║")
        _ <- Console.printLine("║    Q          : Quit               ║")
        _ <- Console.printLine("╚════════════════════════════════════╝")
        _ <- Console.printLine("")
        _ <- ZIO.sleep(1.second)  // 説明を読む時間

        // ランダムに最初の2つのピースを生成
        firstShape <- GameRunner.RandomPieceGenerator.nextShape
        nextShape <- GameRunner.RandomPieceGenerator.nextShape

        // 初期状態を生成（純粋関数）
        initialState = GameState.initial(firstShape, nextShape)

        // sttyでraw modeに設定
        _ <- enableRawMode

        // インタラクティブゲームループを実行
        finalState <- interactiveGameLoop(initialState).ensuring(disableRawMode.ignore)

        // ゲームオーバー表示
        _ <- GameRunner.ConsoleRenderer.renderGameOver(finalState)
        _ <- Console.printLine("\nGame ended.")
        _ <- ZIO.sleep(2.seconds)
      yield ()
    }

  /**
   * sttyでraw modeを有効化
   */
  val enableRawMode: ZIO[Any, Throwable, Unit] =
    ZIO.attempt {
      val rt = java.lang.Runtime.getRuntime
      val commands = Array("/bin/sh", "-c", "stty raw -echo < /dev/tty")
      rt.exec(commands).waitFor()
    }.unit

  /**
   * sttyで通常モードに戻す
   */
  val disableRawMode: ZIO[Any, Throwable, Unit] =
    ZIO.attempt {
      val rt = java.lang.Runtime.getRuntime
      val commands = Array("/bin/sh", "-c", "stty cooked echo < /dev/tty")
      rt.exec(commands).waitFor()
    }.unit

  /**
   * インタラクティブなゲームループ
   * キー入力と自動落下を並行処理
   */
  def interactiveGameLoop(
    initialState: GameState
  ): ZIO[Any, Throwable, GameState] =
    for
      // ゲーム状態を保持するRef
      stateRef <- Ref.make(initialState)
      // 終了フラグ
      quitRef <- Ref.make(false)

      // 初期描画
      state <- stateRef.get
      _ <- GameRunner.ConsoleRenderer.render(state)

      // 自動落下のFiber
      tickFiber <- tickLoop(stateRef, quitRef).fork

      // キー入力処理ループ
      _ <- inputLoop(stateRef, quitRef)

      // Tick Fiberを停止
      _ <- tickFiber.interrupt

      finalState <- stateRef.get
    yield finalState

  /**
   * 自動落下ループ
   */
  def tickLoop(
    stateRef: Ref[GameState],
    quitRef: Ref[Boolean]
  ): ZIO[Any, Nothing, Unit] =
    val tick: ZIO[Any, Nothing, Boolean] =
      for
        quit <- quitRef.get
        state <- stateRef.get
        continue <-
          if quit || state.isGameOver then ZIO.succeed(false)
          else
            for
              nextShape <- GameRunner.RandomPieceGenerator.nextShape
              _ <- stateRef.update { s =>
                GameLogic.update(s, Input.Tick, () => nextShape)
              }
              newState <- stateRef.get
              _ <- GameRunner.ConsoleRenderer.render(newState)
              interval = LineClearing.dropInterval(newState.level)
              _ <- ZIO.sleep(Duration.fromMillis(interval))
            yield !newState.isGameOver
      yield continue

    tick.repeatWhile(identity).unit

  /**
   * キー入力処理ループ（/dev/ttyから直接読み取り）
   */
  def inputLoop(
    stateRef: Ref[GameState],
    quitRef: Ref[Boolean]
  ): ZIO[Any, Throwable, Unit] =
    ZIO.acquireReleaseWith(
      ZIO.attempt(new FileInputStream("/dev/tty"))
    )(fis => ZIO.succeed(fis.close())) { ttyIn =>
      val readAndProcess: ZIO[Any, Throwable, Boolean] =
        for
          quit <- quitRef.get
          state <- stateRef.get
          continue <-
            if quit || state.isGameOver then ZIO.succeed(false)
            else
              for
                keyOpt <- ZIO.attemptBlocking {
                  if ttyIn.available() > 0 then Some(ttyIn.read())
                  else None
                }
                cont <- keyOpt match
                  case None =>
                    ZIO.sleep(20.millis).as(true)
                  case Some(key) =>
                    processKey(key, ttyIn, stateRef, quitRef)
              yield cont
        yield continue

      readAndProcess.repeatWhile(identity).unit
    }

  /**
   * キー入力を処理
   */
  def processKey(
    key: Int,
    ttyIn: FileInputStream,
    stateRef: Ref[GameState],
    quitRef: Ref[Boolean]
  ): ZIO[Any, Throwable, Boolean] =
    // ESCシーケンス（矢印キー）の処理
    val inputOpt: ZIO[Any, Throwable, Option[Input]] =
      if key == 27 then  // ESC
        ZIO.attemptBlocking {
          Thread.sleep(20)  // エスケープシーケンスを待つ
          if ttyIn.available() > 0 then
            val second = ttyIn.read()
            if second == '[' then
              Thread.sleep(5)
              if ttyIn.available() > 0 then
                ttyIn.read() match
                  case 'A' => Some(Input.RotateClockwise)  // ↑
                  case 'B' => Some(Input.MoveDown)          // ↓
                  case 'C' => Some(Input.MoveRight)         // →
                  case 'D' => Some(Input.MoveLeft)          // ←
                  case _   => None
              else None
            else None
          else None  // 単独ESCは無視
        }
      else
        ZIO.succeed(keyToInput(key))

    for
      maybeInput <- inputOpt
      continue <- maybeInput match
        case None if key == 'q' || key == 'Q' =>
          quitRef.set(true).as(false)
        case None =>
          ZIO.succeed(true)  // 未知のキー、続行
        case Some(input) =>
          for
            nextShape <- GameRunner.RandomPieceGenerator.nextShape
            _ <- stateRef.update { state =>
              GameLogic.update(state, input, () => nextShape)
            }
            newState <- stateRef.get
            _ <- GameRunner.ConsoleRenderer.render(newState)
          yield !newState.isGameOver
    yield continue

  /**
   * キーコードをInputに変換
   */
  def keyToInput(key: Int): Option[Input] =
    key match
      case 'h' | 'H'       => Some(Input.MoveLeft)
      case 'l' | 'L'       => Some(Input.MoveRight)
      case 'j' | 'J'       => Some(Input.MoveDown)
      case 'k' | 'K'       => Some(Input.RotateClockwise)
      case 'z' | 'Z'       => Some(Input.RotateCounterClockwise)
      case ' '             => Some(Input.HardDrop)
      case 'p' | 'P'       => Some(Input.Pause)
      case 3               => None  // Ctrl+C
      case _               => None

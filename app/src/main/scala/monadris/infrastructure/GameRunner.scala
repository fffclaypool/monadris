package monadris.infrastructure

import zio.*

import monadris.domain.Input
import monadris.domain.config.AppConfig
import monadris.domain.model.game.DomainEvent
import monadris.domain.model.game.GameCommand
import monadris.domain.model.game.TetrisGame
import monadris.domain.model.scoring.ScoreState
import monadris.infrastructure.input.InputTranslator
import monadris.view.GameView
import monadris.view.ScreenBuffer

/**
 * 副作用をZIOで管理するゲーム実行層
 * コアロジックは純粋関数のまま、入出力のみをエフェクトとして扱う
 */
object GameRunner:

  /**
   * イベントループで扱うコマンド
   */
  enum RunnerCommand:
    case UserAction(input: Input) // ユーザー操作
    case TimeTick                 // 時間経過
    case Quit                     // 終了

  /**
   * タイトル画面を表示
   */
  def showTitle: ZIO[ConsoleService, Throwable, Unit] =
    val buffer = GameView.titleScreen
    ConsoleRenderer.renderWithoutClear(buffer)

  /**
   * ゲーム画面を描画（差分描画対応）
   */
  def renderGame(
    game: TetrisGame,
    config: AppConfig,
    previousBuffer: Option[ScreenBuffer] = None
  ): ZIO[ConsoleService, Throwable, ScreenBuffer] =
    val buffer = GameView.toScreenBuffer(game, config)
    ConsoleRenderer.render(buffer, previousBuffer).as(buffer)

  /**
   * ゲームオーバー画面を描画
   */
  def renderGameOver(game: TetrisGame): ZIO[ConsoleService, Throwable, Unit] =
    val buffer = GameView.gameOverScreen(game)
    ConsoleRenderer.renderWithoutClear(buffer)

  // ============================================================
  // イベントループ定数
  // ============================================================

  private object EventLoop:
    val CommandQueueCapacity = 100

  // ============================================================
  // イベント駆動型ゲームループ（Queue版）
  // ============================================================

  /**
   * ゲームループの状態（TetrisGame + 前回描画バッファ）
   * テスト用にパッケージプライベート
   */
  private[infrastructure] case class LoopState(
    game: TetrisGame,
    previousBuffer: Option[ScreenBuffer]
  )

  /**
   * イベント駆動型のゲームループを実行
   * Queueを使って処理を直列化し、競合状態を解消
   */
  def interactiveGameLoop(
    initialGame: TetrisGame
  ): ZIO[TtyService & ConsoleService & AppConfig, Throwable, TetrisGame] =
    for
      config       <- ZIO.service[AppConfig]
      commandQueue <- Queue.bounded[RunnerCommand](EventLoop.CommandQueueCapacity)

      // 初回描画（全描画）
      initialBuffer <- renderGame(initialGame, config, None)

      // 落下間隔を保持するRef（レベル変化に応じて動的に更新される）
      initialInterval = ScoreState.dropInterval(initialGame.scoreState.level, config.speed)
      intervalRef <- Ref.make(initialInterval)

      // Input Fiber: キー入力を読み取り、コマンドをQueueに追加
      inputFiber <- inputProducer(commandQueue, config.terminal.inputPollIntervalMs).fork

      // Tick Fiber: 一定間隔でTickコマンドをQueueに追加
      tickFiber <- tickProducer(commandQueue, intervalRef).fork

      // Main Loop: Queueからコマンドを取り出して処理
      finalLoopState <- eventLoop(
        commandQueue,
        LoopState(initialGame, Some(initialBuffer)),
        config,
        intervalRef
      )

      // Fiberを停止
      _ <- inputFiber.interrupt
      _ <- tickFiber.interrupt
    yield finalLoopState.game

  /**
   * Input Producer: キー入力を読み取り、コマンドをQueueに追加
   */
  private def inputProducer(
    queue: Queue[RunnerCommand],
    pollIntervalMs: Int
  ): ZIO[TtyService & AppConfig, Throwable, Unit] =
    val readAndOffer = for
      parseResult <- TerminalInput.readKeyZIO
      _ <- parseResult match
        case TerminalInput.ParseResult.Timeout =>
          TtyService.sleep(pollIntervalMs)
        case TerminalInput.ParseResult.Regular(key) if TerminalInput.isQuitKey(key) =>
          ZIO.logInfo("Quit key pressed") *> queue.offer(RunnerCommand.Quit)
        case _ =>
          TerminalInput.toInput(parseResult) match
            case Some(input) => queue.offer(RunnerCommand.UserAction(input))
            case None        => ZIO.unit
    yield ()

    readAndOffer.forever

  /**
   * Tick Producer: 一定間隔でTickコマンドをQueueに追加
   * intervalRefを参照して動的に間隔を調整
   */
  private def tickProducer(
    queue: Queue[RunnerCommand],
    intervalRef: Ref[Long]
  ): ZIO[TtyService, Throwable, Unit] =
    val tickAndSleep = for
      interval <- intervalRef.get
      _        <- TtyService.sleep(interval.toInt)
      _        <- queue.offer(RunnerCommand.TimeTick)
    yield ()

    tickAndSleep.forever

  /**
   * イベントループ本体
   * Queueからコマンドを取り出し、状態を更新して描画
   * テスト用にパッケージプライベート
   */
  private[infrastructure] def eventLoop(
    queue: Queue[RunnerCommand],
    state: LoopState,
    config: AppConfig,
    intervalRef: Ref[Long]
  ): ZIO[ConsoleService, Throwable, LoopState] =
    queue.take.flatMap {
      case RunnerCommand.Quit =>
        ZIO.succeed(state)

      case RunnerCommand.UserAction(input) =>
        InputTranslator.translate(input) match
          case Some(cmd) =>
            processCommand(cmd, state, config, intervalRef).flatMap { newState =>
              if newState.game.isOver then ZIO.succeed(newState)
              else eventLoop(queue, newState, config, intervalRef)
            }
          case None =>
            // Quit input
            ZIO.succeed(state)

      case RunnerCommand.TimeTick =>
        processCommand(GameCommand.Tick, state, config, intervalRef).flatMap { newState =>
          if newState.game.isOver then ZIO.succeed(newState)
          else eventLoop(queue, newState, config, intervalRef)
        }
    }

  /**
   * コマンド処理: 状態更新 + イベントハンドリング + 差分描画 + 落下間隔の動的更新
   */
  private def processCommand(
    cmd: GameCommand,
    state: LoopState,
    config: AppConfig,
    intervalRef: Ref[Long]
  ): ZIO[ConsoleService, Throwable, LoopState] =
    val (newGame, events) = state.game.handle(cmd)
    for
      // ドメインイベントをログ出力
      _ <- ZIO.foreach(events)(handleDomainEvent)

      // レベルに応じた落下間隔を計算し、Refを更新（ゲームオーバー時は更新しない）
      newInterval = ScoreState.dropInterval(newGame.scoreState.level, config.speed)
      _         <- ZIO.unless(newGame.isOver)(intervalRef.set(newInterval))
      newBuffer <- renderGame(newGame, config, state.previousBuffer)
    yield LoopState(newGame, Some(newBuffer))

  /**
   * ドメインイベントのハンドリング
   * 将来的にはサウンド再生などの副作用もここで実行
   */
  private def handleDomainEvent(event: DomainEvent): UIO[Unit] =
    event match
      case DomainEvent.LinesCleared(count, scoreGained) =>
        ZIO.logInfo(s"Lines cleared: $count (+$scoreGained points)")
      case DomainEvent.LevelUp(newLevel) =>
        ZIO.logInfo(s"Level up! Now level $newLevel")
      case DomainEvent.GameOver(finalScore) =>
        ZIO.logInfo(s"Game Over - Final score: $finalScore")
      case _ =>
        ZIO.unit

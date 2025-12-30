package monadris.infrastructure

import zio.*

import monadris.domain.config.AppConfig
import monadris.domain.*
import monadris.logic.*
import monadris.view.{GameView, ScreenBuffer}

/**
 * 副作用をZIOで管理するゲーム実行層
 * コアロジックは純粋関数のまま、入出力のみをエフェクトとして扱う
 */
object GameRunner:

  /**
   * イベントループで扱うコマンド
   */
  enum GameCommand:
    case UserAction(input: Input) // ユーザー操作
    case TimeTick                 // 時間経過
    case Quit                     // 終了

  /**
   * 描画を抽象化するトレイト（依存性注入用）
   */
  trait Renderer:
    def render(state: GameState): UIO[Unit]
    def renderGameOver(state: GameState): UIO[Unit]

  /**
   * 乱数生成を抽象化
   */
  trait RandomPiece:
    def nextShape: UIO[TetrominoShape]

  /**
   * ランダムピース生成器
   */
  object RandomPieceGenerator extends RandomPiece:
    private val shapes = TetrominoShape.values.toVector

    def nextShape: UIO[TetrominoShape] =
      Random.nextIntBounded(shapes.size).map(shapes(_))

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
    state: GameState,
    config: AppConfig,
    previousBuffer: Option[ScreenBuffer] = None
  ): ZIO[ConsoleService, Throwable, ScreenBuffer] =
    val buffer = GameView.toScreenBuffer(state, config)
    ConsoleRenderer.render(buffer, previousBuffer).as(buffer)

  /**
   * ゲームオーバー画面を描画
   */
  def renderGameOver(state: GameState): ZIO[ConsoleService, Throwable, Unit] =
    val buffer = GameView.gameOverScreen(state)
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
   * ゲームループの状態（GameState + 前回描画バッファ）
   * テスト用にパッケージプライベート
   */
  private[infrastructure] case class LoopState(
    gameState: GameState,
    previousBuffer: Option[ScreenBuffer]
  )

  /**
   * イベント駆動型のゲームループを実行
   * Queueを使って処理を直列化し、競合状態を解消
   */
  def interactiveGameLoop(
    initialState: GameState
  ): ZIO[TtyService & ConsoleService & AppConfig, Throwable, GameState] =
    for
      config       <- ZIO.service[AppConfig]
      commandQueue <- Queue.bounded[GameCommand](EventLoop.CommandQueueCapacity)

      // 初回描画（全描画）
      initialBuffer <- renderGame(initialState, config, None)

      // Input Fiber: キー入力を読み取り、コマンドをQueueに追加
      inputFiber <- inputProducer(commandQueue, config.terminal.inputPollIntervalMs).fork

      // Tick Fiber: 一定間隔でTickコマンドをQueueに追加
      initialInterval = LineClearing.dropInterval(initialState.level, config.speed).toInt
      tickFiber <- tickProducer(commandQueue, initialInterval).fork

      // Main Loop: Queueからコマンドを取り出して処理
      finalLoopState <- eventLoop(
        commandQueue,
        LoopState(initialState, Some(initialBuffer)),
        config
      )

      // Fiberを停止
      _ <- inputFiber.interrupt
      _ <- tickFiber.interrupt
    yield finalLoopState.gameState

  /**
   * Input Producer: キー入力を読み取り、コマンドをQueueに追加
   */
  private def inputProducer(
    queue: Queue[GameCommand],
    pollIntervalMs: Int
  ): ZIO[TtyService & AppConfig, Throwable, Unit] =
    val readAndOffer = for
      parseResult <- TerminalInput.readKeyZIO
      _ <- parseResult match
        case TerminalInput.ParseResult.Timeout =>
          TtyService.sleep(pollIntervalMs)
        case TerminalInput.ParseResult.Regular(key) if TerminalInput.isQuitKey(key) =>
          ZIO.logInfo("Quit key pressed") *> queue.offer(GameCommand.Quit)
        case _ =>
          TerminalInput.toInput(parseResult) match
            case Some(input) => queue.offer(GameCommand.UserAction(input))
            case None        => ZIO.unit
    yield ()

    readAndOffer.forever

  /**
   * Tick Producer: 一定間隔でTickコマンドをQueueに追加
   * Note: 現在は固定間隔。レベル変化への追従は将来の拡張として検討
   */
  private def tickProducer(
    queue: Queue[GameCommand],
    intervalMs: Int
  ): ZIO[TtyService, Throwable, Unit] =
    val tickAndSleep = for
      _ <- TtyService.sleep(intervalMs)
      _ <- queue.offer(GameCommand.TimeTick)
    yield ()

    tickAndSleep.forever

  /**
   * イベントループ本体
   * Queueからコマンドを取り出し、状態を更新して描画
   * テスト用にパッケージプライベート
   */
  private[infrastructure] def eventLoop(
    queue: Queue[GameCommand],
    state: LoopState,
    config: AppConfig
  ): ZIO[ConsoleService, Throwable, LoopState] =
    queue.take.flatMap {
      case GameCommand.Quit =>
        ZIO.succeed(state)

      case GameCommand.UserAction(input) =>
        processCommand(input, state, config).flatMap { newState =>
          if newState.gameState.isGameOver then
            ZIO.succeed(newState)
          else
            eventLoop(queue, newState, config)
        }

      case GameCommand.TimeTick =>
        processCommand(Input.Tick, state, config).flatMap { newState =>
          if newState.gameState.isGameOver then
            ZIO.succeed(newState)
          else
            eventLoop(queue, newState, config)
        }
    }

  /**
   * コマンド処理: 状態更新 + 差分描画
   */
  private def processCommand(
    input: Input,
    state: LoopState,
    config: AppConfig
  ): ZIO[ConsoleService, Throwable, LoopState] =
    for
      nextShape <- RandomPieceGenerator.nextShape
      oldGameState = state.gameState
      newGameState = GameLogic.update(oldGameState, input, () => nextShape, config)
      _ <- ZIO.when(newGameState.isGameOver && !oldGameState.isGameOver) {
        ZIO.logInfo(s"Game Over - Score: ${newGameState.score}, Lines: ${newGameState.linesCleared}, Level: ${newGameState.level}")
      }
      newBuffer <- renderGame(newGameState, config, state.previousBuffer)
    yield LoopState(newGameState, Some(newBuffer))

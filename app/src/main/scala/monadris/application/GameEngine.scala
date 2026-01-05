package monadris.application

import zio.*

import monadris.domain.config.AppConfig
import monadris.domain.model.game.DomainEvent
import monadris.domain.model.game.GameCommand
import monadris.domain.model.game.TetrisGame
import monadris.domain.model.scoring.ScoreState
import monadris.infrastructure.*
import monadris.view.GameView
import monadris.view.ScreenBuffer

/**
 * ゲームエンジン - ZIOの同時実行性を活用したゲームループ
 *
 * 責務:
 * - Ref[TetrisGame]による状態管理
 * - Queue[GameCommand]によるコマンド集約
 * - 入力/Tickerファイバーの管理
 * - ドメインイベントの処理
 * - 描画の調整
 */
object GameEngine:

  // ============================================================
  // 定数
  // ============================================================

  private object Constants:
    val CommandQueueCapacity = 100

  // ============================================================
  // パブリックAPI
  // ============================================================

  /**
   * ゲームセッションを実行
   * 初期化からゲームオーバーまでの全ライフサイクルを管理
   *
   * @param initialGame 初期ゲーム状態
   * @return 最終ゲーム状態
   */
  def runSession(
    initialGame: TetrisGame
  ): ZIO[Terminal & AppConfig, Throwable, TetrisGame] =
    for
      config <- ZIO.service[AppConfig]

      // 状態管理用のRef
      gameRef <- Ref.make(initialGame)

      // コマンドキュー（Input/Tickerから集約）
      commandQueue <- Queue.bounded[GameCommand](Constants.CommandQueueCapacity)

      // 終了シグナル用のPromise
      quitSignal <- Promise.make[Nothing, Unit]

      // 落下間隔を保持するRef（レベル変化に応じて動的に更新）
      initialInterval = ScoreState.dropInterval(initialGame.scoreState.level, config.speed)
      intervalRef <- Ref.make(initialInterval)

      // 描画バッファを保持するRef（差分描画用）
      bufferRef <- Ref.make(Option.empty[ScreenBuffer])

      // 初回描画
      _ <- renderGame(gameRef, config, bufferRef)

      // ファイバーを起動してメインループを実行
      finalGame <- runWithFibers(
        gameRef,
        commandQueue,
        quitSignal,
        intervalRef,
        bufferRef,
        config
      )
    yield finalGame

  // ============================================================
  // プライベート実装
  // ============================================================

  /**
   * ファイバーを起動してメインループを実行
   */
  private def runWithFibers(
    gameRef: Ref[TetrisGame],
    commandQueue: Queue[GameCommand],
    quitSignal: Promise[Nothing, Unit],
    intervalRef: Ref[Long],
    bufferRef: Ref[Option[ScreenBuffer]],
    config: AppConfig
  ): ZIO[Terminal & AppConfig, Throwable, TetrisGame] =
    for
      // Input Fiber: キー入力を監視
      inputFiber <- InputLoop.start(commandQueue, quitSignal, config.terminal.inputPollIntervalMs)

      // Ticker Fiber: 一定間隔でTickを生成
      tickerFiber <- GameTicker.start(commandQueue, intervalRef)

      // Main Loop: Queueからコマンドを取り出して処理
      finalGame <- mainLoop(gameRef, commandQueue, quitSignal, intervalRef, bufferRef, config)
        .ensuring(
          // ファイバーをクリーンアップ
          inputFiber.interrupt *> tickerFiber.interrupt
        )
    yield finalGame

  /**
   * メインループ
   * Queueからコマンドを取り出し、状態を更新して描画
   */
  private def mainLoop(
    gameRef: Ref[TetrisGame],
    commandQueue: Queue[GameCommand],
    quitSignal: Promise[Nothing, Unit],
    intervalRef: Ref[Long],
    bufferRef: Ref[Option[ScreenBuffer]],
    config: AppConfig
  ): ZIO[Any, Throwable, TetrisGame] =
    val processOneCommand = for
      // Quit or Commandを競合させる（Option型で統一）
      commandOrQuit <- commandQueue.take.map(Some(_)).race(quitSignal.await.as(None))
      result <- commandOrQuit match
        case Some(cmd) =>
          processCommand(cmd, gameRef, intervalRef, bufferRef, config).map { shouldContinue =>
            if shouldContinue then None else Some(())
          }
        case None =>
          // Quit signal received
          ZIO.succeed(Some(()))
    yield result

    // コマンド処理をQuitまたはGameOverまで繰り返す
    processUntilDone(processOneCommand, gameRef, quitSignal)

  /**
   * 終了条件を満たすまでコマンドを処理
   */
  private def processUntilDone(
    processOne: ZIO[Any, Throwable, Option[Unit]],
    gameRef: Ref[TetrisGame],
    quitSignal: Promise[Nothing, Unit]
  ): ZIO[Any, Throwable, TetrisGame] =
    val loopBody = for
      // コマンドを処理（QuitシグナルとRace）
      maybeQuit <- processOne.race(quitSignal.await.as(Some(())))
      game      <- gameRef.get
      // 終了判定: Quit, GameOver, またはシグナル
      shouldStop = maybeQuit.isDefined || game.isOver
    yield (shouldStop, game)

    loopBody.repeatUntil(_._1).map(_._2)

  /**
   * 単一コマンドを処理
   *
   * @return true: 継続, false: 終了
   */
  private def processCommand(
    cmd: GameCommand,
    gameRef: Ref[TetrisGame],
    intervalRef: Ref[Long],
    bufferRef: Ref[Option[ScreenBuffer]],
    config: AppConfig
  ): ZIO[Any, Throwable, Boolean] =
    for
      // 1. 現在の状態を取得
      currentGame <- gameRef.get

      // 2. 純粋関数で状態を更新
      (newGame, events) = currentGame.handle(cmd)

      // 3. 新しい状態を保存
      _ <- gameRef.set(newGame)

      // 4. ドメインイベントを処理
      _ <- ZIO.foreach(events)(handleDomainEvent)

      // 5. 落下間隔を更新（ゲームオーバー時は更新しない）
      _ <- ZIO.unless(newGame.isOver) {
        val newInterval = ScoreState.dropInterval(newGame.scoreState.level, config.speed)
        intervalRef.set(newInterval)
      }

      // 6. 描画
      _ <- renderGame(gameRef, config, bufferRef)
    yield !newGame.isOver

  /**
   * ゲーム画面を描画（差分描画対応）
   */
  private def renderGame(
    gameRef: Ref[TetrisGame],
    config: AppConfig,
    bufferRef: Ref[Option[ScreenBuffer]]
  ): ZIO[Any, Throwable, Unit] =
    for
      game           <- gameRef.get
      previousBuffer <- bufferRef.get
      currentBuffer = GameView.toScreenBuffer(game, config)
      _ <- ConsoleRenderer.render(currentBuffer, previousBuffer)
      _ <- bufferRef.set(Some(currentBuffer))
    yield ()

  /**
   * ドメインイベントのハンドリング
   * ログ出力や将来的なサウンド再生などの副作用を実行
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

  // ============================================================
  // ユーティリティ（タイトル/ゲームオーバー画面用）
  // ============================================================

  /**
   * タイトル画面を表示
   */
  def showTitleScreen: Task[Unit] =
    val buffer = GameView.titleScreen
    ConsoleRenderer.renderWithoutClear(buffer)

  /**
   * ゲームオーバー画面を表示
   */
  def showGameOverScreen(game: TetrisGame): Task[Unit] =
    val buffer = GameView.gameOverScreen(game)
    ConsoleRenderer.renderWithoutClear(buffer)

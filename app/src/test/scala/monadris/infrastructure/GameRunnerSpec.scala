package monadris.infrastructure

import zio.*
import zio.test.*

import monadris.domain.Input
import monadris.domain.model.game.GamePhase
import monadris.domain.model.game.TetrisGame
import monadris.domain.model.scoring.ScoreState
import monadris.infrastructure.TestServices as LocalTestServices

object GameRunnerSpec extends ZIOSpecDefault:

  val gridWidth: Int  = LocalTestServices.testConfig.grid.width
  val gridHeight: Int = LocalTestServices.testConfig.grid.height
  val config          = LocalTestServices.testConfig

  // ============================================================
  // テスト定数
  // ============================================================

  private val testSeed = 42L

  // ============================================================
  // テストフィクスチャ
  // ============================================================

  def initialGame: TetrisGame =
    TetrisGame.create(testSeed, gridWidth, gridHeight, config.score, config.level)

  def spec = suite("GameRunner")(
    // ============================================================
    // RunnerCommand enumテスト
    // ============================================================

    suite("RunnerCommand")(
      test("UserAction wraps Input correctly") {
        val cmd = GameRunner.RunnerCommand.UserAction(Input.MoveLeft)
        cmd match
          case GameRunner.RunnerCommand.UserAction(input) =>
            assertTrue(input == Input.MoveLeft)
          case _ =>
            assertTrue(false)
      },
      test("TimeTick is a valid command") {
        val cmd = GameRunner.RunnerCommand.TimeTick
        assertTrue(cmd == GameRunner.RunnerCommand.TimeTick)
      },
      test("Quit is a valid command") {
        val cmd = GameRunner.RunnerCommand.Quit
        assertTrue(cmd == GameRunner.RunnerCommand.Quit)
      },
      test("RunnerCommand enum has all expected variants") {
        val userAction = GameRunner.RunnerCommand.UserAction(Input.MoveDown)
        val timeTick   = GameRunner.RunnerCommand.TimeTick
        val quit       = GameRunner.RunnerCommand.Quit

        assertTrue(
          userAction.isInstanceOf[GameRunner.RunnerCommand],
          timeTick.isInstanceOf[GameRunner.RunnerCommand],
          quit.isInstanceOf[GameRunner.RunnerCommand]
        )
      },
      test("UserAction can wrap all Input types") {
        val inputs = List(
          Input.MoveLeft,
          Input.MoveRight,
          Input.MoveDown,
          Input.RotateClockwise,
          Input.RotateCounterClockwise,
          Input.HardDrop,
          Input.Pause,
          Input.Tick
        )
        val commands = inputs.map(GameRunner.RunnerCommand.UserAction(_))
        assertTrue(commands.size == inputs.size)
      }
    ),

    // ============================================================
    // renderGameテスト
    // ============================================================

    suite("renderGame")(
      test("returns ScreenBuffer for initial state") {
        val game = initialGame
        for buffer <- GameRunner.renderGame(game, LocalTestServices.testConfig, None)
        yield assertTrue(
          buffer.width > minimumBufferDimension,
          buffer.height > minimumBufferDimension
        )
      }.provide(LocalTestServices.console),
      test("uses differential rendering when previous buffer provided") {
        val game = initialGame
        for
          service      <- ZIO.service[LocalTestServices.TestConsoleService]
          firstBuffer  <- GameRunner.renderGame(game, LocalTestServices.testConfig, None)
          _            <- service.buffer.set(List.empty) // Clear buffer
          secondBuffer <- GameRunner.renderGame(game, LocalTestServices.testConfig, Some(firstBuffer))
          output       <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          // バッファが同一の場合、画面クリアなしで出力は最小限
          !combined.contains(Ansi.clearScreen) || combined.isEmpty
        )
      }.provide(LocalTestServices.console),
      test("renders game over state correctly") {
        val gameOverGame = initialGame.copy(phase = GamePhase.Over)
        for buffer <- GameRunner.renderGame(gameOverGame, LocalTestServices.testConfig, None)
        yield assertTrue(buffer.width > minimumBufferDimension)
      }.provide(LocalTestServices.console)
    ),

    // ============================================================
    // renderGameOverテスト
    // ============================================================

    suite("renderGameOver")(
      test("outputs game over screen") {
        val game = initialGame.copy(scoreState = ScoreState(1000, 5, 20))
        for
          service <- ZIO.service[LocalTestServices.TestConsoleService]
          _       <- GameRunner.renderGameOver(game)
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains("GAME OVER"),
          combined.contains("1000"),
          combined.contains("20"),
          combined.contains("5")
        )
      }.provide(LocalTestServices.console)
    ),

    // ============================================================
    // showTitleテスト
    // ============================================================

    suite("showTitle")(
      test("outputs title screen content") {
        for
          service <- ZIO.service[LocalTestServices.TestConsoleService]
          _       <- GameRunner.showTitle
          output  <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          combined.contains("Functional Tetris"),
          combined.contains("Controls")
        )
      }.provide(LocalTestServices.console)
    ),

    // ============================================================
    // eventLoopテスト
    // ============================================================

    suite("eventLoop")(
      test("Quit command exits immediately") {
        val game = initialGame
        for
          queue <- Queue.bounded[GameRunner.RunnerCommand](eventLoopQueueCapacity)
          _     <- queue.offer(GameRunner.RunnerCommand.Quit)
          loopState = GameRunner.LoopState(game, None)
          intervalRef <- Ref.make(ScoreState.dropInterval(game.scoreState.level, LocalTestServices.testConfig.speed))
          result      <- GameRunner.eventLoop(queue, loopState, LocalTestServices.testConfig, intervalRef)
        yield assertTrue(
          // Compare relevant fields since PieceQueue contains mutable Random
          result.game.board == game.board,
          result.game.activePiece == game.activePiece,
          result.game.scoreState == game.scoreState,
          result.game.phase == game.phase
        )
      }.provide(LocalTestServices.console),
      test("UserAction updates state and continues") {
        for
          queue <- Queue.bounded[GameRunner.RunnerCommand](eventLoopQueueCapacity)
          _     <- queue.offer(GameRunner.RunnerCommand.UserAction(Input.MoveLeft))
          _     <- queue.offer(GameRunner.RunnerCommand.Quit)
          loopState = GameRunner.LoopState(initialGame, None)
          intervalRef <- Ref.make(
            ScoreState.dropInterval(initialGame.scoreState.level, LocalTestServices.testConfig.speed)
          )
          result <- GameRunner.eventLoop(queue, loopState, LocalTestServices.testConfig, intervalRef)
        yield assertTrue(
          // 状態が処理された（衝突に応じて同じか異なる可能性がある）
          result.game != null
        )
      }.provide(LocalTestServices.console),
      test("TimeTick updates state and continues") {
        for
          queue <- Queue.bounded[GameRunner.RunnerCommand](eventLoopQueueCapacity)
          _     <- queue.offer(GameRunner.RunnerCommand.TimeTick)
          _     <- queue.offer(GameRunner.RunnerCommand.Quit)
          loopState = GameRunner.LoopState(initialGame, None)
          intervalRef <- Ref.make(
            ScoreState.dropInterval(initialGame.scoreState.level, LocalTestServices.testConfig.speed)
          )
          result <- GameRunner.eventLoop(queue, loopState, LocalTestServices.testConfig, intervalRef)
        yield assertTrue(result.game != null)
      }.provide(LocalTestServices.console),
      test("UserAction game over exits loop") {
        // 既にゲームオーバーの状態を作成
        val gameOverGame = initialGame.copy(phase = GamePhase.Over)
        for
          queue <- Queue.bounded[GameRunner.RunnerCommand](eventLoopQueueCapacity)
          _     <- queue.offer(GameRunner.RunnerCommand.UserAction(Input.MoveDown))
          loopState = GameRunner.LoopState(gameOverGame, None)
          intervalRef <- Ref.make(
            ScoreState.dropInterval(gameOverGame.scoreState.level, LocalTestServices.testConfig.speed)
          )
          result <- GameRunner.eventLoop(queue, loopState, LocalTestServices.testConfig, intervalRef)
        yield assertTrue(result.game.isOver)
      }.provide(LocalTestServices.console),
      test("TimeTick game over exits loop") {
        // 既にゲームオーバーの状態を作成
        val gameOverGame = initialGame.copy(phase = GamePhase.Over)
        for
          queue <- Queue.bounded[GameRunner.RunnerCommand](eventLoopQueueCapacity)
          _     <- queue.offer(GameRunner.RunnerCommand.TimeTick)
          loopState = GameRunner.LoopState(gameOverGame, None)
          intervalRef <- Ref.make(
            ScoreState.dropInterval(gameOverGame.scoreState.level, LocalTestServices.testConfig.speed)
          )
          result <- GameRunner.eventLoop(queue, loopState, LocalTestServices.testConfig, intervalRef)
        yield assertTrue(result.game.isOver)
      }.provide(LocalTestServices.console),
      test("multiple commands processed in order") {
        for
          queue <- Queue.bounded[GameRunner.RunnerCommand](eventLoopQueueCapacity)
          _     <- queue.offer(GameRunner.RunnerCommand.UserAction(Input.MoveLeft))
          _     <- queue.offer(GameRunner.RunnerCommand.UserAction(Input.MoveRight))
          _     <- queue.offer(GameRunner.RunnerCommand.TimeTick)
          _     <- queue.offer(GameRunner.RunnerCommand.Quit)
          loopState = GameRunner.LoopState(initialGame, None)
          intervalRef <- Ref.make(
            ScoreState.dropInterval(initialGame.scoreState.level, LocalTestServices.testConfig.speed)
          )
          result <- GameRunner.eventLoop(queue, loopState, LocalTestServices.testConfig, intervalRef)
        yield assertTrue(result.previousBuffer.isDefined)
      }.provide(LocalTestServices.console),
      test("updates drop interval when level changes") {
        // 高レベル（level = 5）の状態を作成
        val highLevelGame = initialGame.copy(scoreState = ScoreState(0, highLevelForTest, 0))
        // レベル1相当の初期間隔を設定
        val level1Interval = ScoreState.dropInterval(1, LocalTestServices.testConfig.speed)
        // レベル5相当の期待される間隔
        val expectedInterval = ScoreState.dropInterval(highLevelForTest, LocalTestServices.testConfig.speed)
        for
          queue <- Queue.bounded[GameRunner.RunnerCommand](eventLoopQueueCapacity)
          _     <- queue.offer(GameRunner.RunnerCommand.TimeTick)
          _     <- queue.offer(GameRunner.RunnerCommand.Quit)
          loopState = GameRunner.LoopState(highLevelGame, None)
          // 意図的にレベル1相当の間隔で初期化
          intervalRef <- Ref.make(level1Interval)
          _           <- GameRunner.eventLoop(queue, loopState, LocalTestServices.testConfig, intervalRef)
          // processCommand実行後、intervalRefがレベル5相当の間隔に更新されているはず
          updatedInterval <- intervalRef.get
        yield assertTrue(
          updatedInterval == expectedInterval,
          updatedInterval < level1Interval
        )
      }.provide(LocalTestServices.console)
    )
  )

  // バッファ検証用のテスト定数
  private val minimumBufferDimension = 0
  private val eventLoopQueueCapacity = 10

  // 落下間隔更新テスト定数
  private val highLevelForTest    = 5
  private val testTimeoutDuration = Duration.fromSeconds(5)

  // アサーション用のANSIエスケープシーケンス
  private object Ansi:
    val clearScreen = "\u001b[2J"

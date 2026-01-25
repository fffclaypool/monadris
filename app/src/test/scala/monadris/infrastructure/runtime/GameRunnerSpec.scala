package monadris.infrastructure.runtime

import zio.*
import zio.test.*

import monadris.domain.*
import monadris.domain.config.AppConfig
import monadris.infrastructure.io.ConsoleService
import monadris.infrastructure.io.TestServices as LocalTestServices
import monadris.logic.LineClearing
import monadris.view.ScreenBuffer

object GameRunnerSpec extends ZIOSpecDefault:

  val gridWidth: Int  = LocalTestServices.testConfig.grid.width
  val gridHeight: Int = LocalTestServices.testConfig.grid.height

  private val shapeSampleCount    = 100
  private val minimumUniqueShapes = 1
  private val tripleRenderCount   = 3

  def initialState: GameState =
    GameState.initial(TetrominoShape.T, TetrominoShape.I, gridWidth, gridHeight)

  case class MockRendererState(renderCount: Int = 0, gameOverRendered: Boolean = false)

  class MockRenderer(stateRef: Ref[MockRendererState]) extends GameRunner.Renderer:
    private val emptyBuffer = ScreenBuffer.empty(0, 0)

    def render(
      state: GameState,
      config: AppConfig,
      previousBuffer: Option[ScreenBuffer]
    ): ZIO[ConsoleService, Throwable, ScreenBuffer] =
      stateRef.update(s => s.copy(renderCount = s.renderCount + 1)).as(emptyBuffer)

    def renderGameOver(state: GameState): ZIO[ConsoleService, Throwable, Unit] =
      stateRef.update(_.copy(gameOverRendered = true))

    def renderTitle: ZIO[ConsoleService, Throwable, Unit] =
      ZIO.unit

    def getRenderCount: UIO[Int]          = stateRef.get.map(_.renderCount)
    def wasGameOverRendered: UIO[Boolean] = stateRef.get.map(_.gameOverRendered)

  object MockRenderer:
    def make: UIO[MockRenderer] =
      Ref.make(MockRendererState()).map(new MockRenderer(_))

  def spec = suite("GameRunner")(
    suite("GameCommand")(
      test("UserAction wraps Input correctly") {
        val cmd = GameRunner.GameCommand.UserAction(Input.MoveLeft)
        cmd match
          case GameRunner.GameCommand.UserAction(input) =>
            assertTrue(input == Input.MoveLeft)
          case _ =>
            assertTrue(false)
      },
      test("TimeTick is a valid command") {
        val cmd = GameRunner.GameCommand.TimeTick
        assertTrue(cmd == GameRunner.GameCommand.TimeTick)
      },
      test("Quit is a valid command") {
        val cmd = GameRunner.GameCommand.Quit
        assertTrue(cmd == GameRunner.GameCommand.Quit)
      },
      test("GameCommand enum has all expected variants") {
        val userAction = GameRunner.GameCommand.UserAction(Input.MoveDown)
        val timeTick   = GameRunner.GameCommand.TimeTick
        val quit       = GameRunner.GameCommand.Quit

        assertTrue(
          userAction.isInstanceOf[GameRunner.GameCommand],
          timeTick.isInstanceOf[GameRunner.GameCommand],
          quit.isInstanceOf[GameRunner.GameCommand]
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
        val commands = inputs.map(GameRunner.GameCommand.UserAction(_))
        assertTrue(commands.size == inputs.size)
      }
    ),

    suite("RandomPieceGenerator")(
      test("nextShape returns valid tetromino shape") {
        for shape <- GameRunner.RandomPieceGenerator.nextShape
        yield assertTrue(TetrominoShape.values.contains(shape))
      },
      test("nextShape returns different shapes over multiple calls") {
        for
          shapes <- ZIO.collectAll(List.fill(shapeSampleCount)(GameRunner.RandomPieceGenerator.nextShape))
          uniqueShapes = shapes.toSet
        yield assertTrue(uniqueShapes.size > minimumUniqueShapes)
      }
    ),

    suite("Renderer trait")(
      test("MockRenderer correctly tracks render calls") {
        val state = initialState

        for
          renderer <- MockRenderer.make
          _        <- renderer.render(state, LocalTestServices.testConfig, None)
          _        <- renderer.render(state, LocalTestServices.testConfig, None)
          _        <- renderer.render(state, LocalTestServices.testConfig, None)
          count    <- renderer.getRenderCount
        yield assertTrue(count == tripleRenderCount)
      }.provide(LocalTestServices.console),
      test("MockRenderer correctly tracks gameOver") {
        val state = initialState.copy(status = GameStatus.GameOver)

        for
          renderer    <- MockRenderer.make
          _           <- renderer.renderGameOver(state)
          wasRendered <- renderer.wasGameOverRendered
        yield assertTrue(wasRendered)
      }.provide(LocalTestServices.console)
    ),

    suite("renderGame")(
      test("returns ScreenBuffer for initial state") {
        val state = initialState
        for buffer <- GameRunner.renderGame(state, LocalTestServices.testConfig, None)
        yield assertTrue(
          buffer.width > minimumBufferDimension,
          buffer.height > minimumBufferDimension
        )
      }.provide(LocalTestServices.console),
      test("uses differential rendering when previous buffer provided") {
        val state = initialState
        for
          service      <- ZIO.service[LocalTestServices.TestConsoleService]
          firstBuffer  <- GameRunner.renderGame(state, LocalTestServices.testConfig, None)
          _            <- service.buffer.set(List.empty)
          secondBuffer <- GameRunner.renderGame(state, LocalTestServices.testConfig, Some(firstBuffer))
          output       <- service.buffer.get
          combined = output.mkString
        yield assertTrue(
          !combined.contains(Ansi.clearScreen) || combined.isEmpty
        )
      }.provide(LocalTestServices.console),
      test("renders game over state correctly") {
        val gameOverState = initialState.copy(status = GameStatus.GameOver)
        for buffer <- GameRunner.renderGame(gameOverState, LocalTestServices.testConfig, None)
        yield assertTrue(buffer.width > minimumBufferDimension)
      }.provide(LocalTestServices.console)
    ),

    suite("renderGameOver")(
      test("outputs game over screen") {
        val state = initialState.copy(score = 1000, level = 5, linesCleared = 20)
        for
          service <- ZIO.service[LocalTestServices.TestConsoleService]
          _       <- GameRunner.renderGameOver(state)
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

    suite("eventLoop")(
      test("Quit command exits immediately") {
        for
          queue <- Queue.bounded[GameRunner.GameCommand](eventLoopQueueCapacity)
          _     <- queue.offer(GameRunner.GameCommand.Quit)
          loopState = GameRunner.LoopState(initialState, None)
          intervalRef <- Ref.make(LineClearing.dropInterval(initialState.level, LocalTestServices.testConfig.speed))
          result      <- GameRunner.eventLoop(queue, loopState, LocalTestServices.testConfig, intervalRef)
        yield assertTrue(result.gameState == initialState)
      }.provide(LocalTestServices.console),
      test("UserAction updates state and continues") {
        for
          queue <- Queue.bounded[GameRunner.GameCommand](eventLoopQueueCapacity)
          _     <- queue.offer(GameRunner.GameCommand.UserAction(Input.MoveLeft))
          _     <- queue.offer(GameRunner.GameCommand.Quit)
          loopState = GameRunner.LoopState(initialState, None)
          intervalRef <- Ref.make(LineClearing.dropInterval(initialState.level, LocalTestServices.testConfig.speed))
          result      <- GameRunner.eventLoop(queue, loopState, LocalTestServices.testConfig, intervalRef)
        yield assertTrue(
          result.gameState != null
        )
      }.provide(LocalTestServices.console),
      test("TimeTick updates state and continues") {
        for
          queue <- Queue.bounded[GameRunner.GameCommand](eventLoopQueueCapacity)
          _     <- queue.offer(GameRunner.GameCommand.TimeTick)
          _     <- queue.offer(GameRunner.GameCommand.Quit)
          loopState = GameRunner.LoopState(initialState, None)
          intervalRef <- Ref.make(LineClearing.dropInterval(initialState.level, LocalTestServices.testConfig.speed))
          result      <- GameRunner.eventLoop(queue, loopState, LocalTestServices.testConfig, intervalRef)
        yield assertTrue(result.gameState != null)
      }.provide(LocalTestServices.console),
      test("UserAction game over exits loop") {
        val gameOverState = initialState.copy(status = GameStatus.GameOver)
        for
          queue <- Queue.bounded[GameRunner.GameCommand](eventLoopQueueCapacity)
          _     <- queue.offer(GameRunner.GameCommand.UserAction(Input.MoveDown))
          loopState = GameRunner.LoopState(gameOverState, None)
          intervalRef <- Ref.make(LineClearing.dropInterval(gameOverState.level, LocalTestServices.testConfig.speed))
          result      <- GameRunner.eventLoop(queue, loopState, LocalTestServices.testConfig, intervalRef)
        yield assertTrue(result.gameState.isGameOver)
      }.provide(LocalTestServices.console),
      test("TimeTick game over exits loop") {
        val gameOverState = initialState.copy(status = GameStatus.GameOver)
        for
          queue <- Queue.bounded[GameRunner.GameCommand](eventLoopQueueCapacity)
          _     <- queue.offer(GameRunner.GameCommand.TimeTick)
          loopState = GameRunner.LoopState(gameOverState, None)
          intervalRef <- Ref.make(LineClearing.dropInterval(gameOverState.level, LocalTestServices.testConfig.speed))
          result      <- GameRunner.eventLoop(queue, loopState, LocalTestServices.testConfig, intervalRef)
        yield assertTrue(result.gameState.isGameOver)
      }.provide(LocalTestServices.console),
      test("multiple commands processed in order") {
        for
          queue <- Queue.bounded[GameRunner.GameCommand](eventLoopQueueCapacity)
          _     <- queue.offer(GameRunner.GameCommand.UserAction(Input.MoveLeft))
          _     <- queue.offer(GameRunner.GameCommand.UserAction(Input.MoveRight))
          _     <- queue.offer(GameRunner.GameCommand.TimeTick)
          _     <- queue.offer(GameRunner.GameCommand.Quit)
          loopState = GameRunner.LoopState(initialState, None)
          intervalRef <- Ref.make(LineClearing.dropInterval(initialState.level, LocalTestServices.testConfig.speed))
          result      <- GameRunner.eventLoop(queue, loopState, LocalTestServices.testConfig, intervalRef)
        yield assertTrue(result.previousBuffer.isDefined)
      }.provide(LocalTestServices.console),
      test("game over transition exits on TimeTick") {
        val topFilledGrid = (1 until gridWidth).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, x) =>
          (0 to 1).foldLeft(g) { (g2, y) =>
            g2.place(Position(x, y), Cell.Filled(TetrominoShape.I))
          }
        }
        val tetromino = Tetromino
          .spawn(TetrominoShape.O, gridWidth)
          .copy(position = Position(tetrominoLockX, gridHeight - 2))
        val nearGameOverState = GameState(
          grid = topFilledGrid,
          currentTetromino = tetromino,
          nextTetromino = TetrominoShape.I,
          score = 0,
          level = 1,
          linesCleared = 0,
          status = GameStatus.Playing
        )
        for
          queue <- Queue.bounded[GameRunner.GameCommand](eventLoopQueueCapacity)
          _     <- queue.offer(GameRunner.GameCommand.TimeTick)
          _     <- queue.offer(GameRunner.GameCommand.Quit)
          loopState = GameRunner.LoopState(nearGameOverState, None)
          intervalRef <- Ref.make(
            LineClearing.dropInterval(nearGameOverState.level, LocalTestServices.testConfig.speed)
          )
          result <- GameRunner
            .eventLoop(queue, loopState, LocalTestServices.testConfig, intervalRef)
            .timeout(testTimeoutDuration)
        yield assertTrue(result.exists(_.gameState.isGameOver))
      }.provide(LocalTestServices.console),
      test("game over transition exits on UserAction HardDrop") {
        val topFilledGrid = (1 until gridWidth).foldLeft(Grid.empty(gridWidth, gridHeight)) { (g, x) =>
          (0 to 1).foldLeft(g) { (g2, y) =>
            g2.place(Position(x, y), Cell.Filled(TetrominoShape.I))
          }
        }
        val tetromino = Tetromino
          .spawn(TetrominoShape.O, gridWidth)
          .copy(position = Position(tetrominoLockX, tetrominoStartBelowFilledRow))
        val nearGameOverState = GameState(
          grid = topFilledGrid,
          currentTetromino = tetromino,
          nextTetromino = TetrominoShape.I,
          score = 0,
          level = 1,
          linesCleared = 0,
          status = GameStatus.Playing
        )
        for
          queue <- Queue.bounded[GameRunner.GameCommand](eventLoopQueueCapacity)
          _     <- queue.offer(GameRunner.GameCommand.UserAction(Input.HardDrop))
          _     <- queue.offer(GameRunner.GameCommand.Quit)
          loopState = GameRunner.LoopState(nearGameOverState, None)
          intervalRef <- Ref.make(
            LineClearing.dropInterval(nearGameOverState.level, LocalTestServices.testConfig.speed)
          )
          result <- GameRunner
            .eventLoop(queue, loopState, LocalTestServices.testConfig, intervalRef)
            .timeout(testTimeoutDuration)
        yield assertTrue(result.exists(_.gameState.isGameOver))
      }.provide(LocalTestServices.console),
      test("updates drop interval when level changes") {
        val highLevelState   = initialState.copy(level = highLevelForTest)
        val level1Interval   = LineClearing.dropInterval(1, LocalTestServices.testConfig.speed)
        val expectedInterval = LineClearing.dropInterval(highLevelForTest, LocalTestServices.testConfig.speed)
        for
          queue <- Queue.bounded[GameRunner.GameCommand](eventLoopQueueCapacity)
          _     <- queue.offer(GameRunner.GameCommand.TimeTick)
          _     <- queue.offer(GameRunner.GameCommand.Quit)
          loopState = GameRunner.LoopState(highLevelState, None)
          intervalRef     <- Ref.make(level1Interval)
          _               <- GameRunner.eventLoop(queue, loopState, LocalTestServices.testConfig, intervalRef)
          updatedInterval <- intervalRef.get
        yield assertTrue(
          updatedInterval == expectedInterval,
          updatedInterval < level1Interval
        )
      }.provide(LocalTestServices.console)
    )
  )

  private val minimumBufferDimension = 0
  private val eventLoopQueueCapacity = 10

  private val gameOverTopRowCount          = 4
  private val tetrominoLockX               = 4
  private val tetrominoBottomOffset        = 2
  private val tetrominoStartBelowFilledRow = 4
  private val ticksToTriggerGameOver       = 5

  private val highLevelForTest    = 5
  private val testTimeoutDuration = Duration.fromSeconds(5)

  private object Ansi:
    val clearScreen = "\u001b[2J"

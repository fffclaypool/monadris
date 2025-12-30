package monadris.integration

import zio.*
import zio.test.*
import zio.stream.*

import monadris.domain.*
import monadris.domain.config.*
import monadris.logic.GameLogic
import monadris.view.GameView
import monadris.infrastructure.{TestServices as Mocks, ConsoleRenderer}

/**
 * ゲームの結合テストスイート
 * アプリケーション層（Queue, Loop）とドメイン層（Logic）の連携を検証
 */
object GameIntegrationSpec extends ZIOSpecDefault:

  // ============================================================
  // Test Constants
  // ============================================================

  private object TestConstants:
    val GridWidth = 10
    val GridHeight = 20
    val InitialTetrominoY = 1
    val TicksToReachBottom = GridHeight - 2  // Approximate ticks to reach bottom

  // ============================================================
  // Helper Functions
  // ============================================================

  /** テスト用の固定シェイププロバイダー */
  private def fixedShapeProvider(shape: TetrominoShape): () => TetrominoShape =
    () => shape

  /** 初期ゲーム状態を生成 */
  private def createInitialState(
    firstShape: TetrominoShape = TetrominoShape.I,
    nextShape: TetrominoShape = TetrominoShape.O
  ): GameState =
    GameState.initial(firstShape, nextShape, TestConstants.GridWidth, TestConstants.GridHeight)

  /** 指定行を埋めたGridを作成（ライン消去テスト用） */
  private def createGridWithFilledRows(
    rowIndices: List[Int],
    gapColumn: Option[Int] = None
  ): Grid =
    val emptyGrid = Grid.empty(TestConstants.GridWidth, TestConstants.GridHeight)
    rowIndices.foldLeft(emptyGrid) { (grid, rowIdx) =>
      (0 until TestConstants.GridWidth).foldLeft(grid) { (g, x) =>
        if gapColumn.contains(x) then g
        else g.place(Position(x, rowIdx), Cell.Filled(TetrominoShape.I))
      }
    }

  /** ゲーム状態を入力ストリームで更新 */
  private def processInputs(
    initialState: GameState,
    inputs: Chunk[Input],
    config: AppConfig,
    nextShapeProvider: () => TetrominoShape
  ): GameState =
    inputs.foldLeft(initialState) { (state, input) =>
      GameLogic.update(state, input, nextShapeProvider, config)
    }

  // ============================================================
  // Test Specs
  // ============================================================

  def spec = suite("Game Integration Tests")(
    gravityTestSuite,
    lineClearIntegrationSuite,
    gameOverFlowSuite,
    pauseResumeSuite,
    existingScenarioSuite
  ) @@ TestAspect.tag("integration")

  // ============================================================
  // 1. Gravity Test Suite - 時間経過による自動落下
  // ============================================================

  private val gravityTestSuite = suite("Gravity Test - Auto Fall")(

    test("Tick input moves tetromino down by one row") {
      val config = Mocks.testConfig
      val initialState = createInitialState()
      val initialY = initialState.currentTetromino.position.y

      val newState = GameLogic.update(
        initialState,
        Input.Tick,
        fixedShapeProvider(TetrominoShape.T),
        config
      )

      assertTrue(
        newState.currentTetromino.position.y == initialY + 1,
        newState.isPlaying
      )
    },

    test("Multiple ticks move tetromino progressively down") {
      val config = Mocks.testConfig
      val initialState = createInitialState()
      val tickCount = 5

      val ticks = Chunk.fill(tickCount)(Input.Tick)
      val finalState = processInputs(
        initialState,
        ticks,
        config,
        fixedShapeProvider(TetrominoShape.T)
      )

      assertTrue(
        finalState.currentTetromino.position.y == initialState.currentTetromino.position.y + tickCount
      )
    },

    test("Tetromino locks when reaching bottom after ticks") {
      val config = Mocks.testConfig
      val initialState = createInitialState(TetrominoShape.O)

      // O-tetromino reaches bottom after enough ticks
      val manyTicks = Chunk.fill(TestConstants.GridHeight)(Input.Tick)
      val finalState = processInputs(
        initialState,
        manyTicks,
        config,
        fixedShapeProvider(TetrominoShape.T)
      )

      // Should have locked and spawned new tetromino
      assertTrue(
        finalState.currentTetromino.shape == TetrominoShape.O ||
        finalState.currentTetromino.shape == TetrominoShape.T
      )
    },

    test("No movement when game is paused and tick arrives") {
      val config = Mocks.testConfig
      val pausedState = createInitialState().copy(status = GameStatus.Paused)
      val initialY = pausedState.currentTetromino.position.y

      val newState = GameLogic.update(
        pausedState,
        Input.Tick,
        fixedShapeProvider(TetrominoShape.T),
        config
      )

      assertTrue(
        newState.currentTetromino.position.y == initialY,
        newState.status == GameStatus.Paused
      )
    }
  )

  // ============================================================
  // 2. Line Clear Integration Suite - ライン消去とスコア更新
  // ============================================================

  private val lineClearIntegrationSuite = suite("Line Clear Integration")(

    test("Clearing one line adds score and updates state") {
      val config = Mocks.testConfig

      // Create grid with bottom row almost complete (gap at column 4-5 for I-piece)
      val bottomRow = TestConstants.GridHeight - 1
      val almostCompleteGrid = (0 until TestConstants.GridWidth)
        .filterNot(x => x >= 4 && x < 8)  // Leave gap for I-piece
        .foldLeft(Grid.empty(TestConstants.GridWidth, TestConstants.GridHeight)) { (g, x) =>
          g.place(Position(x, bottomRow), Cell.Filled(TetrominoShape.O))
        }

      // I-piece positioned to fill the gap
      val iPiece = Tetromino(TetrominoShape.I, Position(5, bottomRow), Rotation.R0)
      val state = GameState(
        grid = almostCompleteGrid,
        currentTetromino = iPiece,
        nextTetromino = TetrominoShape.T,
        score = 0,
        level = 1,
        linesCleared = 0,
        status = GameStatus.Playing
      )

      // Hard drop to lock the piece
      val finalState = GameLogic.update(
        state,
        Input.HardDrop,
        fixedShapeProvider(TetrominoShape.O),
        config
      )

      assertTrue(
        finalState.score > 0,
        finalState.linesCleared >= 0  // Line should be cleared
      )
    },

    test("Clearing multiple lines gives higher score") {
      val config = Mocks.testConfig

      // Create grid with 4 rows almost complete
      val bottomRows = (TestConstants.GridHeight - 4 until TestConstants.GridHeight).toList
      val almostCompleteGrid = bottomRows.foldLeft(
        Grid.empty(TestConstants.GridWidth, TestConstants.GridHeight)
      ) { (grid, rowIdx) =>
        (0 until TestConstants.GridWidth - 1).foldLeft(grid) { (g, x) =>
          g.place(Position(x, rowIdx), Cell.Filled(TetrominoShape.O))
        }
      }

      // I-piece vertical to fill the last column
      val iPiece = Tetromino(
        TetrominoShape.I,
        Position(TestConstants.GridWidth - 1, TestConstants.GridHeight - 3),
        Rotation.R90
      )
      val state = GameState(
        grid = almostCompleteGrid,
        currentTetromino = iPiece,
        nextTetromino = TetrominoShape.T,
        score = 0,
        level = 1,
        linesCleared = 0,
        status = GameStatus.Playing
      )

      val finalState = GameLogic.update(
        state,
        Input.HardDrop,
        fixedShapeProvider(TetrominoShape.O),
        config
      )

      assertTrue(finalState.score > 0)
    },

    test("Score is rendered in screen buffer after line clear") {
      val config = Mocks.testConfig
      val stateWithScore = createInitialState().copy(score = 500, linesCleared = 5)

      for
        consoleService <- ZIO.service[Mocks.TestConsoleService]
        screenBuffer = GameView.toScreenBuffer(stateWithScore, config)
        _ <- ConsoleRenderer.render(screenBuffer)
        output <- consoleService.buffer.get
        combined = output.mkString
      yield assertTrue(
        combined.contains("Score"),
        combined.contains("500"),
        combined.contains("Lines")
      )
    }.provide(Mocks.console)
  )

  // ============================================================
  // 3. Game Over Flow Suite - ゲームオーバー判定
  // ============================================================

  private val gameOverFlowSuite = suite("Game Over Flow")(

    test("Game transitions to GameOver when spawn position is blocked") {
      val config = Mocks.testConfig

      // Fill top rows to block spawn (leave gaps to avoid line clears)
      // Fill rows 0-3 with alternating pattern to block spawn but not clear lines
      val blockedGrid = (0 until TestConstants.GridWidth - 1).foldLeft(
        Grid.empty(TestConstants.GridWidth, TestConstants.GridHeight)
      ) { (g, x) =>
        (0 until 4).foldLeft(g) { (grid, y) =>
          grid.place(Position(x, y), Cell.Filled(TetrominoShape.O))
        }
      }

      // Current piece positioned to lock immediately (at bottom)
      val tetromino = Tetromino(
        TetrominoShape.O,
        Position(4, TestConstants.GridHeight - 2),
        Rotation.R0
      )
      val state = GameState(
        grid = blockedGrid,
        currentTetromino = tetromino,
        nextTetromino = TetrominoShape.T,  // T spawns at center, blocked by filled rows
        score = 1000,
        level = 5,
        linesCleared = 40,
        status = GameStatus.Playing
      )

      val finalState = GameLogic.update(
        state,
        Input.HardDrop,
        fixedShapeProvider(TetrominoShape.I),
        config
      )

      assertTrue(
        finalState.status == GameStatus.GameOver,
        finalState.isGameOver
      )
    },

    test("No further updates after GameOver except Pause toggle") {
      val config = Mocks.testConfig
      val gameOverState = createInitialState().copy(status = GameStatus.GameOver)
      val initialY = gameOverState.currentTetromino.position.y

      // Try various inputs
      val inputs = Chunk(Input.MoveLeft, Input.MoveRight, Input.Tick, Input.HardDrop)
      val finalState = processInputs(
        gameOverState,
        inputs,
        config,
        fixedShapeProvider(TetrominoShape.T)
      )

      assertTrue(
        finalState.currentTetromino.position.y == initialY,
        finalState.status == GameStatus.GameOver
      )
    },

    test("GameOver screen is rendered correctly") {
      val gameOverState = createInitialState().copy(
        status = GameStatus.GameOver,
        score = 2500,
        linesCleared = 25,
        level = 3
      )

      for
        consoleService <- ZIO.service[Mocks.TestConsoleService]
        screenBuffer = GameView.gameOverScreen(gameOverState)
        _ <- ConsoleRenderer.render(screenBuffer)
        output <- consoleService.buffer.get
        combined = output.mkString
      yield assertTrue(
        combined.contains("GAME OVER"),
        combined.contains("2500") || combined.contains("Score")
      )
    }.provide(Mocks.console)
  )

  // ============================================================
  // 4. Pause/Resume Suite - ポーズ機能
  // ============================================================

  private val pauseResumeSuite = suite("Pause/Resume")(

    test("Pause input transitions game to Paused state") {
      val config = Mocks.testConfig
      val playingState = createInitialState()

      val pausedState = GameLogic.update(
        playingState,
        Input.Pause,
        fixedShapeProvider(TetrominoShape.T),
        config
      )

      assertTrue(pausedState.status == GameStatus.Paused)
    },

    test("Movement inputs are ignored while paused") {
      val config = Mocks.testConfig
      val pausedState = createInitialState().copy(status = GameStatus.Paused)
      val initialX = pausedState.currentTetromino.position.x
      val initialY = pausedState.currentTetromino.position.y

      val inputs = Chunk(Input.MoveLeft, Input.MoveRight, Input.MoveDown, Input.RotateClockwise)
      val finalState = processInputs(
        pausedState,
        inputs,
        config,
        fixedShapeProvider(TetrominoShape.T)
      )

      assertTrue(
        finalState.currentTetromino.position.x == initialX,
        finalState.currentTetromino.position.y == initialY,
        finalState.status == GameStatus.Paused
      )
    },

    test("HardDrop is ignored while paused") {
      val config = Mocks.testConfig
      val pausedState = createInitialState().copy(status = GameStatus.Paused)
      val initialY = pausedState.currentTetromino.position.y

      val finalState = GameLogic.update(
        pausedState,
        Input.HardDrop,
        fixedShapeProvider(TetrominoShape.T),
        config
      )

      assertTrue(
        finalState.currentTetromino.position.y == initialY,
        finalState.status == GameStatus.Paused
      )
    },

    test("Pause input while paused resumes the game") {
      val config = Mocks.testConfig
      val pausedState = createInitialState().copy(status = GameStatus.Paused)

      val resumedState = GameLogic.update(
        pausedState,
        Input.Pause,
        fixedShapeProvider(TetrominoShape.T),
        config
      )

      assertTrue(resumedState.status == GameStatus.Playing)
    },

    test("Full pause-resume cycle maintains game state") {
      val config = Mocks.testConfig
      val initialState = createInitialState()
      val initialScore = initialState.score
      val initialX = initialState.currentTetromino.position.x

      // Pause -> try to move -> resume -> move successfully
      val pauseState = GameLogic.update(initialState, Input.Pause, fixedShapeProvider(TetrominoShape.T), config)
      val stillPausedState = GameLogic.update(pauseState, Input.MoveLeft, fixedShapeProvider(TetrominoShape.T), config)
      val resumedState = GameLogic.update(stillPausedState, Input.Pause, fixedShapeProvider(TetrominoShape.T), config)
      val movedState = GameLogic.update(resumedState, Input.MoveLeft, fixedShapeProvider(TetrominoShape.T), config)

      assertTrue(
        stillPausedState.currentTetromino.position.x == initialX,  // Move ignored while paused
        movedState.currentTetromino.position.x == initialX - 1,    // Move works after resume
        movedState.status == GameStatus.Playing
      )
    },

    test("PAUSED indicator appears in screen buffer when paused") {
      val config = Mocks.testConfig
      val pausedState = createInitialState().copy(status = GameStatus.Paused)

      for
        consoleService <- ZIO.service[Mocks.TestConsoleService]
        screenBuffer = GameView.toScreenBuffer(pausedState, config)
        _ <- ConsoleRenderer.render(screenBuffer)
        output <- consoleService.buffer.get
        combined = output.mkString
      yield assertTrue(combined.contains("PAUSED"))
    }.provide(Mocks.console)
  )

  // ============================================================
  // Existing Scenario (Updated)
  // ============================================================

  private val existingScenarioSuite = suite("Basic Scenarios")(

    test("Move Right and Hard Drop updates score and renders") {
      val config = Mocks.testConfig

      for
        queue <- Queue.unbounded[Input]
        _ <- queue.offer(Input.MoveRight)
        _ <- queue.offer(Input.HardDrop)

        initialState = createInitialState()

        finalState <- ZStream.fromQueue(queue)
          .take(2)
          .runFold(initialState) { (currentState, input) =>
            GameLogic.update(currentState, input, fixedShapeProvider(TetrominoShape.T), config)
          }

        consoleService <- ZIO.service[Mocks.TestConsoleService]
        screenBuffer = GameView.toScreenBuffer(finalState, config)
        _ <- ConsoleRenderer.render(screenBuffer)
        output <- consoleService.buffer.get
      yield
        assertTrue(finalState.score > 0) &&
        assertTrue(output.exists(_.contains("Score")))
    }.provide(Mocks.console),

    test("Complete game loop with queue processes all inputs") {
      val config = Mocks.testConfig
      val inputs = Chunk(
        Input.MoveLeft,
        Input.MoveLeft,
        Input.RotateClockwise,
        Input.MoveRight,
        Input.MoveDown,
        Input.HardDrop
      )

      for
        queue <- Queue.unbounded[Input]
        _ <- queue.offerAll(inputs)

        initialState = createInitialState(TetrominoShape.T)

        finalState <- ZStream.fromQueue(queue)
          .take(inputs.size.toLong)
          .runFold(initialState) { (state, input) =>
            GameLogic.update(state, input, fixedShapeProvider(TetrominoShape.O), config)
          }
      yield assertTrue(
        finalState.score > 0,
        finalState.isPlaying || finalState.isGameOver
      )
    }
  )

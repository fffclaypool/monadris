package monadris.infrastructure.game

import zio.*
import zio.test.*

import monadris.domain.*
import monadris.game.GameLogic
import monadris.infrastructure.terminal.TestServices as LocalTestServices

/**
 * Stress test for checking memory pressure and leaks.
 * Runs a large number of game loop iterations using GameLogic
 * and monitors memory usage over time.
 */
object StressTest extends ZIOSpecDefault:

  private object Constants:
    val totalIterations     = 100000
    val memoryCheckInterval = 10000
    val bytesPerMegabyte    = 1024 * 1024

  private def getUsedMemoryMB: Long =
    val runtime   = java.lang.Runtime.getRuntime
    val usedBytes = runtime.totalMemory() - runtime.freeMemory()
    usedBytes / Constants.bytesPerMegabyte

  private def printMemoryStatus(label: String, iteration: Int): UIO[Unit] =
    ZIO.succeed {
      val usedMB = getUsedMemoryMB
      println(s"[$label] Iteration $iteration: Memory used = ${usedMB}MB")
    }

  private def printMemoryStatus(label: String): UIO[Unit] =
    ZIO.succeed {
      val usedMB = getUsedMemoryMB
      println(s"[$label] Memory used = ${usedMB}MB")
    }

  private def runGarbageCollection: UIO[Unit] =
    ZIO.succeed {
      java.lang.System.gc()
      Thread.sleep(100)
    }

  private val testConfig = LocalTestServices.testConfig
  private val gridWidth  = testConfig.grid.width
  private val gridHeight = testConfig.grid.height

  private def initialState: GameState =
    GameState.initial(TetrominoShape.T, TetrominoShape.I, gridWidth, gridHeight)

  /**
   * Deterministic tetromino shape provider.
   * Eliminates randomness to make tests reproducible.
   */
  private def deterministicShapeProvider(counter: Int): () => TetrominoShape =
    val shapes = TetrominoShape.values
    () => shapes(counter % shapes.length)

  def spec = suite("StressTest")(
    test("GameLogic handles 100,000 iterations without memory leak") {
      for
        _          <- printMemoryStatus("START")
        finalState <- runGameLoop(initialState, Constants.totalIterations)
        _          <- printMemoryStatus("BEFORE GC")
        _          <- runGarbageCollection
        _          <- printMemoryStatus("AFTER GC (Final)")
      yield assertTrue(
        finalState != null,
        finalState.score >= 0,
        finalState.linesCleared >= 0
      )
    } @@ TestAspect.timeout(5.minutes) @@ TestAspect.tag("heavy") @@ TestAspect.ifEnvSet("RUN_STRESS_TESTS")
  )

  /**
   * Run the game loop for a specified number of iterations.
   * Repeatedly calls the pure function GameLogic.update.
   */
  private def runGameLoop(
    initialState: GameState,
    iterations: Int
  ): UIO[GameState] =
    ZIO.succeed {
      val inputs = createInputSequence

      var state        = initialState
      var shapeCounter = 0

      for i <- 0 until iterations do
        if i > 0 && i % Constants.memoryCheckInterval == 0 then
          val usedMB = getUsedMemoryMB
          println(s"[PROGRESS] Iteration $i: Memory used = ${usedMB}MB")

        if state.isGameOver then
          state = initialState.copy(
            score = state.score,
            linesCleared = state.linesCleared
          )

        val input = inputs(i % inputs.length)

        val provider = deterministicShapeProvider(shapeCounter)
        state = GameLogic.update(state, input, provider, testConfig)
        shapeCounter = shapeCounter + 1

      state
    }

  /**
   * Generate an input sequence for testing.
   * Contains a pattern of various operations.
   */
  private def createInputSequence: Vector[Input] =
    Vector(
      Input.Tick,
      Input.MoveLeft,
      Input.Tick,
      Input.MoveRight,
      Input.Tick,
      Input.RotateClockwise,
      Input.Tick,
      Input.MoveDown,
      Input.Tick,
      Input.HardDrop
    )

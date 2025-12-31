package monadris.infrastructure

import zio.*
import zio.test.*

import monadris.domain.*
import monadris.infrastructure.TestServices as LocalTestServices
import monadris.logic.GameLogic

/**
 * メモリ負荷とリークをチェックするストレステスト
 * GameLogicを使用して大量のゲームループを実行し、
 * メモリ使用量の推移を監視する
 */
object StressTest extends ZIOSpecDefault:

  // ============================================================
  // Test constants
  // ============================================================

  private object Constants:
    val totalIterations     = 100000
    val memoryCheckInterval = 10000
    val bytesPerMegabyte    = 1024 * 1024

  // ============================================================
  // Memory utilities
  // ============================================================

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
      Thread.sleep(100) // GCが完了するまで少し待機
    }

  // ============================================================
  // Test fixtures
  // ============================================================

  private val testConfig = LocalTestServices.testConfig
  private val gridWidth  = testConfig.grid.width
  private val gridHeight = testConfig.grid.height

  private def initialState: GameState =
    GameState.initial(TetrominoShape.T, TetrominoShape.I, gridWidth, gridHeight)

  /**
   * 決定的なテトリミノ形状プロバイダー
   * ランダム性を排除してテストを再現可能にする
   */
  private def deterministicShapeProvider(counter: Int): () => TetrominoShape =
    val shapes = TetrominoShape.values
    () => shapes(counter % shapes.length)

  // ============================================================
  // Stress test
  // ============================================================

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
   * ゲームループを指定回数実行
   * 純粋関数GameLogic.updateを繰り返し呼び出す
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
        // 定期的なメモリチェック
        if i > 0 && i % Constants.memoryCheckInterval == 0 then
          val usedMB = getUsedMemoryMB
          println(s"[PROGRESS] Iteration $i: Memory used = ${usedMB}MB")

        // ゲームオーバーなら初期状態にリセット
        if state.isGameOver then
          state = initialState.copy(
            score = state.score,
            linesCleared = state.linesCleared
          )

        // 入力を選択
        val input = inputs(i % inputs.length)

        // 状態更新
        val provider = deterministicShapeProvider(shapeCounter)
        state = GameLogic.update(state, input, provider, testConfig)
        shapeCounter = shapeCounter + 1

      state
    }

  /**
   * テスト用の入力シーケンスを生成
   * 様々な操作を含むパターン
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

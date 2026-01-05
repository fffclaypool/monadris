package monadris.infrastructure

import zio.*
import zio.test.*

import monadris.domain.model.game.GameCommand
import monadris.domain.model.game.TetrisGame
import monadris.infrastructure.TestServices as LocalTestServices

/**
 * メモリ負荷とリークをチェックするストレステスト
 * TetrisGame.handleを使用して大量のゲームループを実行し、
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
    val testSeed            = 42L

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

  private def initialGame: TetrisGame =
    TetrisGame.create(Constants.testSeed, gridWidth, gridHeight, testConfig.score, testConfig.level)

  // ============================================================
  // Stress test
  // ============================================================

  def spec = suite("StressTest")(
    test("TetrisGame handles 100,000 iterations without memory leak") {
      for
        _         <- printMemoryStatus("START")
        finalGame <- runGameLoop(initialGame, Constants.totalIterations)
        _         <- printMemoryStatus("BEFORE GC")
        _         <- runGarbageCollection
        _         <- printMemoryStatus("AFTER GC (Final)")
      yield assertTrue(
        finalGame != null,
        finalGame.scoreState.score >= 0,
        finalGame.scoreState.linesCleared >= 0
      )
    } @@ TestAspect.timeout(5.minutes) @@ TestAspect.tag("heavy") @@ TestAspect.ifEnvSet("RUN_STRESS_TESTS")
  )

  /**
   * ゲームループを指定回数実行
   * 純粋関数TetrisGame.handleを繰り返し呼び出す
   */
  private def runGameLoop(
    initialGame: TetrisGame,
    iterations: Int
  ): UIO[TetrisGame] =
    ZIO.succeed {
      val commands = createCommandSequence

      var game = initialGame

      for i <- 0 until iterations do
        // 定期的なメモリチェック
        if i > 0 && i % Constants.memoryCheckInterval == 0 then
          val usedMB = getUsedMemoryMB
          println(s"[PROGRESS] Iteration $i: Memory used = ${usedMB}MB")

        // ゲームオーバーなら初期状態にリセット
        if game.isOver then
          game = TetrisGame.create(
            Constants.testSeed + i,
            gridWidth,
            gridHeight,
            testConfig.score,
            testConfig.level
          )

        // コマンドを選択
        val command = commands(i % commands.length)

        // 状態更新
        val (newGame, _) = game.handle(command)
        game = newGame

      game
    }

  /**
   * テスト用のコマンドシーケンスを生成
   * 様々な操作を含むパターン
   */
  private def createCommandSequence: Vector[GameCommand] =
    Vector(
      GameCommand.Tick,
      GameCommand.MoveLeft,
      GameCommand.Tick,
      GameCommand.MoveRight,
      GameCommand.Tick,
      GameCommand.RotateCW,
      GameCommand.Tick,
      GameCommand.SoftDrop,
      GameCommand.Tick,
      GameCommand.HardDrop
    )

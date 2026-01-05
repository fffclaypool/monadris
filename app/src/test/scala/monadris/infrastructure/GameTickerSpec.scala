package monadris.infrastructure

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import monadris.domain.model.game.GameCommand

/**
 * GameTickerのユニットテスト
 */
object GameTickerSpec extends ZIOSpecDefault:

  private object TestInterval:
    val fast: Long = 50
    val slow: Long = 200

  def spec = suite("GameTicker")(
    test("produces Tick commands to queue") {
      for
        queue       <- Queue.unbounded[GameCommand]
        intervalRef <- Ref.make(TestInterval.fast)
        fiber       <- GameTicker.start(queue, intervalRef)
        // Wait for at least one tick
        _    <- ZIO.sleep(TestInterval.fast.millis * 2)
        _    <- fiber.interrupt
        size <- queue.size
      yield assertTrue(size >= 1)
    },
    test("adjusts interval dynamically") {
      for
        queue       <- Queue.unbounded[GameCommand]
        intervalRef <- Ref.make(TestInterval.slow)
        fiber       <- GameTicker.start(queue, intervalRef)
        // Wait for one slow tick
        _     <- ZIO.sleep(TestInterval.slow.millis + 50.millis)
        size1 <- queue.size
        // Speed up the interval
        _ <- intervalRef.set(TestInterval.fast)
        // Clear the queue
        _ <- queue.takeAll
        // Wait for fast ticks
        _     <- ZIO.sleep(TestInterval.fast.millis * 3)
        _     <- fiber.interrupt
        size2 <- queue.size
      yield assertTrue(size1 >= 1, size2 >= 1)
    },
    test("tick command is GameCommand.Tick") {
      for
        queue       <- Queue.unbounded[GameCommand]
        intervalRef <- Ref.make(TestInterval.fast)
        fiber       <- GameTicker.start(queue, intervalRef)
        _           <- ZIO.sleep(TestInterval.fast.millis * 2)
        _           <- fiber.interrupt
        cmd         <- queue.take
      yield assertTrue(cmd == GameCommand.Tick)
    },
    test("multiple ticks are produced over time") {
      for
        queue       <- Queue.unbounded[GameCommand]
        intervalRef <- Ref.make(TestInterval.fast)
        fiber       <- GameTicker.start(queue, intervalRef)
        // Wait for multiple ticks
        _    <- ZIO.sleep(TestInterval.fast.millis * 5)
        _    <- fiber.interrupt
        size <- queue.size
      yield assertTrue(size >= 3)
    }
  ) @@ withLiveClock @@ timeout(10.seconds)

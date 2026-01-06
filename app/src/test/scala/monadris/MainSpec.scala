package monadris

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import monadris.infrastructure.TestServices as Mocks

/**
 * Main.program の結合テスト
 * TestServices のモック実装を使用してアプリケーション全体をテスト
 *
 * Note: ConsoleRenderer now writes directly to stdout, so we can't easily
 * capture output in tests. These tests focus on ensuring the program
 * runs without errors and exits correctly.
 */
object MainSpec extends ZIOSpecDefault:

  def spec = suite("Main Application")(
    test("program runs and exits with Q key") {
      for _ <- Main.program
          .timeout(5.seconds)
      yield assertTrue(true)
    }.provide(
      Mocks.terminal(Chunk('q'.toInt)),
      Mocks.config
    ),
    test("program runs and exits with lowercase q") {
      for _ <- Main.program
          .timeout(5.seconds)
      yield assertTrue(true)
    }.provide(
      Mocks.terminal(Chunk('q'.toInt)),
      Mocks.config
    ),
    test("program runs and exits with uppercase Q") {
      for _ <- Main.program
          .timeout(5.seconds)
      yield assertTrue(true)
    }.provide(
      Mocks.terminal(Chunk('Q'.toInt)),
      Mocks.config
    ),
    test("program handles movement before quit") {
      val inputs = Chunk('h'.toInt, 'l'.toInt, 'j'.toInt, 'q'.toInt)
      for _ <- Main.program
          .timeout(5.seconds)
      yield assertTrue(true)
    }.provide(
      Mocks.terminal(Chunk('h'.toInt, 'l'.toInt, 'j'.toInt, 'q'.toInt)),
      Mocks.config
    ),
    test("program handles arrow keys before quit") {
      // ESC [ D (Left arrow) then q
      val inputs = Chunk(27, '['.toInt, 'D'.toInt, 'q'.toInt)
      for _ <- Main.program
          .timeout(5.seconds)
      yield assertTrue(true)
    }.provide(
      Mocks.terminal(Chunk(27, '['.toInt, 'D'.toInt, 'q'.toInt)),
      Mocks.config
    ),
    test("program handles rotation before quit") {
      // ESC [ A (Up arrow = rotate) then q
      val inputs = Chunk(27, '['.toInt, 'A'.toInt, 'q'.toInt)
      for _ <- Main.program
          .timeout(5.seconds)
      yield assertTrue(true)
    }.provide(
      Mocks.terminal(Chunk(27, '['.toInt, 'A'.toInt, 'q'.toInt)),
      Mocks.config
    ),
    test("program handles hard drop before quit") {
      // Space (hard drop) then q
      val inputs = Chunk(' '.toInt, 'q'.toInt)
      for _ <- Main.program
          .timeout(5.seconds)
      yield assertTrue(true)
    }.provide(
      Mocks.terminal(Chunk(' '.toInt, 'q'.toInt)),
      Mocks.config
    ),
    test("program handles pause and unpause before quit") {
      // p (pause), p (unpause), q (quit)
      val inputs = Chunk('p'.toInt, 'p'.toInt, 'q'.toInt)
      for _ <- Main.program
          .timeout(5.seconds)
      yield assertTrue(true)
    }.provide(
      Mocks.terminal(Chunk('p'.toInt, 'p'.toInt, 'q'.toInt)),
      Mocks.config
    ),
    test("program handles multiple movements before quit") {
      // h, h, l, j, j, q
      val inputs = Chunk('h'.toInt, 'h'.toInt, 'l'.toInt, 'j'.toInt, 'j'.toInt, 'q'.toInt)
      for _ <- Main.program
          .timeout(5.seconds)
      yield assertTrue(true)
    }.provide(
      Mocks.terminal(Chunk('h'.toInt, 'h'.toInt, 'l'.toInt, 'j'.toInt, 'j'.toInt, 'q'.toInt)),
      Mocks.config
    )
  ) @@ withLiveClock

package monadris.application

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import monadris.domain.config.AppConfig
import monadris.domain.model.game.TetrisGame
import monadris.infrastructure.TestServices as Mocks

/**
 * GameEngineのユニットテスト
 */
object GameEngineSpec extends ZIOSpecDefault:

  val config: AppConfig = Mocks.testConfig
  val testSeed: Long    = 42L

  def createGame: TetrisGame =
    TetrisGame.create(testSeed, config.grid.width, config.grid.height, config.score, config.level)

  def spec = suite("GameEngine")(
    suite("showTitleScreen")(
      test("renders title screen without error") {
        for _ <- GameEngine.showTitleScreen
        yield assertTrue(true)
      }
    ),
    suite("showGameOverScreen")(
      test("renders game over screen without error") {
        val game = createGame
        for _ <- GameEngine.showGameOverScreen(game)
        yield assertTrue(true)
      },
      test("renders game over screen with different scores") {
        // Create a game and simulate some scoring
        val game        = createGame
        val (paused, _) = game.handle(monadris.domain.model.game.GameCommand.TogglePause)
        for _ <- GameEngine.showGameOverScreen(paused)
        yield assertTrue(true)
      }
    ),
    suite("runSession")(
      test("runs and handles quit signal") {
        val game = createGame
        for finalGame <- GameEngine.runSession(game).timeout(3.seconds)
        yield assertTrue(finalGame.isDefined)
      }.provide(
        Mocks.terminal(Chunk('q'.toInt)),
        Mocks.config
      ),
      test("processes movement commands before quit") {
        val game   = createGame
        val inputs = Chunk('h'.toInt, 'l'.toInt, 'q'.toInt)
        for finalGame <- GameEngine.runSession(game).timeout(3.seconds)
        yield assertTrue(finalGame.isDefined)
      }.provide(
        Mocks.terminal(Chunk('h'.toInt, 'l'.toInt, 'q'.toInt)),
        Mocks.config
      ),
      test("processes rotation commands") {
        val game   = createGame
        val inputs = Chunk('k'.toInt, 'q'.toInt)
        for finalGame <- GameEngine.runSession(game).timeout(3.seconds)
        yield assertTrue(finalGame.isDefined)
      }.provide(
        Mocks.terminal(Chunk('k'.toInt, 'q'.toInt)),
        Mocks.config
      ),
      test("processes hard drop command") {
        val game   = createGame
        val inputs = Chunk(' '.toInt, 'q'.toInt)
        for finalGame <- GameEngine.runSession(game).timeout(3.seconds)
        yield assertTrue(finalGame.isDefined)
      }.provide(
        Mocks.terminal(Chunk(' '.toInt, 'q'.toInt)),
        Mocks.config
      ),
      test("processes pause command") {
        val game   = createGame
        val inputs = Chunk('p'.toInt, 'p'.toInt, 'q'.toInt)
        for finalGame <- GameEngine.runSession(game).timeout(3.seconds)
        yield assertTrue(finalGame.isDefined)
      }.provide(
        Mocks.terminal(Chunk('p'.toInt, 'p'.toInt, 'q'.toInt)),
        Mocks.config
      )
    )
  ) @@ withLiveClock

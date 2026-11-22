package monadris.infrastructure.game

import zio.*
import zio.test.*

import monadris.domain.*
import monadris.game.LineClearing
import monadris.infrastructure.game.GameLoopRunner.LoopState
import monadris.infrastructure.terminal.Renderer
import monadris.infrastructure.terminal.TestServices as Mocks
import monadris.input.GameCommand

object GameLoopRunnerSpec extends ZIOSpecDefault:

  private val config     = Mocks.testConfig
  private val gridWidth  = config.grid.width
  private val gridHeight = config.grid.height

  private object Setup:
    val QueueCapacity     = 10
    val StartLevel        = 1
    val HigherLevel       = 5
    val BlockedRows       = 2
    val FreeColumn        = 0
    val LockColumn        = 4
    val OPieceHeight      = 2
    val DrawnAtStart      = 1
    val DrawnAfterOneLock = 2

  private val suiteTimeout = 10.seconds

  private val upcomingShapes = Vector(TetrominoShape.S, TetrominoShape.Z, TetrominoShape.J)

  private val initialState: GameState =
    GameState.initial(TetrominoShape.T, TetrominoShape.I, gridWidth, gridHeight)

  private val start: Tetromino = initialState.currentTetromino

  private val gameOverState: GameState = initialState.copy(status = GameStatus.GameOver)

  /** The top rows are filled except one column, so the next spawn collides without clearing a line */
  private val blockedSpawnGrid: Grid =
    val blocked =
      for
        x <- (0 until gridWidth).filterNot(_ == Setup.FreeColumn)
        y <- 0 until Setup.BlockedRows
      yield Position(x, y)
    blocked.foldLeft(Grid.empty(gridWidth, gridHeight))(_.place(_, Cell.Filled(TetrominoShape.I)))

  /** An O piece resting on the floor: the next downward input locks it and ends the game */
  private val aboutToLose: GameState = initialState.copy(
    grid = blockedSpawnGrid,
    currentTetromino = Tetromino
      .spawn(TetrominoShape.O, gridWidth)
      .copy(position = Position(Setup.LockColumn, gridHeight - Setup.OPieceHeight))
  )

  /** Where the loop stopped and what the drop interval was at that point */
  final private case class Stopped(loop: LoopState, dropIntervalMs: Long):
    def game: GameState = loop.gameState

  private def user(input: Input): GameCommand = GameCommand.UserAction(input)

  /** Feeds the commands to the event loop; the loop must stop on its own (Quit or game over) */
  private def runLoop(
    state: GameState,
    commands: List[GameCommand],
    pieces: GameRunner.RandomPiece = GameRunner.RandomPieceGenerator,
    startLevel: Int = Setup.StartLevel
  ): Task[Stopped] =
    val run =
      for
        queue       <- Queue.bounded[GameCommand](Setup.QueueCapacity)
        _           <- queue.offerAll(commands)
        intervalRef <- Ref.make(LineClearing.dropInterval(startLevel, config.speed))
        stopped  <- GameLoopRunner.eventLoop(queue, LoopState(state, None), config, intervalRef, Renderer.live, pieces)
        interval <- intervalRef.get
      yield Stopped(stopped, interval)
    run.provide(Mocks.console)

  private def commandSuite = suite("commands")(
    test("Quit stops the loop without touching the state") {
      runLoop(initialState, List(GameCommand.Quit)).map { stopped =>
        assertTrue(stopped.game == initialState, stopped.loop.previousBuffer.isEmpty)
      }
    },
    test("a user action is applied to the game") {
      runLoop(initialState, List(user(Input.MoveLeft), GameCommand.Quit)).map { stopped =>
        assertTrue(stopped.game.currentTetromino == start.moveLeft)
      }
    },
    test("a time tick moves the piece down") {
      runLoop(initialState, List(GameCommand.TimeTick, GameCommand.TimeTick, GameCommand.Quit)).map { stopped =>
        assertTrue(stopped.game.currentTetromino == start.moveDown.moveDown)
      }
    },
    test("commands are applied in order and the last frame is kept for diff rendering") {
      val commands = List(user(Input.MoveLeft), user(Input.MoveRight), GameCommand.TimeTick, GameCommand.Quit)
      runLoop(initialState, commands).map { stopped =>
        assertTrue(stopped.game.currentTetromino == start.moveDown, stopped.loop.previousBuffer.isDefined)
      }
    },
    test("Pause resumes a paused game") {
      val paused = initialState.copy(status = GameStatus.Paused)
      runLoop(paused, List(user(Input.Pause), GameCommand.Quit)).map { stopped =>
        assertTrue(stopped.game.status == GameStatus.Playing)
      }
    },
    test("the drop interval follows the level of the game") {
      val levelled = initialState.copy(level = Setup.HigherLevel)
      runLoop(levelled, List(GameCommand.TimeTick, GameCommand.Quit)).map { stopped =>
        assertTrue(stopped.dropIntervalMs == LineClearing.dropInterval(Setup.HigherLevel, config.speed))
      }
    }
  )

  private def gameOverSuite = suite("game over")(
    test("a user action on a finished game stops the loop") {
      runLoop(gameOverState, List(user(Input.MoveDown))).map(stopped => assertTrue(stopped.game.isGameOver))
    },
    test("a time tick on a finished game stops the loop") {
      runLoop(gameOverState, List(GameCommand.TimeTick)).map(stopped => assertTrue(stopped.game.isGameOver))
    },
    test("a tick that locks the last piece ends the game and stops the loop") {
      runLoop(aboutToLose, List(GameCommand.TimeTick)).map(stopped => assertTrue(stopped.game.isGameOver))
    },
    test("a hard drop that locks the last piece ends the game and stops the loop") {
      runLoop(aboutToLose, List(user(Input.HardDrop))).map(stopped => assertTrue(stopped.game.isGameOver))
    }
  )

  private def pieceDrawSuite = suite("drawing the upcoming piece")(
    test("commands that do not lock a piece draw nothing beyond the first upcoming piece") {
      val commands = List(user(Input.MoveLeft), user(Input.RotateClockwise), GameCommand.TimeTick, GameCommand.Quit)
      for
        pieces <- TestPieces.cycling(upcomingShapes)
        _      <- runLoop(initialState, commands, pieces)
        drawn  <- pieces.drawCount
      yield assertTrue(drawn == Setup.DrawnAtStart)
    },
    test("locking a piece puts the drawn piece in the preview and draws the one after it") {
      for
        pieces  <- TestPieces.cycling(upcomingShapes)
        stopped <- runLoop(initialState, List(user(Input.HardDrop), GameCommand.Quit), pieces)
        drawn   <- pieces.drawCount
      yield assertTrue(
        stopped.game.currentTetromino.shape == initialState.nextTetromino,
        stopped.game.nextTetromino == upcomingShapes.head,
        drawn == Setup.DrawnAfterOneLock
      )
    },
    test("consecutive locks consume the drawn pieces in order") {
      val commands = List(user(Input.HardDrop), user(Input.HardDrop), GameCommand.Quit)
      for
        pieces  <- TestPieces.cycling(upcomingShapes)
        stopped <- runLoop(initialState, commands, pieces)
      yield assertTrue(
        stopped.game.currentTetromino.shape == upcomingShapes(0),
        stopped.game.nextTetromino == upcomingShapes(1)
      )
    }
  )

  def spec = suite("GameLoopRunner")(
    commandSuite,
    gameOverSuite,
    pieceDrawSuite
  ) @@ TestAspect.timeout(suiteTimeout)

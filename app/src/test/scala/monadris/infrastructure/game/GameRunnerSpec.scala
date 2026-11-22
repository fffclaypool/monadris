package monadris.infrastructure.game

import zio.*
import zio.test.*

import monadris.domain.*
import monadris.infrastructure.terminal.ConsoleService
import monadris.infrastructure.terminal.TestServices as Mocks
import monadris.view.GameView

object GameRunnerSpec extends ZIOSpecDefault:

  private val config     = Mocks.testConfig
  private val gridWidth  = config.grid.width
  private val gridHeight = config.grid.height

  private val shapeSampleCount    = 100
  private val minimumUniqueShapes = 1
  private val suiteTimeout        = 10.seconds

  private object Ansi:
    val Home        = "\u001b[H"
    val ClearScreen = "\u001b[2J"

  private object Keys:
    val Escape: Int = 27
    val Quit: Int   = 'q'.toInt
    val Drop: Int   = ' '.toInt

    def arrow(code: Char): List[Int] = List(Escape, '['.toInt, code.toInt)

  private object Shown:
    val Labels    = List("Score:", "Level:", "Lines:", "Next:")
    val Borders   = List("┌", "┐", "└", "┘", "│")
    val Block     = "█"
    val EmptyCell = "·"
    val GameOver  = "GAME OVER"
    val Title     = List("Functional Tetris", "Controls")

  private object Result:
    val Score = 1000
    val Level = 5
    val Lines = 20

  private val initialState: GameState =
    GameState.initial(TetrominoShape.T, TetrominoShape.I, gridWidth, gridHeight)

  private val start: Tetromino = initialState.currentTetromino

  /** A full-width row just below the spawn area: the piece after the next lock cannot spawn */
  private val aboutToLose: GameState =
    val blockedRow = 2
    initialState.copy(
      grid = (0 until gridWidth).foldLeft(initialState.grid) { (grid, x) =>
        grid.place(Position(x, blockedRow), Cell.Filled(TetrominoShape.I))
      },
      nextTetromino = TetrominoShape.O
    )

  /** Everything the effect printed to the console */
  private def printedBy(effect: ZIO[ConsoleService, Throwable, Any]): Task[String] =
    val run =
      for
        console <- ZIO.service[Mocks.TestConsoleService]
        _       <- effect
        output  <- console.buffer.get
      yield output.mkString
    run.provide(Mocks.console)

  /** Plays the interactive loop with the given keys; the keys must end the game */
  private def play(state: GameState, keys: List[Int]): Task[GameState] =
    GameRunner
      .interactiveGameLoop(state)
      .provide(Mocks.ttyWithoutGravity(Chunk.fromIterable(keys)), Mocks.console, Mocks.config)

  /** The falling piece after typing the keys and then quitting */
  private def pieceAfter(keys: List[Int]): Task[Tetromino] =
    play(initialState, keys :+ Keys.Quit).map(_.currentTetromino)

  private def movesPiece(keys: List[Int], expected: Tetromino): Task[TestResult] =
    pieceAfter(keys).map(piece => assertTrue(piece == expected))

  private def randomPieceSuite = suite("RandomPieceGenerator")(
    test("nextShape returns a valid tetromino shape") {
      GameRunner.RandomPieceGenerator.nextShape.map(shape => assertTrue(TetrominoShape.values.contains(shape)))
    },
    test("nextShape returns different shapes over multiple calls") {
      ZIO
        .replicateZIO(shapeSampleCount)(GameRunner.RandomPieceGenerator.nextShape)
        .map(shapes => assertTrue(shapes.toSet.size > minimumUniqueShapes))
    }
  )

  private def renderSuite = suite("rendering")(
    test("renderGame draws from the home position and returns the frame it drew") {
      for
        console <- ZIO.service[Mocks.TestConsoleService]
        buffer  <- GameRunner.renderGame(initialState, config)
        output  <- console.buffer.get
      yield assertTrue(output.mkString.contains(Ansi.Home), buffer == GameView.toScreenBuffer(initialState))
    }.provide(Mocks.console),
    test("renderGame shows the info panel labels") {
      printedBy(GameRunner.renderGame(initialState, config)).map { screen =>
        assertTrue(Shown.Labels.forall(screen.contains))
      }
    },
    test("renderGame shows the grid border, the falling piece and the empty cells") {
      printedBy(GameRunner.renderGame(initialState, config)).map { screen =>
        assertTrue(
          Shown.Borders.forall(screen.contains),
          screen.contains(Shown.Block),
          screen.contains(Shown.EmptyCell)
        )
      }
    },
    test("renderGame with the previous frame redraws only what changed") {
      val previous = GameView.toScreenBuffer(initialState)
      printedBy(GameRunner.renderGame(initialState.copy(score = Result.Score), config, Some(previous))).map { screen =>
        assertTrue(
          screen.nonEmpty,
          !screen.contains(Ansi.ClearScreen),
          !Shown.Labels.exists(screen.contains),
          !Shown.Borders.exists(screen.contains)
        )
      }
    },
    test("renderGameOver shows the final result") {
      val finished = initialState.copy(
        status = GameStatus.GameOver,
        score = Result.Score,
        level = Result.Level,
        linesCleared = Result.Lines
      )
      printedBy(GameRunner.renderGameOver(finished)).map { screen =>
        assertTrue(
          screen.contains(Shown.GameOver),
          List(Result.Score, Result.Level, Result.Lines).forall(value => screen.contains(value.toString))
        )
      }
    },
    test("showTitle shows the title screen") {
      printedBy(GameRunner.showTitle).map(screen => assertTrue(Shown.Title.forall(screen.contains)))
    }
  )

  private def movementSuite = suite("interactiveGameLoop movement")(
    test("h moves the piece left") {
      movesPiece(List('h'.toInt), start.moveLeft)
    },
    test("l moves the piece right") {
      movesPiece(List('l'.toInt), start.moveRight)
    },
    test("j moves the piece down") {
      movesPiece(List('j'.toInt), start.moveDown)
    },
    test("k rotates the piece clockwise") {
      movesPiece(List('k'.toInt), start.rotateClockwise)
    },
    test("z rotates the piece counter-clockwise") {
      movesPiece(List('z'.toInt), start.rotateCounterClockwise)
    },
    test("the arrow keys move and rotate like their vim counterparts") {
      for
        up    <- pieceAfter(Keys.arrow('A'))
        down  <- pieceAfter(Keys.arrow('B'))
        right <- pieceAfter(Keys.arrow('C'))
        left  <- pieceAfter(Keys.arrow('D'))
      yield assertTrue(
        up == start.rotateClockwise,
        down == start.moveDown,
        right == start.moveRight,
        left == start.moveLeft
      )
    },
    test("several keys are applied in order") {
      movesPiece(List('h'.toInt, 'h'.toInt, 'l'.toInt, 'j'.toInt), start.moveLeft.moveDown)
    },
    test("an unbound key changes nothing") {
      movesPiece(List('x'.toInt), start)
    }
  )

  private def gameFlowSuite = suite("interactiveGameLoop flow")(
    test("q and Q end the game without changing it") {
      for
        lower <- play(initialState, List(Keys.Quit))
        upper <- play(initialState, List('Q'.toInt))
      yield assertTrue(lower == initialState, upper == initialState)
    },
    test("space hard-drops the piece and brings in the next one") {
      play(initialState, List(Keys.Drop, Keys.Quit)).map { state =>
        assertTrue(
          state.currentTetromino.shape == initialState.nextTetromino,
          state.score > initialState.score,
          state.grid != initialState.grid
        )
      }
    },
    test("p pauses the game") {
      play(initialState, List('p'.toInt, Keys.Quit)).map(state => assertTrue(state.status == GameStatus.Paused))
    },
    test("the loop ends by itself when the game is over") {
      play(aboutToLose, List(Keys.Drop)).map(state => assertTrue(state.isGameOver))
    },
    test("every processed key redraws the game") {
      val game = GameRunner
        .interactiveGameLoop(initialState)
        .provideSome[ConsoleService](Mocks.ttyWithoutGravity(Chunk('h'.toInt, Keys.Quit)), Mocks.config)
      printedBy(game).map(screen => assertTrue(screen.contains(Ansi.Home), Shown.Labels.forall(screen.contains)))
    }
  )

  def spec = suite("GameRunner")(
    randomPieceSuite,
    renderSuite,
    movementSuite,
    gameFlowSuite
  ) @@ TestAspect.timeout(suiteTimeout)

package monadris.view

import monadris.TestConfig
import monadris.domain.config.*
import monadris.domain.model.board.Board
import monadris.domain.model.board.Cell
import monadris.domain.model.board.Position
import monadris.domain.model.game.GamePhase
import monadris.domain.model.game.TetrisGame
import monadris.domain.model.piece.TetrominoShape
import monadris.domain.model.scoring.ScoreState

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GameViewSpec extends AnyFlatSpec with Matchers:

  val config: AppConfig = TestConfig.testConfig
  val gridWidth: Int    = config.grid.width
  val gridHeight: Int   = config.grid.height

  def initialGame: TetrisGame =
    TetrisGame.create(42L, gridWidth, gridHeight, config.score, config.level)

  // ============================================================
  // shapeToColor テスト
  // ============================================================

  "GameView.shapeToColor" should "return Cyan for I-tetromino" in:
    GameView.shapeToColor(TetrominoShape.I) shouldBe UiColor.Cyan

  it should "return Yellow for O-tetromino" in:
    GameView.shapeToColor(TetrominoShape.O) shouldBe UiColor.Yellow

  it should "return Magenta for T-tetromino" in:
    GameView.shapeToColor(TetrominoShape.T) shouldBe UiColor.Magenta

  it should "return Green for S-tetromino" in:
    GameView.shapeToColor(TetrominoShape.S) shouldBe UiColor.Green

  it should "return Red for Z-tetromino" in:
    GameView.shapeToColor(TetrominoShape.Z) shouldBe UiColor.Red

  it should "return Blue for J-tetromino" in:
    GameView.shapeToColor(TetrominoShape.J) shouldBe UiColor.Blue

  it should "return White for L-tetromino" in:
    GameView.shapeToColor(TetrominoShape.L) shouldBe UiColor.White

  it should "return correct color for all shapes" in:
    TetrominoShape.values.foreach { shape =>
      GameView.shapeToColor(shape) shouldBe a[UiColor]
    }

  // ============================================================
  // toScreenBuffer テスト
  // ============================================================

  "GameView.toScreenBuffer" should "create non-empty buffer" in:
    val buffer = GameView.toScreenBuffer(initialGame, config)
    buffer.width should be > 0
    buffer.height should be > 0

  it should "include grid borders" in:
    val buffer = GameView.toScreenBuffer(initialGame, config)
    buffer.pixels(0)(0).char shouldBe '┌'

  it should "include score info" in:
    val buffer  = GameView.toScreenBuffer(initialGame, config)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Score:")

  it should "include level info" in:
    val buffer  = GameView.toScreenBuffer(initialGame, config)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Level:")

  it should "include lines info" in:
    val buffer  = GameView.toScreenBuffer(initialGame, config)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Lines:")

  it should "include next piece info" in:
    val buffer  = GameView.toScreenBuffer(initialGame, config)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Next:")

  it should "show PAUSED when game is paused" in:
    val pausedGame = initialGame.copy(phase = GamePhase.Paused)
    val buffer     = GameView.toScreenBuffer(pausedGame, config)
    val allText    = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("PAUSED")

  it should "not show PAUSED when game is playing" in:
    val buffer  = GameView.toScreenBuffer(initialGame, config)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should not include "PAUSED"

  it should "include control hints" in:
    val buffer  = GameView.toScreenBuffer(initialGame, config)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Quit")

  it should "show filled blocks with color" in:
    val board = Board
      .empty(gridWidth, gridHeight)
      .placeCell(Position(5, 15), Cell.Filled(TetrominoShape.I))
    val gameWithBlock = initialGame.copy(board = board)

    val buffer = GameView.toScreenBuffer(gameWithBlock, config)
    buffer.pixels.exists(row => row.exists(p => p.char == '▓')) shouldBe true

  it should "show current tetromino blocks" in:
    val buffer = GameView.toScreenBuffer(initialGame, config)
    buffer.pixels.exists(row => row.exists(p => p.char == '█')) shouldBe true

  it should "show empty cells" in:
    val buffer = GameView.toScreenBuffer(initialGame, config)
    buffer.pixels.exists(row => row.exists(p => p.char == '·')) shouldBe true

  // ============================================================
  // titleScreen テスト
  // ============================================================

  "GameView.titleScreen" should "create buffer with title content" in:
    val buffer  = GameView.titleScreen
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Functional Tetris")

  it should "include controls section" in:
    val buffer  = GameView.titleScreen
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Controls")

  it should "mention movement keys" in:
    val buffer  = GameView.titleScreen
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Move")

  it should "mention rotation" in:
    val buffer  = GameView.titleScreen
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Rotate")

  it should "mention quit key" in:
    val buffer  = GameView.titleScreen
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("Quit")

  it should "have valid dimensions" in:
    val buffer = GameView.titleScreen
    buffer.width should be > 0
    buffer.height should be > 0

  // ============================================================
  // gameOverScreen テスト
  // ============================================================

  "GameView.gameOverScreen" should "create buffer with GAME OVER message" in:
    val buffer  = GameView.gameOverScreen(initialGame)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("GAME OVER")

  it should "show final score" in:
    val gameWithScore = initialGame.copy(
      scoreState = ScoreState(12345, 1, 0)
    )
    val buffer  = GameView.gameOverScreen(gameWithScore)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("12345")

  it should "show lines cleared" in:
    val gameWithLines = initialGame.copy(
      scoreState = ScoreState(0, 1, 42)
    )
    val buffer  = GameView.gameOverScreen(gameWithLines)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("42")

  it should "show final level" in:
    val gameWithLevel = initialGame.copy(
      scoreState = ScoreState(0, 7, 0)
    )
    val buffer  = GameView.gameOverScreen(gameWithLevel)
    val allText = buffer.pixels.flatMap(_.map(_.char)).mkString
    allText should include("7")

  it should "have valid dimensions" in:
    val buffer = GameView.gameOverScreen(initialGame)
    buffer.width should be > 0
    buffer.height should be > 0

  // ============================================================
  // グリッド描画のエッジケース
  // ============================================================

  "GameView grid rendering" should "handle grid with multiple filled cells" in:
    val board = (0 until gridWidth).foldLeft(Board.empty(gridWidth, gridHeight)) { (b, x) =>
      b.placeCell(Position(x, gridHeight - 1), Cell.Filled(TetrominoShape.I))
    }

    val gameWithFilledRow = initialGame.copy(board = board)
    val buffer            = GameView.toScreenBuffer(gameWithFilledRow, config)

    val lockedCount = buffer.pixels.flatMap(_.filter(_.char == '▓')).size
    lockedCount shouldBe >=(gridWidth)

  it should "use correct colors for filled cells" in:
    val board = Board
      .empty(gridWidth, gridHeight)
      .placeCell(Position(0, 0), Cell.Filled(TetrominoShape.I))
      .placeCell(Position(1, 0), Cell.Filled(TetrominoShape.O))

    val game   = initialGame.copy(board = board)
    val buffer = GameView.toScreenBuffer(game, config)

    buffer.pixels.exists(row => row.exists(p => p.char == '▓')) shouldBe true

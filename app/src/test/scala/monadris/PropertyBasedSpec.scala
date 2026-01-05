package monadris

import zio.*
import zio.test.*

import monadris.domain.model.board.Board
import monadris.domain.model.board.Cell
import monadris.domain.model.board.Position
import monadris.domain.model.game.GameCommand
import monadris.domain.model.game.TetrisGame
import monadris.domain.model.piece.ActivePiece
import monadris.domain.model.piece.Rotation
import monadris.domain.model.piece.TetrominoShape
import monadris.domain.model.scoring.ScoreState
import monadris.infrastructure.TestServices

/**
 * 純粋関数のプロパティベーステスト
 * 任意の有効な入力に対して成立すべき不変条件をテスト
 */
object PropertyBasedSpec extends ZIOSpecDefault:

  val gridWidth: Int  = TestServices.testConfig.grid.width
  val gridHeight: Int = TestServices.testConfig.grid.height
  val config          = TestServices.testConfig

  // ============================================================
  // ジェネレータ
  // ============================================================

  val genTetrominoShape: Gen[Any, TetrominoShape] =
    Gen.fromIterable(TetrominoShape.values)

  val genRotation: Gen[Any, Rotation] =
    Gen.fromIterable(Rotation.values)

  val genPosition: Gen[Any, Position] =
    for
      x <- Gen.int(-5, gridWidth + 5)
      y <- Gen.int(-5, gridHeight + 5)
    yield Position(x, y)

  val genValidPosition: Gen[Any, Position] =
    for
      x <- Gen.int(0, gridWidth - 1)
      y <- Gen.int(0, gridHeight - 1)
    yield Position(x, y)

  val genActivePiece: Gen[Any, ActivePiece] =
    for
      shape    <- genTetrominoShape
      x        <- Gen.int(2, gridWidth - 3)
      y        <- Gen.int(0, gridHeight - 5)
      rotation <- genRotation
    yield ActivePiece(shape, Position(x, y), rotation)

  val genCommand: Gen[Any, GameCommand] =
    Gen.fromIterable(
      List(
        GameCommand.MoveLeft,
        GameCommand.MoveRight,
        GameCommand.SoftDrop,
        GameCommand.RotateCW,
        GameCommand.RotateCCW,
        GameCommand.HardDrop,
        GameCommand.TogglePause,
        GameCommand.Tick
      )
    )

  val genCommandSequence: Gen[Any, List[GameCommand]] =
    Gen.listOfBounded(0, 50)(genCommand)

  val genLevel: Gen[Any, Int] =
    Gen.int(1, 20)

  val genLinesCleared: Gen[Any, Int] =
    Gen.int(0, 4)

  val genSeed: Gen[Any, Long] =
    Gen.long(0, Long.MaxValue)

  def genTetrisGame: Gen[Any, TetrisGame] =
    for seed <- genSeed
    yield TetrisGame.create(seed, gridWidth, gridHeight, config.score, config.level)

  // ============================================================
  // Boardのプロパティ
  // ============================================================

  def spec = suite("Property-Based Tests")(
    suite("Board Properties")(
      test("empty board has all cells empty") {
        check(Gen.int(5, 15), Gen.int(10, 25)) { (width, height) =>
          val board = Board.empty(width, height)
          val allEmpty = (for
            x <- 0 until width
            y <- 0 until height
          yield board.get(Position(x, y)) == Some(Cell.Empty)).forall(identity)
          assertTrue(allEmpty)
        }
      },
      test("placing a cell and reading it back returns the same cell") {
        check(genValidPosition, genTetrominoShape) { (pos, shape) =>
          val board    = Board.empty(gridWidth, gridHeight)
          val cell     = Cell.Filled(shape)
          val newBoard = board.placeCell(pos, cell)
          assertTrue(newBoard.get(pos) == Some(cell))
        }
      },
      test("placing a cell does not affect other cells") {
        check(genValidPosition, genValidPosition, genTetrominoShape) { (pos1, pos2, shape) =>
          val board     = Board.empty(gridWidth, gridHeight)
          val cell      = Cell.Filled(shape)
          val newBoard  = board.placeCell(pos1, cell)
          val unchanged = pos1 == pos2 || newBoard.get(pos2) == Some(Cell.Empty)
          assertTrue(unchanged)
        }
      },
      test("board dimensions are preserved after operations") {
        check(genValidPosition, genTetrominoShape) { (pos, shape) =>
          val board    = Board.empty(gridWidth, gridHeight)
          val newBoard = board.placeCell(pos, Cell.Filled(shape))
          assertTrue(
            newBoard.width == board.width &&
              newBoard.height == board.height
          )
        }
      }
    ),

    // ============================================================
    // ActivePieceのプロパティ
    // ============================================================

    suite("ActivePiece Properties")(
      test("every piece has exactly 4 blocks") {
        check(genTetrominoShape, genRotation) { (shape, rotation) =>
          val piece = ActivePiece(shape, Position(5, 5), rotation)
          assertTrue(piece.blocks.size == 4)
        }
      },
      test("rotating piece 4 times returns to original orientation (via rotation field)") {
        check(genActivePiece) { piece =>
          // Test rotation field property directly since ActivePiece uses Board for wall kicks
          val rotated = piece.rotation.rotateClockwise.rotateClockwise.rotateClockwise.rotateClockwise
          assertTrue(rotated == piece.rotation)
        }
      },
      test("moving and reverse moving returns to original position") {
        check(genActivePiece) { piece =>
          val movedAndBack = piece.moveLeft.moveRight
          assertTrue(movedAndBack.position == piece.position)
        }
      },
      test("blocks are always within reasonable bounds after spawn") {
        check(genTetrominoShape) { shape =>
          val piece = ActivePiece.spawn(shape, gridWidth)
          val allBlocksValid = piece.blocks.forall { pos =>
            pos.x >= 0 && pos.x < gridWidth && pos.y >= 0
          }
          assertTrue(allBlocksValid)
        }
      }
    ),

    // ============================================================
    // ScoreStateのプロパティ
    // ============================================================

    suite("ScoreState Properties")(
      test("score is always non-negative") {
        val scoreConfig = config.score
        val levelConfig = config.level
        check(genLinesCleared, genLevel) { (lines, level) =>
          val state    = ScoreState(0, level, 0)
          val newState = state.addLines(lines, scoreConfig, levelConfig)
          assertTrue(newState.score >= 0)
        }
      },
      test("more lines cleared means higher or equal score at same level") {
        val scoreConfig = config.score
        val levelConfig = config.level
        check(genLevel) { level =>
          val baseState = ScoreState(0, level, 0)
          val scores = (0 to 4).map { lines =>
            baseState.addLines(lines, scoreConfig, levelConfig).score
          }
          val isMonotonic = scores.sliding(2).forall {
            case Seq(a, b) => b >= a
            case _         => true
          }
          assertTrue(isMonotonic)
        }
      },
      test("level is always at least initial level") {
        val levelConfig = config.level
        val scoreConfig = config.score
        check(Gen.int(0, 100)) { totalLines =>
          val state = ScoreState(0, 1, 0)
          // Add lines incrementally
          val finalState = (0 until totalLines).foldLeft(state) { (s, _) =>
            s.addLines(1, scoreConfig, levelConfig)
          }
          assertTrue(finalState.level >= 1)
        }
      },
      test("drop interval is always positive and within bounds") {
        val speedConfig = config.speed
        check(genLevel) { level =>
          val interval = ScoreState.dropInterval(level, speedConfig)
          assertTrue(
            interval >= speedConfig.minDropIntervalMs &&
              interval <= speedConfig.baseDropIntervalMs
          )
        }
      },
      test("higher level means faster or equal drop speed") {
        val speedConfig = config.speed
        check(Gen.int(1, 19)) { level =>
          val interval1 = ScoreState.dropInterval(level, speedConfig)
          val interval2 = ScoreState.dropInterval(level + 1, speedConfig)
          assertTrue(interval2 <= interval1)
        }
      }
    ),

    // ============================================================
    // TetrisGameのプロパティ（堅牢性）
    // ============================================================

    suite("TetrisGame Robustness")(
      test("any command sequence does not throw exception") {
        check(genCommandSequence) { commands =>
          val game = TetrisGame.create(42L, gridWidth, gridHeight, config.score, config.level)

          val finalGame = commands.foldLeft(game) { (g, cmd) =>
            if g.isOver then g
            else g.handle(cmd)._1
          }

          // 例外なく完了したことを確認するだけ
          assertTrue(true)
        }
      },
      test("score never decreases") {
        check(genCommandSequence) { commands =>
          val game = TetrisGame.create(42L, gridWidth, gridHeight, config.score, config.level)

          val scores = commands
            .scanLeft(game) { (g, cmd) =>
              if g.isOver then g
              else g.handle(cmd)._1
            }
            .map(_.scoreState.score)

          val neverDecreases = scores.sliding(2).forall {
            case Seq(a, b) => b >= a
            case _         => true
          }
          assertTrue(neverDecreases)
        }
      },
      test("level never decreases") {
        check(genCommandSequence) { commands =>
          val game = TetrisGame.create(42L, gridWidth, gridHeight, config.score, config.level)

          val levels = commands
            .scanLeft(game) { (g, cmd) =>
              if g.isOver then g
              else g.handle(cmd)._1
            }
            .map(_.scoreState.level)

          val neverDecreases = levels.sliding(2).forall {
            case Seq(a, b) => b >= a
            case _         => true
          }
          assertTrue(neverDecreases)
        }
      },
      test("pause toggle is idempotent after two presses") {
        check(genTetrisGame) { game =>
          val (pausedOnce, _)  = game.handle(GameCommand.TogglePause)
          val (pausedTwice, _) = pausedOnce.handle(GameCommand.TogglePause)
          assertTrue(pausedTwice.phase == game.phase)
        }
      },
      test("board dimensions remain constant") {
        check(genCommandSequence) { commands =>
          val game = TetrisGame.create(42L, gridWidth, gridHeight, config.score, config.level)

          val finalGame = commands.foldLeft(game) { (g, cmd) =>
            if g.isOver then g
            else g.handle(cmd)._1
          }

          assertTrue(
            finalGame.board.width == game.board.width &&
              finalGame.board.height == game.board.height
          )
        }
      }
    ),

    // ============================================================
    // Rotationのプロパティ
    // ============================================================

    suite("Rotation Properties")(
      test("clockwise and counter-clockwise are inverses") {
        check(genRotation) { rotation =>
          val result = rotation.rotateClockwise.rotateCounterClockwise
          assertTrue(result == rotation)
        }
      },
      test("four clockwise rotations return to original") {
        check(genRotation) { rotation =>
          val result = rotation.rotateClockwise.rotateClockwise.rotateClockwise.rotateClockwise
          assertTrue(result == rotation)
        }
      }
    )
  )

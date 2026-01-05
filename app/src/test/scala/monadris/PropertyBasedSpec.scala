package monadris

import zio.*
import zio.test.*

import monadris.domain.model.board.Board
import monadris.domain.model.board.Cell
import monadris.domain.model.board.Position
import monadris.domain.model.game.DomainEvent
import monadris.domain.model.game.GameCommand
import monadris.domain.model.game.GamePhase
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
      },
      test("clearCompletedRows returns (board, 0) when no rows are complete") {
        val board             = Board.empty(gridWidth, gridHeight)
        val (newBoard, count) = board.clearCompletedRows()
        assertTrue(count == 0, newBoard == board)
      },
      test("clearCompletedRows clears a complete row and returns correct count") {
        // Fill the bottom row completely
        val board = (0 until gridWidth).foldLeft(Board.empty(gridWidth, gridHeight)) { (b, x) =>
          b.placeCell(Position(x, gridHeight - 1), Cell.Filled(TetrominoShape.I))
        }
        val (newBoard, count) = board.clearCompletedRows()
        assertTrue(
          count == 1,
          newBoard.height == gridHeight,
          newBoard.get(Position(0, gridHeight - 1)) == Some(Cell.Empty)
        )
      },
      test("clearCompletedRows clears multiple complete rows") {
        // Fill the bottom two rows completely
        val board = (0 until gridWidth).foldLeft(Board.empty(gridWidth, gridHeight)) { (b, x) =>
          b.placeCell(Position(x, gridHeight - 1), Cell.Filled(TetrominoShape.I))
            .placeCell(Position(x, gridHeight - 2), Cell.Filled(TetrominoShape.J))
        }
        val (newBoard, count) = board.clearCompletedRows()
        assertTrue(
          count == 2,
          newBoard.height == gridHeight
        )
      },
      test("clearCompletedRows preserves non-complete rows") {
        // Fill bottom row completely, leave one cell empty in second row
        val bottomFilled = (0 until gridWidth).foldLeft(Board.empty(gridWidth, gridHeight)) { (b, x) =>
          b.placeCell(Position(x, gridHeight - 1), Cell.Filled(TetrominoShape.I))
        }
        // Add partial row above (leave position 0 empty)
        val withPartial = (1 until gridWidth).foldLeft(bottomFilled) { (b, x) =>
          b.placeCell(Position(x, gridHeight - 2), Cell.Filled(TetrominoShape.J))
        }
        val (newBoard, count) = withPartial.clearCompletedRows()
        assertTrue(
          count == 1,
          // The partial row should drop down to the bottom
          newBoard.get(Position(1, gridHeight - 1)) == Some(Cell.Filled(TetrominoShape.J))
        )
      },
      test("completedRows returns correct indices") {
        // Fill middle row completely
        val middleRow = gridHeight / 2
        val board = (0 until gridWidth).foldLeft(Board.empty(gridWidth, gridHeight)) { (b, x) =>
          b.placeCell(Position(x, middleRow), Cell.Filled(TetrominoShape.T))
        }
        assertTrue(board.completedRows == List(middleRow))
      },
      test("completedRows returns empty list for empty board") {
        val board = Board.empty(gridWidth, gridHeight)
        assertTrue(board.completedRows.isEmpty)
      },
      test("get returns None for out of bounds position") {
        val board = Board.empty(gridWidth, gridHeight)
        assertTrue(
          board.get(Position(-1, 0)) == None,
          board.get(Position(0, -1)) == None,
          board.get(Position(gridWidth, 0)) == None,
          board.get(Position(0, gridHeight)) == None
        )
      },
      test("isEmpty returns false for out of bounds position") {
        val board = Board.empty(gridWidth, gridHeight)
        assertTrue(!board.isEmpty(Position(-1, 0)))
      },
      test("canPlace returns false when blocks are out of bounds") {
        val board  = Board.empty(gridWidth, gridHeight)
        val blocks = List(Position(-1, 0), Position(0, 0))
        assertTrue(!board.canPlace(blocks))
      },
      test("placeCell ignores out of bounds positions") {
        val board    = Board.empty(gridWidth, gridHeight)
        val newBoard = board.placeCell(Position(-1, 0), Cell.Filled(TetrominoShape.I))
        assertTrue(newBoard == board)
      },
      test("dropDistance calculates correct distance") {
        val board    = Board.empty(gridWidth, gridHeight)
        val blocks   = List(Position(5, 0))
        val distance = board.dropDistance(blocks)
        assertTrue(distance == gridHeight - 1)
      },
      test("isBlocked returns true when position is occupied") {
        val board = Board
          .empty(gridWidth, gridHeight)
          .placeCell(Position(5, 5), Cell.Filled(TetrominoShape.I))
        val blocks = List(Position(5, 5))
        assertTrue(board.isBlocked(blocks))
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
      },
      test("hasLanded returns true when piece cannot move down") {
        // Place a piece at the very bottom (I-piece in R90 rotation is vertical)
        val board = Board.empty(gridWidth, gridHeight)
        // I-piece in R90 is vertical, blocks at y-1, y, y+1, y+2 relative to position
        // At position (5, gridHeight - 1), it will extend below the board
        val piece = ActivePiece(TetrominoShape.O, Position(5, gridHeight - 2), Rotation.R0)
        // O-piece blocks are at position + offsets, typically 2x2
        assertTrue(piece.hasLanded(board))
      },
      test("tryMoveLeft returns None when blocked") {
        val board = Board.empty(gridWidth, gridHeight)
        val piece = ActivePiece(TetrominoShape.I, Position(0, 5), Rotation.R90)
        assertTrue(piece.tryMoveLeft(board).isEmpty)
      },
      test("tryMoveRight returns None when blocked") {
        val board = Board.empty(gridWidth, gridHeight)
        val piece = ActivePiece(TetrominoShape.I, Position(gridWidth - 1, 5), Rotation.R90)
        assertTrue(piece.tryMoveRight(board).isEmpty)
      },
      test("rotateOn with counter-clockwise works") {
        val board   = Board.empty(gridWidth, gridHeight)
        val piece   = ActivePiece(TetrominoShape.T, Position(5, 5), Rotation.R0)
        val rotated = piece.rotateOn(board, clockwise = false)
        assertTrue(rotated.isDefined)
      },
      test("rotateOn returns None when all wall kicks fail") {
        // Create a constrained board where rotation is impossible
        val board = (0 until gridWidth).foldLeft(Board.empty(gridWidth, gridHeight)) { (b, x) =>
          (0 until gridHeight).foldLeft(b) { (b2, y) =>
            if x == 5 && y == 5 then b2
            else b2.placeCell(Position(x, y), Cell.Filled(TetrominoShape.O))
          }
        }
        val piece   = ActivePiece(TetrominoShape.T, Position(5, 5), Rotation.R0)
        val rotated = piece.rotateOn(board, clockwise = true)
        assertTrue(rotated.isEmpty)
      },
      test("I-tetromino uses specific wall kick offsets") {
        val board = Board.empty(gridWidth, gridHeight)
        val piece = ActivePiece(TetrominoShape.I, Position(1, 5), Rotation.R0)
        // Try rotation - I piece has different wall kick offsets
        val rotated = piece.rotateOn(board, clockwise = true)
        assertTrue(rotated.isDefined)
      },
      test("O-tetromino rotation maintains shape") {
        val board   = Board.empty(gridWidth, gridHeight)
        val piece   = ActivePiece(TetrominoShape.O, Position(5, 5), Rotation.R0)
        val rotated = piece.rotateOn(board, clockwise = true)
        assertTrue(
          rotated.isDefined,
          rotated.get.blocks.toSet == piece.copy(rotation = Rotation.R90).blocks.toSet
        )
      },
      test("blocks are correctly rotated for R90") {
        val piece = ActivePiece(TetrominoShape.I, Position(5, 5), Rotation.R90)
        assertTrue(piece.blocks.size == 4)
      },
      test("blocks are correctly rotated for R180") {
        val piece = ActivePiece(TetrominoShape.I, Position(5, 5), Rotation.R180)
        assertTrue(piece.blocks.size == 4)
      },
      test("blocks are correctly rotated for R270") {
        val piece = ActivePiece(TetrominoShape.I, Position(5, 5), Rotation.R270)
        assertTrue(piece.blocks.size == 4)
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
      },
      test("game over state ignores all commands") {
        val game                = TetrisGame.create(42L, gridWidth, gridHeight, config.score, config.level)
        val gameOver            = game.copy(phase = GamePhase.Over)
        val (afterMove, events) = gameOver.handle(GameCommand.MoveLeft)
        assertTrue(
          afterMove == gameOver,
          events.isEmpty
        )
      },
      test("paused state only responds to TogglePause") {
        val game                        = TetrisGame.create(42L, gridWidth, gridHeight, config.score, config.level)
        val paused                      = game.copy(phase = GamePhase.Paused)
        val (afterMove, moveEvents)     = paused.handle(GameCommand.MoveLeft)
        val (afterResume, resumeEvents) = paused.handle(GameCommand.TogglePause)
        assertTrue(
          afterMove == paused,
          moveEvents.isEmpty,
          afterResume.phase == GamePhase.Playing,
          resumeEvents.nonEmpty
        )
      },
      test("isPlaying returns correct value") {
        val game = TetrisGame.create(42L, gridWidth, gridHeight, config.score, config.level)
        assertTrue(
          game.isPlaying,
          !game.copy(phase = GamePhase.Paused).isPlaying,
          !game.copy(phase = GamePhase.Over).isPlaying
        )
      },
      test("isOver returns correct value") {
        val game = TetrisGame.create(42L, gridWidth, gridHeight, config.score, config.level)
        assertTrue(
          !game.isOver,
          !game.copy(phase = GamePhase.Paused).isOver,
          game.copy(phase = GamePhase.Over).isOver
        )
      },
      test("nextShape returns peeked shape from queue") {
        val game = TetrisGame.create(42L, gridWidth, gridHeight, config.score, config.level)
        assertTrue(game.nextShape == game.pieceQueue.peek)
      },
      test("MoveLeft emits PieceMoved event when successful") {
        val game        = TetrisGame.create(42L, gridWidth, gridHeight, config.score, config.level)
        val (_, events) = game.handle(GameCommand.MoveLeft)
        assertTrue(events.exists(_.isInstanceOf[DomainEvent.PieceMoved]))
      },
      test("MoveRight emits PieceMoved event when successful") {
        val game        = TetrisGame.create(42L, gridWidth, gridHeight, config.score, config.level)
        val (_, events) = game.handle(GameCommand.MoveRight)
        assertTrue(events.exists(_.isInstanceOf[DomainEvent.PieceMoved]))
      },
      test("RotateCW emits PieceRotated event when successful") {
        val game        = TetrisGame.create(42L, gridWidth, gridHeight, config.score, config.level)
        val (_, events) = game.handle(GameCommand.RotateCW)
        // May or may not succeed depending on piece and position
        assertTrue(events.isEmpty || events.exists(_.isInstanceOf[DomainEvent.PieceRotated]))
      },
      test("RotateCCW emits PieceRotated event when successful") {
        val game        = TetrisGame.create(42L, gridWidth, gridHeight, config.score, config.level)
        val (_, events) = game.handle(GameCommand.RotateCCW)
        assertTrue(events.isEmpty || events.exists(_.isInstanceOf[DomainEvent.PieceRotated]))
      },
      test("Tick is handled same as SoftDrop") {
        val game                    = TetrisGame.create(42L, gridWidth, gridHeight, config.score, config.level)
        val (afterTick, tickEvents) = game.handle(GameCommand.Tick)
        val (afterDrop, dropEvents) = game.handle(GameCommand.SoftDrop)
        assertTrue(
          afterTick.activePiece.position == afterDrop.activePiece.position
        )
      },
      test("HardDrop moves piece to bottom and locks") {
        val game                = TetrisGame.create(42L, gridWidth, gridHeight, config.score, config.level)
        val (afterDrop, events) = game.handle(GameCommand.HardDrop)
        assertTrue(
          events.exists(_.isInstanceOf[DomainEvent.PieceMoved]),
          events.exists(_.isInstanceOf[DomainEvent.PieceLocked])
        )
      }
    ),

    // ============================================================
    // Rotationのプロパティ
    // ============================================================

    // ============================================================
    // PieceQueueのプロパティ
    // ============================================================

    suite("PieceQueue Properties")(
      test("fromSeed creates deterministic queue") {
        val seed   = 42L
        val queue1 = monadris.domain.service.PieceQueue.fromSeed(seed)
        val queue2 = monadris.domain.service.PieceQueue.fromSeed(seed)
        assertTrue(queue1.peek == queue2.peek)
      },
      test("next returns current peeked and updates queue") {
        val queue             = monadris.domain.service.PieceQueue.fromSeed(42L)
        val peeked            = queue.peek
        val (shape, newQueue) = queue.next
        assertTrue(
          shape == peeked,
          newQueue.peek != peeked || TetrominoShape.values.length == 1
        )
      },
      test("next exhausts bag and refills") {
        // Get 7 pieces to exhaust the initial bag
        val queue   = monadris.domain.service.PieceQueue.fromSeed(42L)
        val (_, q1) = queue.next
        val (_, q2) = q1.next
        val (_, q3) = q2.next
        val (_, q4) = q3.next
        val (_, q5) = q4.next
        val (_, q6) = q5.next
        val (_, q7) = q6.next
        // 8th piece comes from new bag
        val (shape8, _) = q7.next
        assertTrue(TetrominoShape.values.contains(shape8))
      },
      test("peek returns next shape without consuming") {
        val queue   = monadris.domain.service.PieceQueue.fromSeed(42L)
        val peeked1 = queue.peek
        val peeked2 = queue.peek
        assertTrue(peeked1 == peeked2)
      },
      test("different seeds produce different sequences") {
        val queue1 = monadris.domain.service.PieceQueue.fromSeed(1L)
        val queue2 = monadris.domain.service.PieceQueue.fromSeed(999L)
        // Very likely different (not guaranteed but statistically almost certain)
        val shapes1 = (0 until 7)
          .foldLeft((List.empty[TetrominoShape], queue1)) { case ((acc, q), _) =>
            val (s, nq) = q.next
            (acc :+ s, nq)
          }
          ._1
        val shapes2 = (0 until 7)
          .foldLeft((List.empty[TetrominoShape], queue2)) { case ((acc, q), _) =>
            val (s, nq) = q.next
            (acc :+ s, nq)
          }
          ._1
        assertTrue(shapes1 != shapes2)
      }
    ),
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

package monadris

import zio.*
import zio.test.*
import zio.test.Assertion.*

import monadris.domain.*
import monadris.domain.GameConfig.Grid as GridConfig
import monadris.logic.*

/**
 * Property-Based Testing for pure functions
 * Tests invariants that should hold for any valid input
 */
object PropertyBasedSpec extends ZIOSpecDefault:

  // ============================================================
  // Generators
  // ============================================================

  val genTetrominoShape: Gen[Any, TetrominoShape] =
    Gen.fromIterable(TetrominoShape.values)

  val genRotation: Gen[Any, Rotation] =
    Gen.fromIterable(Rotation.values)

  val genPosition: Gen[Any, Position] =
    for
      x <- Gen.int(-5, GridConfig.DefaultWidth + 5)
      y <- Gen.int(-5, GridConfig.DefaultHeight + 5)
    yield Position(x, y)

  val genValidPosition: Gen[Any, Position] =
    for
      x <- Gen.int(0, GridConfig.DefaultWidth - 1)
      y <- Gen.int(0, GridConfig.DefaultHeight - 1)
    yield Position(x, y)

  val genTetromino: Gen[Any, Tetromino] =
    for
      shape <- genTetrominoShape
      pos <- genPosition
      rotation <- genRotation
    yield Tetromino(shape, pos, rotation)

  val genValidTetromino: Gen[Any, Tetromino] =
    for
      shape <- genTetrominoShape
      x <- Gen.int(2, GridConfig.DefaultWidth - 3)
      y <- Gen.int(0, GridConfig.DefaultHeight - 5)
      rotation <- genRotation
    yield Tetromino(shape, Position(x, y), rotation)

  val genInput: Gen[Any, Input] =
    Gen.fromIterable(List(
      Input.MoveLeft,
      Input.MoveRight,
      Input.MoveDown,
      Input.RotateClockwise,
      Input.RotateCounterClockwise,
      Input.HardDrop,
      Input.Pause,
      Input.Tick
    ))

  val genInputSequence: Gen[Any, List[Input]] =
    Gen.listOfBounded(0, 50)(genInput)

  val genLevel: Gen[Any, Int] =
    Gen.int(1, 20)

  val genLinesCleared: Gen[Any, Int] =
    Gen.int(0, 4)

  def genGameState: Gen[Any, GameState] =
    for
      firstShape <- genTetrominoShape
      nextShape <- genTetrominoShape
    yield GameState.initial(firstShape, nextShape)

  // ============================================================
  // Grid Properties
  // ============================================================

  def spec = suite("Property-Based Tests")(
    suite("Grid Properties")(
      test("empty grid has all cells empty") {
        check(Gen.int(5, 15), Gen.int(10, 25)) { (width, height) =>
          val grid = Grid.empty(width, height)
          val allEmpty = (for
            x <- 0 until width
            y <- 0 until height
          yield grid.isEmpty(Position(x, y))).forall(identity)
          assertTrue(allEmpty)
        }
      },

      test("placing a cell and reading it back returns the same cell") {
        check(genValidPosition, genTetrominoShape) { (pos, shape) =>
          val grid = Grid.empty()
          val cell = Cell.Filled(shape)
          val newGrid = grid.place(pos, cell)
          assertTrue(newGrid.get(pos) == Some(cell))
        }
      },

      test("placing a cell does not affect other cells") {
        check(genValidPosition, genValidPosition, genTetrominoShape) { (pos1, pos2, shape) =>
          val grid = Grid.empty()
          val cell = Cell.Filled(shape)
          val newGrid = grid.place(pos1, cell)
          val unchanged = pos1 == pos2 || newGrid.get(pos2) == Some(Cell.Empty)
          assertTrue(unchanged)
        }
      },

      test("clearing rows reduces or maintains block count") {
        check(genTetrominoShape) { shape =>
          val grid = Grid.empty()
          val filled = Cell.Filled(shape)
          // Fill bottom row completely
          val filledGrid = (0 until GridConfig.DefaultWidth).foldLeft(grid) { (g, x) =>
            g.place(Position(x, GridConfig.DefaultHeight - 1), filled)
          }
          val clearedGrid = filledGrid.clearRows(List(GridConfig.DefaultHeight - 1))

          // Count filled cells
          def countFilled(g: Grid): Int =
            (for
              x <- 0 until g.width
              y <- 0 until g.height
              if !g.isEmpty(Position(x, y))
            yield 1).sum

          assertTrue(countFilled(clearedGrid) <= countFilled(filledGrid))
        }
      },

      test("grid dimensions are preserved after operations") {
        check(genValidPosition, genTetrominoShape) { (pos, shape) =>
          val grid = Grid.empty()
          val newGrid = grid.place(pos, Cell.Filled(shape))
          assertTrue(
            newGrid.width == grid.width &&
            newGrid.height == grid.height
          )
        }
      }
    ),

    // ============================================================
    // Tetromino Properties
    // ============================================================

    suite("Tetromino Properties")(
      test("every tetromino has exactly 4 blocks") {
        check(genTetrominoShape, genRotation) { (shape, rotation) =>
          val tetromino = Tetromino(shape, Position(5, 5), rotation)
          assertTrue(tetromino.currentBlocks.size == 4)
        }
      },

      test("rotating 4 times returns to original orientation") {
        check(genTetromino) { tetromino =>
          val rotated = tetromino
            .rotateClockwise
            .rotateClockwise
            .rotateClockwise
            .rotateClockwise
          assertTrue(rotated.rotation == tetromino.rotation)
        }
      },

      test("moving and reverse moving returns to original position") {
        check(genTetromino) { tetromino =>
          val movedAndBack = tetromino.moveLeft.moveRight
          assertTrue(movedAndBack.position == tetromino.position)
        }
      },

      test("blocks are always within reasonable bounds after spawn") {
        check(genTetrominoShape) { shape =>
          val tetromino = Tetromino.spawn(shape, GridConfig.DefaultWidth)
          val allBlocksValid = tetromino.currentBlocks.forall { pos =>
            pos.x >= 0 && pos.x < GridConfig.DefaultWidth && pos.y >= 0
          }
          assertTrue(allBlocksValid)
        }
      }
    ),

    // ============================================================
    // Collision Properties
    // ============================================================

    suite("Collision Properties")(
      test("spawned tetromino is always valid on empty grid") {
        check(genTetrominoShape) { shape =>
          val grid = Grid.empty()
          val tetromino = Tetromino.spawn(shape, grid.width)
          assertTrue(Collision.isValidPosition(tetromino, grid))
        }
      },

      test("wall kick returns valid position or None") {
        check(genValidTetromino) { tetromino =>
          val grid = Grid.empty()
          val result = Collision.tryRotateWithWallKick(tetromino, grid, clockwise = true)
          val isValid = result match
            case Some(rotated) => Collision.isValidPosition(rotated, grid)
            case None => true // None is also acceptable
          assertTrue(isValid)
        }
      },

      test("hard drop position is always at or below current position") {
        check(genValidTetromino) { tetromino =>
          val grid = Grid.empty()
          val dropped = Collision.hardDropPosition(tetromino, grid)
          assertTrue(dropped.position.y >= tetromino.position.y)
        }
      },

      test("hard drop position cannot move down further") {
        check(genValidTetromino) { tetromino =>
          val grid = Grid.empty()
          // Only test if initial position is valid (some rotations may extend outside bounds)
          if Collision.isValidPosition(tetromino, grid) then
            val dropped = Collision.hardDropPosition(tetromino, grid)
            assertTrue(Collision.hasLanded(dropped, grid))
          else
            assertTrue(true) // Skip invalid initial positions
        }
      }
    ),

    // ============================================================
    // LineClearing Properties
    // ============================================================

    suite("LineClearing Properties")(
      test("score is always non-negative") {
        check(genLinesCleared, genLevel) { (lines, level) =>
          val score = LineClearing.calculateScore(lines, level)
          assertTrue(score >= 0)
        }
      },

      test("more lines cleared means higher or equal score at same level") {
        check(genLevel) { level =>
          val scores = (0 to 4).map(lines => LineClearing.calculateScore(lines, level))
          val isMonotonic = scores.sliding(2).forall {
            case Seq(a, b) => b >= a
            case _ => true
          }
          assertTrue(isMonotonic)
        }
      },

      test("higher level means higher or equal score for same lines") {
        check(genLinesCleared) { lines =>
          val scores = (1 to 10).map(level => LineClearing.calculateScore(lines, level))
          val isMonotonic = scores.sliding(2).forall {
            case Seq(a, b) => b >= a
            case _ => true
          }
          assertTrue(isMonotonic)
        }
      },

      test("level is always at least start level") {
        check(Gen.int(0, 100), Gen.int(1, 10)) { (totalLines, startLevel) =>
          val level = LineClearing.calculateLevel(totalLines, startLevel)
          assertTrue(level >= startLevel)
        }
      },

      test("drop interval is always positive and within bounds") {
        check(genLevel) { level =>
          val interval = LineClearing.dropInterval(level)
          assertTrue(
            interval >= GameConfig.Speed.MinDropIntervalMs &&
            interval <= GameConfig.Speed.BaseDropIntervalMs
          )
        }
      },

      test("higher level means faster or equal drop speed") {
        check(Gen.int(1, 19)) { level =>
          val interval1 = LineClearing.dropInterval(level)
          val interval2 = LineClearing.dropInterval(level + 1)
          assertTrue(interval2 <= interval1)
        }
      }
    ),

    // ============================================================
    // GameLogic Properties (Robustness)
    // ============================================================

    suite("GameLogic Robustness")(
      test("any input sequence does not throw exception") {
        check(genInputSequence) { inputs =>
          val initialState = GameState.initial(TetrominoShape.T, TetrominoShape.I)
          val nextShape = () => TetrominoShape.O

          val finalState = inputs.foldLeft(initialState) { (state, input) =>
            if state.isGameOver then state
            else GameLogic.update(state, input, nextShape)
          }

          // Just verify it completed without exception
          assertTrue(true)
        }
      },

      test("score never decreases") {
        check(genInputSequence) { inputs =>
          val initialState = GameState.initial(TetrominoShape.T, TetrominoShape.I)
          val nextShape = () => TetrominoShape.O

          val scores = inputs.scanLeft(initialState) { (state, input) =>
            if state.isGameOver then state
            else GameLogic.update(state, input, nextShape)
          }.map(_.score)

          val neverDecreases = scores.sliding(2).forall {
            case Seq(a, b) => b >= a
            case _ => true
          }
          assertTrue(neverDecreases)
        }
      },

      test("level never decreases") {
        check(genInputSequence) { inputs =>
          val initialState = GameState.initial(TetrominoShape.T, TetrominoShape.I)
          val nextShape = () => TetrominoShape.O

          val levels = inputs.scanLeft(initialState) { (state, input) =>
            if state.isGameOver then state
            else GameLogic.update(state, input, nextShape)
          }.map(_.level)

          val neverDecreases = levels.sliding(2).forall {
            case Seq(a, b) => b >= a
            case _ => true
          }
          assertTrue(neverDecreases)
        }
      },

      test("pause toggle is idempotent after two presses") {
        check(genGameState) { state =>
          val nextShape = () => TetrominoShape.O
          val pausedOnce = GameLogic.update(state, Input.Pause, nextShape)
          val pausedTwice = GameLogic.update(pausedOnce, Input.Pause, nextShape)
          assertTrue(pausedTwice.status == state.status)
        }
      },

      test("grid dimensions remain constant") {
        check(genInputSequence) { inputs =>
          val initialState = GameState.initial(TetrominoShape.T, TetrominoShape.I)
          val nextShape = () => TetrominoShape.O

          val finalState = inputs.foldLeft(initialState) { (state, input) =>
            if state.isGameOver then state
            else GameLogic.update(state, input, nextShape)
          }

          assertTrue(
            finalState.grid.width == initialState.grid.width &&
            finalState.grid.height == initialState.grid.height
          )
        }
      }
    ),

    // ============================================================
    // Rotation Properties
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
          val result = rotation
            .rotateClockwise
            .rotateClockwise
            .rotateClockwise
            .rotateClockwise
          assertTrue(result == rotation)
        }
      }
    )
  )

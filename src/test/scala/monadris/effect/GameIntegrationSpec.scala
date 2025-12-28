package monadris.effect

import zio.*
import zio.stream.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect

import monadris.domain.*

object GameIntegrationSpec extends ZIOSpecDefault:

  // 1. 描画内容をメモリに記録するテスト用レンダラー
  class TestRenderer(renderedStates: Ref[List[GameState]]) extends GameRunner.Renderer:
    def render(state: GameState): UIO[Unit] =
      renderedStates.update(history => state :: history)
    
    def renderGameOver(state: GameState): UIO[Unit] =
      renderedStates.update(history => state :: history)

  // 2. 決まった順番でミノを出すテスト用ジェネレータ
  class TestRandomPiece(shapes: Ref[List[TetrominoShape]]) extends GameRunner.RandomPiece:
    def nextShape: UIO[TetrominoShape] =
      shapes.modify {
        case head :: tail => (head, tail)
        case Nil          => (TetrominoShape.I, Nil) // デフォルト
      }

  // 3. テスト用のヘルパー
  def createSetupState(holeX: Int): GameState =
    val emptyGrid = Grid.empty(10, 20)
    val filled = Cell.Filled(TetrominoShape.O)

    val setupGrid = (0 until 4).foldLeft(emptyGrid) { (g, yOffset) =>
      val y = 19 - yOffset
      (0 until 10).filter(_ != holeX).foldLeft(g) { (accG, x) =>
        accG.place(Position(x, y), filled)
      }
    }

    GameState(
      grid = setupGrid,
      currentTetromino = Tetromino.spawn(TetrominoShape.I, 10),
      nextTetromino = TetrominoShape.O,
      score = 0,
      level = 1,
      linesCleared = 0,
      status = GameStatus.Playing
    )

  override def spec = suite("GameIntegrationSpec")(
    test("Full Game Simulation: Move, Drop, and Score") {
      for
        renderHistory <- Ref.make[List[GameState]](Nil)
        renderer      = new TestRenderer(renderHistory)
        pieceQueue    <- Ref.make(List(TetrominoShape.T, TetrominoShape.I, TetrominoShape.O))
        randomPiece   = new TestRandomPiece(pieceQueue)

        // T字を右に動かして落とす
        inputScenario = ZStream(
          Input.MoveRight,
          Input.MoveRight,
          Input.HardDrop
        )

        initialState = GameState.initial(TetrominoShape.T, TetrominoShape.I)

        finalState <- GameRunner.gameLoop(
          initialState,
          inputScenario,
          renderer,
          randomPiece
        )
        
        history <- renderHistory.get
      yield
        assert(finalState.score)(isGreaterThan(0)) &&
        assert(finalState.linesCleared)(equalTo(0)) &&
        assert(history.size)(isGreaterThan(1)) &&
        assert(finalState.grid.isEmpty(Position(7, 19)))(isFalse)
    },

    test("Game Over: Should transition to GameOver status when grid is full") {
      for
        renderHistory <- Ref.make[List[GameState]](Nil)
        renderer      = new TestRenderer(renderHistory)
        pieceQueue    <- Ref.make(List.fill(20)(TetrominoShape.I))
        randomPiece   = new TestRandomPiece(pieceQueue)

        inputScenario = ZStream.repeat(Input.HardDrop).take(20)

        initialState = GameState.initial(TetrominoShape.I, TetrominoShape.I)

        finalState <- GameRunner.gameLoop(
          initialState,
          inputScenario,
          renderer,
          randomPiece
        )
      yield
        assert(finalState.status)(equalTo(GameStatus.GameOver)) &&
        assert(finalState.isGameOver)(isTrue)
    },

    test("Scoring: Should clear 4 lines and award points (Tetris)") {
      for
        renderHistory <- Ref.make[List[GameState]](Nil)
        renderer      = new TestRenderer(renderHistory)
        pieceQueue    <- Ref.make(List(TetrominoShape.I, TetrominoShape.O))
        randomPiece   = new TestRandomPiece(pieceQueue)

        // 左端(x=0)を空けた状態。
        // 先に回転させて縦長にしてから左端(x=0)まで寄せる
        inputScenario = ZStream(
          Input.RotateCounterClockwise,
          Input.MoveLeft, Input.MoveLeft, Input.MoveLeft, Input.MoveLeft, Input.MoveLeft,
          Input.HardDrop
        )

        initialState = createSetupState(holeX = 0)

        finalState <- GameRunner.gameLoop(
          initialState,
          inputScenario,
          renderer,
          randomPiece
        )
      yield
        assert(finalState.linesCleared)(equalTo(4)) &&
        assert(finalState.score)(isGreaterThanEqualTo(800)) &&
        assert(finalState.grid.isEmpty(Position(1, 19)))(isTrue)
    },

    test("Wall Kick: Should rotate correctly near wall without crashing") {
      for
        renderHistory <- Ref.make[List[GameState]](Nil)
        renderer      = new TestRenderer(renderHistory)
        pieceQueue    <- Ref.make(List(TetrominoShape.T, TetrominoShape.O))
        randomPiece   = new TestRandomPiece(pieceQueue)

        // 右端で回転
        inputScenario = ZStream(
          Input.MoveRight, Input.MoveRight, Input.MoveRight, Input.MoveRight, Input.MoveRight,
          Input.RotateClockwise,
          Input.HardDrop
        )

        initialState = GameState.initial(TetrominoShape.T, TetrominoShape.O)

        finalState <- GameRunner.gameLoop(
          initialState,
          inputScenario,
          renderer,
          randomPiece
        )
      yield
        assert(finalState.isGameOver)(isFalse) &&
        assert(finalState.grid.isEmpty(Position(9, 18)))(isFalse)
    }
  ) @@ TestAspect.withLiveClock

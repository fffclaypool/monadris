package monadris.domain

/**
 * ゲームの進行状態
 */
enum GameStatus:
  case Playing
  case Paused
  case GameOver

/**
 * ゲーム全体の状態を表す不変データ構造
 */
final case class GameState(
  grid: Grid,
  currentTetromino: Tetromino,
  nextTetromino: TetrominoShape,
  score: Int,
  level: Int,
  linesCleared: Int,
  status: GameStatus
):
  def isPlaying: Boolean = status == GameStatus.Playing
  def isGameOver: Boolean = status == GameStatus.GameOver

object GameState:
  /**
   * 初期状態を生成
   */
  def initial(
    firstShape: TetrominoShape,
    nextShape: TetrominoShape,
    gridWidth: Int,
    gridHeight: Int
  ): GameState =
    GameState(
      grid = Grid.empty(gridWidth, gridHeight),
      currentTetromino = Tetromino.spawn(firstShape, gridWidth),
      nextTetromino = nextShape,
      score = 0,
      level = 1,
      linesCleared = 0,
      status = GameStatus.Playing
    )

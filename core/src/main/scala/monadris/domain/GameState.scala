package monadris.domain

enum GameStatus:
  case Playing
  case Paused
  case GameOver

final case class GameState(
  grid: Grid,
  currentTetromino: Tetromino,
  nextTetromino: TetrominoShape,
  score: Int,
  level: Int,
  linesCleared: Int,
  status: GameStatus
):
  def isPlaying: Boolean  = status == GameStatus.Playing
  def isGameOver: Boolean = status == GameStatus.GameOver

object GameState:
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

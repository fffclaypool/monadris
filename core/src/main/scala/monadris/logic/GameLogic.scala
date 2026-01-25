package monadris.logic

import monadris.domain.*
import monadris.domain.config.AppConfig

object GameLogic:

  final case class UpdateResult(
    state: GameState,
    needsNextShape: Boolean
  )

  def update(
    state: GameState,
    input: Input,
    nextShapeProvider: () => TetrominoShape,
    config: AppConfig
  ): GameState =
    if !state.isPlaying then
      input match
        case Input.Pause if state.status == GameStatus.Paused =>
          state.copy(status = GameStatus.Playing)
        case _ => state
    else
      input match
        case Input.MoveLeft               => handleMove(state, _.moveLeft)
        case Input.MoveRight              => handleMove(state, _.moveRight)
        case Input.MoveDown               => handleMoveDown(state, nextShapeProvider, config)
        case Input.RotateClockwise        => handleRotation(state, clockwise = true)
        case Input.RotateCounterClockwise => handleRotation(state, clockwise = false)
        case Input.HardDrop               => handleHardDrop(state, nextShapeProvider, config)
        case Input.Pause                  => state.copy(status = GameStatus.Paused)
        case Input.Quit                   => state
        case Input.Tick                   => handleTick(state, nextShapeProvider, config)

  private def handleMove(
    state: GameState,
    moveFn: Tetromino => Tetromino
  ): GameState =
    val movedTetromino = moveFn(state.currentTetromino)
    if Collision.isValidPosition(movedTetromino, state.grid) then state.copy(currentTetromino = movedTetromino)
    else state

  private def handleMoveDown(
    state: GameState,
    nextShapeProvider: () => TetrominoShape,
    config: AppConfig
  ): GameState =
    val movedTetromino = state.currentTetromino.moveDown
    if Collision.isValidPosition(movedTetromino, state.grid) then state.copy(currentTetromino = movedTetromino)
    else lockTetromino(state, nextShapeProvider, config)

  private def handleRotation(state: GameState, clockwise: Boolean): GameState =
    Collision.tryRotateWithWallKick(state.currentTetromino, state.grid, clockwise) match
      case Some(rotated) => state.copy(currentTetromino = rotated)
      case None          => state

  private def handleHardDrop(
    state: GameState,
    nextShapeProvider: () => TetrominoShape,
    config: AppConfig
  ): GameState =
    val droppedTetromino = Collision.hardDropPosition(state.currentTetromino, state.grid)
    val dropDistance     = droppedTetromino.position.y - state.currentTetromino.position.y
    val bonusScore       = dropDistance * 2

    val newState = state.copy(
      currentTetromino = droppedTetromino,
      score = state.score + bonusScore
    )
    lockTetromino(newState, nextShapeProvider, config)

  private def handleTick(
    state: GameState,
    nextShapeProvider: () => TetrominoShape,
    config: AppConfig
  ): GameState =
    handleMoveDown(state, nextShapeProvider, config)

  private def lockTetromino(
    state: GameState,
    nextShapeProvider: () => TetrominoShape,
    config: AppConfig
  ): GameState =
    val newGrid         = state.grid.placeTetromino(state.currentTetromino)
    val clearResult     = LineClearing.clearLines(newGrid, state.level, config.score)
    val newLinesCleared = state.linesCleared + clearResult.linesCleared
    val newLevel        = LineClearing.calculateLevel(newLinesCleared, config.level)
    val nextTetromino   = Tetromino.spawn(state.nextTetromino, state.grid.width)
    val upcomingShape   = nextShapeProvider()

    if Collision.isGameOver(nextTetromino, clearResult.grid) then
      state.copy(
        grid = clearResult.grid,
        score = state.score + clearResult.scoreGained,
        linesCleared = newLinesCleared,
        level = newLevel,
        status = GameStatus.GameOver
      )
    else
      state.copy(
        grid = clearResult.grid,
        currentTetromino = nextTetromino,
        nextTetromino = upcomingShape,
        score = state.score + clearResult.scoreGained,
        linesCleared = newLinesCleared,
        level = newLevel
      )

  def restart(
    firstShape: TetrominoShape,
    nextShape: TetrominoShape,
    gridWidth: Int,
    gridHeight: Int
  ): GameState =
    GameState.initial(firstShape, nextShape, gridWidth, gridHeight)

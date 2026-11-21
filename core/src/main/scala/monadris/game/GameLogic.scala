package monadris.game

import monadris.config.AppConfig
import monadris.domain.*

object GameLogic:

  /** The state after an input, and whether that input locked the current piece into the grid */
  final case class UpdateResult(
    state: GameState,
    pieceLocked: Boolean
  )

  private def unlocked(state: GameState): UpdateResult =
    UpdateResult(state, pieceLocked = false)

  /**
   * Applies one input.
   *
   * @param nextShape the shape that enters the preview if this input locks the current piece
   */
  def step(
    state: GameState,
    input: Input,
    nextShape: TetrominoShape,
    config: AppConfig
  ): UpdateResult =
    if !state.isPlaying then
      input match
        case Input.Pause if state.status == GameStatus.Paused =>
          unlocked(state.copy(status = GameStatus.Playing))
        case _ => unlocked(state)
    else
      input match
        case Input.MoveLeft               => unlocked(handleMove(state, _.moveLeft))
        case Input.MoveRight              => unlocked(handleMove(state, _.moveRight))
        case Input.MoveDown               => handleMoveDown(state, nextShape, config)
        case Input.RotateClockwise        => unlocked(handleRotation(state, clockwise = true))
        case Input.RotateCounterClockwise => unlocked(handleRotation(state, clockwise = false))
        case Input.HardDrop               => handleHardDrop(state, nextShape, config)
        case Input.Pause                  => unlocked(state.copy(status = GameStatus.Paused))
        case Input.Tick                   => handleMoveDown(state, nextShape, config)

  def update(
    state: GameState,
    input: Input,
    nextShape: TetrominoShape,
    config: AppConfig
  ): GameState =
    step(state, input, nextShape, config).state

  private def handleMove(
    state: GameState,
    moveFn: Tetromino => Tetromino
  ): GameState =
    val movedTetromino = moveFn(state.currentTetromino)
    if Collision.isValidPosition(movedTetromino, state.grid) then state.copy(currentTetromino = movedTetromino)
    else state

  private def handleMoveDown(
    state: GameState,
    nextShape: TetrominoShape,
    config: AppConfig
  ): UpdateResult =
    val movedTetromino = state.currentTetromino.moveDown
    if Collision.isValidPosition(movedTetromino, state.grid) then
      unlocked(state.copy(currentTetromino = movedTetromino))
    else lockTetromino(state, nextShape, config)

  private def handleRotation(state: GameState, clockwise: Boolean): GameState =
    Collision.tryRotateWithWallKick(state.currentTetromino, state.grid, clockwise) match
      case Some(rotated) => state.copy(currentTetromino = rotated)
      case None          => state

  private def handleHardDrop(
    state: GameState,
    nextShape: TetrominoShape,
    config: AppConfig
  ): UpdateResult =
    val droppedTetromino = Collision.hardDropPosition(state.currentTetromino, state.grid)
    val dropDistance     = droppedTetromino.position.y - state.currentTetromino.position.y
    val bonusScore       = dropDistance * config.score.hardDropPerCell
    val newState         = state.copy(
      currentTetromino = droppedTetromino,
      score = state.score + bonusScore
    )
    lockTetromino(newState, nextShape, config)

  private def lockTetromino(
    state: GameState,
    nextShape: TetrominoShape,
    config: AppConfig
  ): UpdateResult =
    val newGrid         = state.grid.placeTetromino(state.currentTetromino)
    val clearResult     = LineClearing.clearLines(newGrid, state.level, config.score)
    val newLinesCleared = state.linesCleared + clearResult.linesCleared
    val newLevel        = LineClearing.calculateLevel(newLinesCleared, config.level)
    val nextTetromino   = Tetromino.spawn(state.nextTetromino, state.grid.width)
    val lockedState     = state.copy(
      grid = clearResult.grid,
      score = state.score + clearResult.scoreGained,
      linesCleared = newLinesCleared,
      level = newLevel
    )

    val newState =
      if Collision.isGameOver(nextTetromino, clearResult.grid) then lockedState.copy(status = GameStatus.GameOver)
      else lockedState.copy(currentTetromino = nextTetromino, nextTetromino = nextShape)

    UpdateResult(newState, pieceLocked = true)

package monadris.logic

import monadris.domain.*

/**
 * ゲームの状態遷移を管理する純粋関数群
 * 中心となるシグネチャ: (GameState, Input) => GameState
 */
object GameLogic:

  /**
   * ゲーム状態の更新結果
   * nextShapeProvider は次のテトリミノを生成するために必要
   */
  final case class UpdateResult(
    state: GameState,
    needsNextShape: Boolean
  )

  /**
   * メインの状態更新関数
   * 純粋関数: (現在の状態, 入力) => 新しい状態
   *
   * @param state 現在のゲーム状態
   * @param input プレイヤーの入力
   * @param nextShapeProvider 次のテトリミノの形状を提供する関数（外部から注入）
   */
  def update(
    state: GameState,
    input: Input,
    nextShapeProvider: () => TetrominoShape
  ): GameState =
    if !state.isPlaying then
      input match
        case Input.Pause if state.status == GameStatus.Paused =>
          state.copy(status = GameStatus.Playing)
        case _ => state
    else
      input match
        case Input.MoveLeft             => handleMove(state, _.moveLeft)
        case Input.MoveRight            => handleMove(state, _.moveRight)
        case Input.MoveDown             => handleMoveDown(state, nextShapeProvider)
        case Input.RotateClockwise      => handleRotation(state, clockwise = true)
        case Input.RotateCounterClockwise => handleRotation(state, clockwise = false)
        case Input.HardDrop             => handleHardDrop(state, nextShapeProvider)
        case Input.Pause                => state.copy(status = GameStatus.Paused)
        case Input.Quit                 => state
        case Input.Tick                 => handleTick(state, nextShapeProvider)

  /**
   * 左右移動の処理
   */
  private def handleMove(
    state: GameState,
    moveFn: Tetromino => Tetromino
  ): GameState =
    val movedTetromino = moveFn(state.currentTetromino)
    if Collision.isValidPosition(movedTetromino, state.grid) then
      state.copy(currentTetromino = movedTetromino)
    else
      state

  /**
   * 下移動の処理
   */
  private def handleMoveDown(
    state: GameState,
    nextShapeProvider: () => TetrominoShape
  ): GameState =
    val movedTetromino = state.currentTetromino.moveDown
    if Collision.isValidPosition(movedTetromino, state.grid) then
      state.copy(currentTetromino = movedTetromino)
    else
      // 着地 -> 固定処理
      lockTetromino(state, nextShapeProvider)

  /**
   * 回転処理（ウォールキック対応）
   */
  private def handleRotation(state: GameState, clockwise: Boolean): GameState =
    Collision.tryRotateWithWallKick(state.currentTetromino, state.grid, clockwise) match
      case Some(rotated) => state.copy(currentTetromino = rotated)
      case None          => state

  /**
   * ハードドロップ処理
   */
  private def handleHardDrop(
    state: GameState,
    nextShapeProvider: () => TetrominoShape
  ): GameState =
    val droppedTetromino = Collision.hardDropPosition(state.currentTetromino, state.grid)
    // ドロップした距離に応じてボーナススコア
    val dropDistance = droppedTetromino.position.y - state.currentTetromino.position.y
    val bonusScore = dropDistance * 2

    val newState = state.copy(
      currentTetromino = droppedTetromino,
      score = state.score + bonusScore
    )
    lockTetromino(newState, nextShapeProvider)

  /**
   * Tick処理（時間経過による自動落下）
   */
  private def handleTick(
    state: GameState,
    nextShapeProvider: () => TetrominoShape
  ): GameState =
    handleMoveDown(state, nextShapeProvider)

  /**
   * テトリミノを盤面に固定し、次のテトリミノを生成
   */
  private def lockTetromino(
    state: GameState,
    nextShapeProvider: () => TetrominoShape
  ): GameState =
    // テトリミノを盤面に固定
    val newGrid = state.grid.placeTetromino(state.currentTetromino)

    // ライン消去
    val clearResult = LineClearing.clearLines(newGrid, state.level)

    // 総ライン数とレベルの更新
    val newLinesCleared = state.linesCleared + clearResult.linesCleared
    val newLevel = LineClearing.calculateLevel(newLinesCleared)

    // 次のテトリミノを生成
    val nextTetromino = Tetromino.spawn(state.nextTetromino, state.grid.width)
    val upcomingShape = nextShapeProvider()

    // ゲームオーバー判定
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

  /**
   * ゲームをリスタート
   */
  def restart(
    firstShape: TetrominoShape,
    nextShape: TetrominoShape,
    gridWidth: Int = 10,
    gridHeight: Int = 20
  ): GameState =
    GameState.initial(firstShape, nextShape, gridWidth, gridHeight)

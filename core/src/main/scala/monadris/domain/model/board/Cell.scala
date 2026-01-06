package monadris.domain.model.board

import monadris.domain.model.piece.TetrominoShape

/**
 * 盤面のセル状態
 */
enum Cell:
  case Empty
  case Filled(shape: TetrominoShape)

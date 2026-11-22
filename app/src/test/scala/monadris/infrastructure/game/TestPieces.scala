package monadris.infrastructure.game

import zio.*

import monadris.domain.TetrominoShape

/** Deterministic piece sources for tests. */
object TestPieces:

  /** Hands out `shapes` in order, starting over at the end, and counts the draws */
  final class Cycling(shapes: Vector[TetrominoShape], drawn: Ref[Int]) extends GameRunner.RandomPiece:
    def nextShape: UIO[TetrominoShape] =
      drawn.getAndUpdate(_ + 1).map(index => shapes(index % shapes.size))

    def drawCount: UIO[Int] = drawn.get

  def cycling(shapes: Vector[TetrominoShape]): UIO[Cycling] =
    Ref.make(0).map(Cycling(shapes, _))

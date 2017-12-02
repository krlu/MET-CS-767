package org.bu

import org.bu.met.types.{Pawn, PawnMoves, RookMoves, _}

/**
  * Created by Kenneth on 9/30/2017.
  */
package object met {
  val range: Range = 0 to 7
  type Board = Array[Array[Option[ChessPiece]]]
  type Position = (Int, Int) // (x,y) cartesian, NOT row/col!!!!!!

  def toRowCol(x: Int, y: Int): (Int, Int) = (range.max - y, x)
  def toXY(row: Int, col: Int): (Int, Int) = (col, range.max - row)
  
  def getMovesForPiece(piece: ChessPiece, x: Int, y: Int, board: Board): Seq[(Int, Int)] = piece match {
    case k: Knight => KnightMoves(x,y, board, k.color)
    case k: King => KingMoves(x,y, board, k.color)
    case q: Queen => QueenMoves(x,y, board, q.color)
    case b: Bishop => BishopMoves(x,y, board, b.color)
    case r: Rook => RookMoves(x,y, board, r.color)
    case p: Pawn =>
      val newPos = PawnMoves(x,y, board, p.color)
      newPos
  }

  // checks if the king (of given color) would be in check if it were at position (x,y)
  def inCheck(x: Int, y: Int, board: Board, pieces: Seq[(ChessPiece, Position)], color: Color): Boolean = {
    val opposingColor = if(color == White) Black else White
    val moves: Seq[(Int, Int)] = pieces
      .filter{case (piece, _) => piece.color == opposingColor}
      .flatMap{case(piece,(a,b)) => getMovesForPiece(piece, a, b, board)}
    moves.contains((x,y))
  }

  // TODO: Placeholder for actual chess-bot, used to generate initial training data
  def choose[A](it: Iterator[A]): A =
    it.zip(Iterator.iterate(1)(_ + 1)).reduceLeft((row, col) =>
      if (util.Random.nextInt(col._2) == 0) col else row
    )._1
}

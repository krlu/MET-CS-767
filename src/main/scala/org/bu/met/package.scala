package org.bu

import org.bu.met.types.{Pawn, PawnMoves, RookMoves, _}

/**
  * Created by Kenneth on 9/30/2017.
  */
package object met {
  val range = 0 to 7
  type Board = Array[Array[Option[ChessPiece]]]
  type Position = (Int, Int)
  def toRowCol(x: Int, y: Int) = (range.max - y, x)
  def toXY(row: Int, col: Int) = (col, range.max - row)
  
  def getMovesForPiece(piece: ChessPiece, x: Int, y: Int, board: Board)= piece match {
    case k: Knight => KnightMoves(x,y, board, k.color)
    case k: King => KingMoves(x,y, board, k.color)
    case q: Queen => QueenMoves(x,y, board, q.color)
    case b: Bishop => BishopMoves(x,y, board, b.color)
    case r: Rook => RookMoves(x,y, board, r.color)
    case p: Pawn =>
      val newPos = PawnMoves(x,y, board, p.color)
      newPos
  }
}

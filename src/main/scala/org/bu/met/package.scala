package org.bu

import org.bu.met.types.{Pawn, PawnMoves, RookMoves, _}

import scala.collection.immutable.IndexedSeq

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

  // checks if the king (of given color) would be in check if it were at position (x,y)
  def inCheck(x: Int, y: Int, board: Board, color: Color): Boolean ={
    val opposingColor = if(color == White) Black else White
    val possibleMoves: IndexedSeq[(Int, Int)] = (for {
      i: Int <- 0 to 7
      j: Int <- 0 to 7
    } yield {
      (board(i)(j), toXY(i, j))
    }).filter{case (option, _) => option.nonEmpty}
      .filter{case (option, _) => option.get.color == opposingColor}
      .flatMap{ case(pieceOpt, (a,b)) =>
        getMovesForPiece(pieceOpt.get, a,b, board)
      }
    possibleMoves.contains((x,y))
  }
}

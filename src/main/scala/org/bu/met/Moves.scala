package org.bu.met

trait Moves extends ((Int, Int, Array[Array[Option[ChessPiece]]], Color) => Seq[(Int, Int)])

object KnightMoves extends Moves {
  override def apply(x: Int, y: Int, board: Array[Array[Option[ChessPiece]]],color: Color): Seq[(Int, Int)] = {
    val possibleMoves =
      Seq((x+2, y+1), (x+2, y-1),
          (x+1, y+2), (x+1, y-2),
          (x-1, y+2), (x+1, y-2),
          (x-2, y+1), (x+2, y-1)).filter{case (a,b) =>
        val range = 0 to 7
        val edgeCondition = range.contains(a) && range.contains(b)
        val pieceCondition = board(b)(a) match {
          case Some(piece) => piece.color != color
          case _ => true
        }
        edgeCondition && pieceCondition
      }
    possibleMoves
  }
}

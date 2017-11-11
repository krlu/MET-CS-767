package org.bu.met.types

import org.bu.met._

trait Vector{
  val elements: Seq[Int]
  override def toString = elements.mkString(",")
}

protected case class StateVector(elements: Seq[Int]) extends Vector

object StateVector{
  private val NUM_CHESS_PIECES = 32
  def apply(activePieces: Seq[(ChessPiece, Position)], turn: Color): StateVector = {
    val turnInt = if(turn == Black) 0 else 1
    val statesArray = Array.fill(NUM_CHESS_PIECES)(Seq(1,0,0))
    activePieces.foreach{ case(piece,(x,y)) => statesArray(piece.stateVectorIndex) = Seq(0,x,y)}
    StateVector(statesArray.toSeq.flatten ++ Seq(turnInt))
  }
}
case class MoveVector(private val stateVectorIndex: Int,
                      private val desiredPosX: Int,
                      private val desiredPosY: Int) extends Vector{
  val elements = Seq(stateVectorIndex, desiredPosX, desiredPosY)
}

///**
//  * @param turn - 1 for white 0 for black
//  */
//[whiteRook1: (taken, x, y), // 0
// whiteKnight1: (taken, x, y),
// whiteBishop1: (taken, x, y),
// whiteQueen: (taken, x, y),
// whiteKing: (taken, x, y),
// whiteBishop2: (taken, x, y),
// whiteKnight2: (taken, x, y),
// whiteRook2: (taken, x, y),  // 7
// whitePawn1: (taken, x, y),
// whitePawn2: (taken, x, y),
// whitePawn3: (taken, x, y),
// whitePawn4: (taken, x, y),
// whitePawn5: (taken, x, y),
// whitePawn6: (taken, x, y),
// whitePawn7: (taken, x, y),
// whitePawn8: (taken, x, y), // 15
// blackRook1: (taken, x, y), // 16
// blackKnight1: (taken, x, y),
// blackBishop1: (taken, x, y),
// blackQueen: (taken, x, y),
// blackKing: (taken, x, y),
// blackBishop2: (taken, x, y),
// blackKnight2: (taken, x, y),
// blackRook2: (taken, x, y), // 23
// blackPawn1: (taken, x, y),
// blackPawn2: (taken, x, y),
// blackPawn3: (taken, x, y),
// blackPawn4: (taken, x, y),
// blackPawn5: (taken, x, y),
// blackPawn6: (taken, x, y),
// blackPawn7: (taken, x, y),
// blackPawn8: (taken, x, y), // 31
// turn: Int]

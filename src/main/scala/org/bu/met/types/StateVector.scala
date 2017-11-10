package org.bu.met.types

import org.bu.met._

///**
//  * @param turn - 1 for white 0 for black
//  */
//case class StateVector(whiteRook1: PieceState, // 0
//                       whiteKnight1: PieceState,
//                       whiteBishop1: PieceState,
//                       whiteQueen: PieceState,
//                       whiteKing: PieceState,
//                       whiteBishop2: PieceState,
//                       whiteKnight2: PieceState,
//                       whiteRook2: PieceState,  // 7
//                       whitePawn1: PieceState,
//                       whitePawn2: PieceState,
//                       whitePawn3: PieceState,
//                       whitePawn4: PieceState,
//                       whitePawn5: PieceState,
//                       whitePawn6: PieceState,
//                       whitePawn7: PieceState,
//                       whitePawn8: PieceState, // 15
//                       blackRook1: PieceState, // 16
//                       blackKnight1: PieceState,
//                       blackBishop1: PieceState,
//                       blackQueen: PieceState,
//                       blackKing: PieceState,
//                       blackBishop2: PieceState,
//                       blackKnight2: PieceState,
//                       blackRook2: PieceState, // 23
//                       blackPawn1: PieceState,
//                       blackPawn2: PieceState,
//                       blackPawn3: PieceState,
//                       blackPawn4: PieceState,
//                       blackPawn5: PieceState,
//                       blackPawn6: PieceState,
//                       blackPawn7: PieceState,
//                       blackPawn8: PieceState, // 31
//                       turn: Int)

sealed case class StateVector(elements: Seq[Int]){
  override def toString = elements.mkString(",")
}

object StateVector{
  def apply(moveVectorOpt: Option[MoveVector],
            activePieces: Seq[(ChessPiece, Position)],
            turn: Color): Option[StateVector] = moveVectorOpt match {
    case Some(mv) =>
      val turnInt = if(turn == Black) 0 else 1
      val statesArray = Array.fill(32)(PieceState(1,0,0))
      activePieces.foreach{ case(piece,(x,y)) =>
        statesArray(piece.stateVectorIndex) = PieceState(0,x,y)
      }
      Some(StateVector(statesArray.toSeq.flatMap{ ps => Seq(ps.taken, ps.xPos, ps.yPos)} ++ Seq(turnInt)))
    case None => None
  }
}

/**
  * @param taken - 1 for true 0 for false
  * @param xPos - between 0 and 7
  * @param yPos - between 0 and 7
  */
case class PieceState(taken: Int, xPos: Int, yPos: Int)

case class MoveVector(stateVectorIndex: Int, desiredPosX: Int, desiredPosY: Int){
  override def toString = s"$stateVectorIndex,$desiredPosX,$desiredPosY"
}

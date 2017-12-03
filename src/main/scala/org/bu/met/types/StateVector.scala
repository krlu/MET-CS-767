package org.bu.met.types

import org.bu.met._

trait Vector{
  val elements: Seq[Int]
  override def toString = elements.mkString(",")
}

sealed case class StateVector(elements: Seq[Int]) extends Vector

object StateVector{
  private val NUM_CHESS_PIECES = 32
  def apply(activePieces: Seq[(ChessPiece, Position)], turn: Color): StateVector = {
    val turnInt = if(turn == Black) 0 else 1
    val statesArray = Array.fill(NUM_CHESS_PIECES)(Seq(1,1,1)) // 1 - taken, (0,0) default position
    activePieces.foreach{ case(piece,(x,y)) => statesArray(piece.stateVectorIndex) = Seq(0,x,y)} // 0 - not taken
    StateVector(statesArray.toSeq.flatten ++ Seq(turnInt))
  }
}
case class MoveVector(private val stateVectorIndex: Int,
                      private val desiredPosX: Int,
                      private val desiredPosY: Int) extends Vector{
  val elements = Seq(stateVectorIndex, desiredPosX, desiredPosY)
  def toReadableMove: (Option[ChessPiece], Position) = {
    val pieceOpt: Option[ChessPiece] = stateVectorIndex match {
      case 0 => Some(Rook(White, stateVectorIndex))
      case 1 => Some(Knight(White, stateVectorIndex))
      case 2 => Some(Bishop(White, stateVectorIndex))
      case 3 => Some(Queen(White, stateVectorIndex))
      case 4 => Some(King(White, stateVectorIndex))
      case 5 => Some(Bishop(White, stateVectorIndex))
      case 6 => Some(Knight(White, stateVectorIndex))
      case 7 => Some(Rook(White, stateVectorIndex))
      case x: Int if (8 to 15).contains(x) => Some(Pawn(White, stateVectorIndex))
      case 16 => Some(Rook(Black, stateVectorIndex))
      case 17 => Some(Knight(Black, stateVectorIndex))
      case 18 => Some(Bishop(Black, stateVectorIndex))
      case 19 => Some(Queen(Black, stateVectorIndex))
      case 20 => Some(King(Black, stateVectorIndex))
      case 21 => Some(Bishop(Black, stateVectorIndex))
      case 22 => Some(Knight(Black, stateVectorIndex))
      case 23 => Some(Rook(Black, stateVectorIndex))
      case x: Int if (24 to 31).contains(x) => Some(Pawn(Black, stateVectorIndex))
      case _ => None
    }
    (pieceOpt, (desiredPosX, desiredPosY))
  }
}

///*
//  * @param turn - 1 for white 0 for black
//  */
//[whiteRook1: (taken, x, y), // 0
// whiteKnight1: (taken, x, y),
// whiteBishop1: (taken, x, y),
// whiteQueen: (taken, x, y),
// whiteKing: (taken, x, y),
// whiteBishop2: (taken, x, y),
// whiteKnight2: (taken, x, y),
// whiteRook2: (taken, x, y),  // 7 * 3
// whitePawn1: (taken, x, y),
// whitePawn2: (taken, x, y),
// whitePawn3: (taken, x, y),
// whitePawn4: (taken, x, y),
// whitePawn5: (taken, x, y),
// whitePawn6: (taken, x, y),
// whitePawn7: (taken, x, y),
// whitePawn8: (taken, x, y), // 15 * 3
// blackRook1: (taken, x, y), // 16 * 3
// blackKnight1: (taken, x, y),
// blackBishop1: (taken, x, y),
// blackQueen: (taken, x, y),
// blackKing: (taken, x, y),
// blackBishop2: (taken, x, y),
// blackKnight2: (taken, x, y),
// blackRook2: (taken, x, y), // 23 * 3
// blackPawn1: (taken, x, y),
// blackPawn2: (taken, x, y),
// blackPawn3: (taken, x, y),
// blackPawn4: (taken, x, y),
// blackPawn5: (taken, x, y),
// blackPawn6: (taken, x, y),
// blackPawn7: (taken, x, y),
// blackPawn8: (taken, x, y), // 31 * 3
// turn: Int]

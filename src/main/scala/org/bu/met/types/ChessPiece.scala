package org.bu.met.types

trait Color
case object White extends Color
case object Black extends Color

trait ChessPiece{
  val color: Color
  val stateVectorIndex: Int
}

case class Pawn(color: Color, stateVectorIndex: Int) extends ChessPiece
case class Knight(color: Color, stateVectorIndex: Int) extends ChessPiece
case class Bishop(color: Color, stateVectorIndex: Int) extends ChessPiece
case class Rook(color: Color, stateVectorIndex: Int) extends ChessPiece
case class Queen(color: Color, stateVectorIndex: Int) extends ChessPiece
case class King(color: Color, stateVectorIndex: Int) extends ChessPiece

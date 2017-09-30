package org.bu.met

trait Color
case object White extends Color
case object Black extends Color

trait ChessPiece{
  val color: Color
}

case class Pawn(override val color: Color) extends ChessPiece
case class Knight(override val color: Color) extends ChessPiece
case class Bishop(override val color: Color) extends ChessPiece
case class Rook(override val color: Color) extends ChessPiece
case class Queen(override val color: Color) extends ChessPiece
case class King(override val color: Color) extends ChessPiece

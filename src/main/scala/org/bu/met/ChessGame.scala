package org.bu.met

import org.bu.met.types._

class ChessGame(var activePieces: Seq[(ChessPiece, (Int, Int))], var turn: Color){
  activePieces.foreach{case(_, (x, y)) =>
    require(range.contains(x) && range.contains(y))
  }

  val board: Array[Array[Option[ChessPiece]]] = {
    val board: Array[Array[Option[ChessPiece]]] = Array.fill(8)(Array.fill(8)(None))
    activePieces.foreach{case(piece, (x, y)) => board(x)(y) = Some(piece)}
    board
  }

  def updateBoard(): Array[Array[Option[ChessPiece]]] = {
    val piecesToMove: Seq[(ChessPiece, (Int, Int))] = activePieces.filter{case(piece, _) => piece.color == turn}
    val (selectedPiece, (oldX,oldY)) = piecesToMove.head
    val (newX, newY) = selectedPiece match {
      case k: Knight => KnightMoves(oldX,oldY, board, k.color).head
      case p: Pawn =>
        val newPos = PawnMoves(oldX,oldY, board, p.color).head
        promotePawn(p, (oldX, oldY))
        newPos
      case _ => KnightMoves(oldX,oldY, board, turn).head
    }
    if(board(newX)(newY).nonEmpty){
      activePieces = activePieces.filter{ case(_, (x,y)) => x != newX && y != newY}
      board(newX)(newY) = Some(selectedPiece)
    }
    turn = if(turn.equals(White)) Black else White
    board
  }

  private def promotePawn(p: Pawn, position: (Int, Int)): Unit ={
    def promotablePawn(y: Int, color: Color) =
      (color == White && y == 7) || (color == Black && y == 0)
    val (x,y) = position
    if(promotablePawn(y, p.color)){
      val possiblePieces = List(Knight, Queen).map(piece => piece(p.color))
      val promotedPiece = possiblePieces.head
      board(x)(y) = Some(promotedPiece)
      activePieces = activePieces.filter{case( piece, pos) => piece match {
        case p: Pawn => pos == (x,y)
        case _ => false
      }}
      activePieces = activePieces :+ ((promotedPiece, (x,y)))
    }
  }
  def printBoard(): Unit ={
    for{
      x <- range
      y <- range.reverse
    }println(board(y)(x))
  }
}


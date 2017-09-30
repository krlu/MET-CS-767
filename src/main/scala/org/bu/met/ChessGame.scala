package org.bu.met

class ChessGame(var activePieces: Seq[(ChessPiece, (Int, Int))], var turn: Color){
  val range = 0 to 7
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
      case _ => KnightMoves(oldX,oldY, board, turn).head
    }
    if(board(newX)(newY).nonEmpty){
      activePieces = activePieces.filter{ case(_, (x,y)) => x != newX && y != newY}
      board(newX)(newY) = Some(selectedPiece)
    }
    turn = if(turn.equals(White)) Black else White
    board
  }
  def printBoard(): Unit ={
    for{
      x <- range
      y <- range.reverse
    }println(board(y)(x))
  }
}


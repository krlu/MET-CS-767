package org.bu.met

import org.bu.met.types._

class ChessGame(var activePieces: Seq[(ChessPiece, (Int, Int))], var turn: Color){
  activePieces.foreach{case(_, (row, col)) =>
    require(range.contains(row) && range.contains(col))
  }

  val board: Board = {
    val board: Board = Array.fill(8)(Array.fill(8)(None))
    activePieces.foreach{case(piece, (row, col)) => board(col)(row) = Some(piece)}
    board
  }

  // TODO: Default behavior uniformly selects piece and move
  def updateBoard(): Array[Array[Option[ChessPiece]]] = {
    val piecesToMove: Seq[(ChessPiece, (Int, Int))] = activePieces.filter{case(piece, _) => piece.color == turn}
    val (selectedPiece, (oldX,oldY)) = choose(piecesToMove.iterator)
    val possibleMoves = selectedPiece match {
      case k: Knight => KnightMoves(oldX,oldY, board, k.color)
      case k: King => KingMoves(oldX,oldY, board, k.color)
      case q: Queen => QueenMoves(oldX,oldY, board, q.color)
      case b: Bishop => BishopMoves(oldX,oldY, board, b.color)
      case r: Rook => RookMoves(oldX,oldY, board, r.color)
      case p: Pawn =>
        val newPos = PawnMoves(oldX,oldY, board, p.color)
        promotePawn(p, (oldX, oldY))
        newPos
    }
    val (newX, newY) = choose(possibleMoves.iterator)
    if(board(newX)(newY).nonEmpty){
      activePieces = activePieces.filter{ case(_, (row,col)) => row != newX && col != newY}
      board(newX)(newY) = Some(selectedPiece)
    }
    turn = if(turn.equals(White)) Black else White
    board
  }

  private def choose[A](it: Iterator[A]): A =
    it.zip(Iterator.iterate(1)(_ + 1)).reduceLeft((row, col) =>
      if (util.Random.nextInt(col._2) == 0) col else row
    )._1

  private def promotePawn(p: Pawn, position: (Int, Int)): Unit ={
    def promotablePawn(col: Int, color: Color) =
      (color == White && col == 7) || (color == Black && col == 0)
    val (row,col) = position
    if(promotablePawn(col, p.color)){
      val possiblePieces = List(Knight, Queen).map(piece => piece(p.color))
      val promotedPiece = possiblePieces.head
      board(row)(col) = Some(promotedPiece)
      activePieces = activePieces.filter{case( piece, pos) => piece match {
        case p: Pawn => pos == (row,col)
        case _ => false
      }}
      activePieces = activePieces :+ ((promotedPiece, (row,col)))
    }
  }

  def printBoard(): Unit ={
    for(row <- range){
      println()
      for(col <- range) {
        board(row)(col) match {
          case None => print("[           ]")
          case Some(piece) => print(s"[$piece]")
        }
      }
    }
  }
}


package org.bu.met

import org.bu.met.types._

class ChessGame(var activePieces: Seq[(ChessPiece, Position)], var turn: Color){
  activePieces.foreach{case(_, (row, col)) =>
    require(range.contains(row) && range.contains(col))
  }

  val board: Board = {
    val board: Board = Array.fill(8)(Array.fill(8)(None))
    activePieces.foreach{case(piece, (x,y))=>
      val (row,col) = toRowCol(x,y)
      board(row)(col) = Some(piece)
    }
    board
  }

  // TODO: Default behavior uniformly selects piece and move
  def updateBoard(): Unit = {
    val piecesToMove: Seq[(ChessPiece, Position)] = activePieces.filter{case(piece, _) => piece.color == turn}
    val kingOpt: Option[(ChessPiece, (Int, Int))] = piecesToMove.find{case (piece, _) => piece.isInstanceOf[King]}
    val (selectedPiece, (oldX,oldY)) = kingOpt match {
      case Some((king: King, (x,y))) =>
        if(KingMoves.inCheck(x,y, board, turn)) (king, (x,y))
        else choose(piecesToMove.iterator)
      case _ => choose(piecesToMove.iterator)
    }

    val possibleMoves: Seq[Position] = getMovesForPiece(selectedPiece, oldX, oldY, board)
    turn = if (turn.equals(White)) Black else White
    if(possibleMoves.nonEmpty) {
      val (newX, newY) = choose(possibleMoves.iterator)
      val (newRow, newCol) = toRowCol(newX, newY)
      val (oldRow, oldCol) = toRowCol(oldX, oldY)
      activePieces = activePieces.filter { case (_, (x, y)) => x != oldX || y != oldY }
      if (board(newRow)(newCol).nonEmpty) {
        activePieces = activePieces.filter { case (_, (x, y)) => x != newX || y != newY }
      }
      selectedPiece match {
        case p: Pawn => attemptPromotePawn(p, (oldX, oldY))
        case _ =>
      }
      activePieces = activePieces :+(selectedPiece, (newX, newY))
      board(newRow)(newCol) = Some(selectedPiece)
      board(oldRow)(oldCol) = None
    }
    else{
      println(s"$turn wins!!!")
    }
  }

  // TODO: Placeholder for actual chess-bot
  private def choose[A](it: Iterator[A]): A =
    it.zip(Iterator.iterate(1)(_ + 1)).reduceLeft((row, col) =>
      if (util.Random.nextInt(col._2) == 0) col else row
    )._1

  private def attemptPromotePawn(p: Pawn, position: Position): Unit ={
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
          case None => print("[            ]")
          case Some(piece) => print(s"[$piece]")
        }
      }
    }
  }
}

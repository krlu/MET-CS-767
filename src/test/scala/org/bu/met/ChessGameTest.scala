package org.bu.met

import org.bu.met.types._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Kenneth on 9/30/2017.
  */
class ChessGameTest extends FlatSpec with Matchers {
  val game = new ChessGame(Seq((Knight(Black), toRowCol(5,5))), Black)
  "Chess Game" should "move queens" in {
    for(_ <- 0 to 100) {
      val game = new ChessGame(Seq((Queen(Black), (5,5))), Black)
      val qMoves = QueenMoves(5,5, game.board, Black)
      assert(qMoves ==
        List(
          (6,5), (7,5),
          (4,5), (3,5), (2,5), (1,5), (0,5),
          (5,6), (5,7),
          (5,4), (5,3), (5,2), (5,1), (5,0),
          (6,4), (7,3),
          (4,4), (3,3), (2,2), (1,1), (0,0),
          (6,6), (7,7),
          (4,6), (3,7)))
      game.updateBoard()
      assert(movedCorrectly(qMoves, game.board, Queen(Black)))
    }
  }

  "Chess Game" should "move bishops" in {
    for(_ <- 0 to 100) {
      val game = new ChessGame(Seq((Bishop(Black), (5,5))), Black)
      val qMoves = BishopMoves(5,5, game.board, Black)
      assert(qMoves ==
        List(
          (6,4), (7,3),
          (4,4), (3,3), (2,2), (1,1), (0,0),
          (6,6), (7,7),
          (4,6), (3,7)))
      game.updateBoard()
      assert(movedCorrectly(qMoves, game.board, Bishop(Black)))
    }
  }

  "Chess Game" should "move rooks" in {
    for (_ <- 0 to 100) {
      val game = new ChessGame(Seq((Rook(Black), (5, 5))), Black)
      val qMoves = RookMoves(5, 5, game.board, Black)
      assert(qMoves ==
        List(
          (6,5), (7,5),
          (4,5), (3,5), (2,5), (1,5), (0,5),
          (5,6), (5,7),
          (5,4), (5,3), (5,2), (5,1), (5,0)
        )
      )
      game.updateBoard()
      assert(movedCorrectly(qMoves, game.board, Rook(Black)))
    }
  }

  "Chess Game" should "move knights" in {
    for (_ <- 0 to 100) {
      val game = new ChessGame(Seq((Knight(Black), (5, 5))), Black)
      val qMoves = KnightMoves(5, 5, game.board, Black)
      assert(qMoves == List((7,6), (7,4), (6,7), (6,3), (4,7), (4,3), (3,6), (3,4)))
      game.updateBoard()
      assert(movedCorrectly(qMoves, game.board, Knight(Black)))
    }
  }

  "Chess Game" should "move kings" in {
    for (_ <- 0 to 100) {
      val game = new ChessGame(Seq((King(Black), (5, 5))), Black)
      val qMoves = KingMoves(5, 5, game.board, Black)
      assert(qMoves == Seq((5,6), (5,4), (6,6), (6,5), (6,4), (4,6), (4,5), (4,4)))
      game.updateBoard()
      assert(movedCorrectly(qMoves, game.board, King(Black)))
    }

    for (_ <- 0 to 100) {
      val game = new ChessGame(Seq((King(Black), (7, 7))), Black)
      val qMoves = KingMoves(7, 7, game.board, Black)
      assert(qMoves == Seq((7,6), (6,7), (6,6)))
      game.updateBoard()
      assert(movedCorrectly(qMoves, game.board, King(Black)))
    }
  }


  private def movedCorrectly(moves: Seq[Position], board: Board, piece: ChessPiece): Boolean ={
    moves.exists { case (x, y) =>
      val (r, c) = toRowCol(x, y)
      board(r)(c).nonEmpty && board(r)(c).get == piece
    }
  }
}

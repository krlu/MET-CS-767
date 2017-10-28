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
      val possibleMoves = QueenMoves(5,5, game.board, Black)
      assert(possibleMoves ==
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
      assert(movedCorrectly(possibleMoves, game.board, Queen(Black)))
    }
  }

  "Chess Game" should "move bishops" in {
    for(_ <- 0 to 100) {
      val game = new ChessGame(Seq((Bishop(Black), (5,5))), Black)
      val possibleMoves = BishopMoves(5,5, game.board, Black)
      assert(possibleMoves ==
        List(
          (6,4), (7,3),
          (4,4), (3,3), (2,2), (1,1), (0,0),
          (6,6), (7,7),
          (4,6), (3,7)))
      game.updateBoard()
      assert(movedCorrectly(possibleMoves, game.board, Bishop(Black)))
    }
    for(_ <- 0 to 100){
      val game = new ChessGame(Seq((Bishop(Black), (5, 5)), (Pawn(Black), (4,4))), Black)
      val bishopMoves = BishopMoves(5, 5, game.board, Black)
      val pawnMoves = PawnMoves(4,4, game.board, Black)
      assert(bishopMoves ==
        List(
          (6, 4), (7, 3),
          (6, 6), (7, 7),
          (4, 6), (3, 7)
        )
      )
      game.updateBoard()
      val pawnMoved = movedCorrectly(bishopMoves, game.board, Bishop(Black))
      val bishopMoved = movedCorrectly(pawnMoves, game.board, Pawn(Black))
      assert(pawnMoved || bishopMoved)
    }
    for(_ <- 0 to 100){
      val game = new ChessGame(Seq((Bishop(Black), (5, 5)), (Pawn(White), (4,4))), Black)
      val bishopMoves = BishopMoves(5, 5, game.board, Black)
      assert(bishopMoves ==
        List(
          (6, 4), (7, 3),
          (4, 4), (6, 6),
          (7, 7), (4, 6),
          (3, 7))
      )
      game.updateBoard()
      assert(movedCorrectly(bishopMoves, game.board, Bishop(Black)))
    }
  }

  "Chess Game" should "move rooks" in {
    for (_ <- 0 to 100) {
      val game = new ChessGame(Seq((Rook(Black), (5, 5))), Black)
      val possibleMoves = RookMoves(5, 5, game.board, Black)
      assert(possibleMoves ==
        List(
          (6,5), (7,5),
          (4,5), (3,5), (2,5), (1,5), (0,5),
          (5,6), (5,7),
          (5,4), (5,3), (5,2), (5,1), (5,0)
        )
      )
      game.updateBoard()
      assert(movedCorrectly(possibleMoves, game.board, Rook(Black)))
    }
  }

  "Chess Game" should "move knights" in {
    for (_ <- 0 to 100) {
      val game = new ChessGame(Seq((Knight(Black), (5, 5))), Black)
      val possibleMoves = KnightMoves(5, 5, game.board, Black)
      assert(possibleMoves == List((7,6), (7,4), (6,7), (6,3), (4,7), (4,3), (3,6), (3,4)))
      game.updateBoard()
      assert(movedCorrectly(possibleMoves, game.board, Knight(Black)))
    }
  }

  "Chess Game" should "move kings" in {
    for (_ <- 0 to 100) {
      val game = new ChessGame(Seq((King(Black), (5, 5))), Black)
      val possibleMoves = KingMoves(5, 5, game.board, Black)
      assert(possibleMoves == Seq((5,6), (5,4), (6,6), (6,5), (6,4), (4,6), (4,5), (4,4)))
      game.updateBoard()
      assert(movedCorrectly(possibleMoves, game.board, King(Black)))
    }

    val game = new ChessGame(Seq((King(Black), (7, 7))), Black)
    val possibleMoves = KingMoves(7, 7, game.board, Black)
    assert(possibleMoves == Seq((7,6), (6,7), (6,6)))
    game.updateBoard()
    assert(movedCorrectly(possibleMoves, game.board, King(Black)))

  }

  "Chess Game" should "checkmate king" in {
    val game = new ChessGame(Seq((King(Black), (7, 7)),(Rook(White), (7, 5)), (King(White), (5, 6))), Black)
    val possibleMoves = KingMoves(7, 7, game.board, Black).filter{case (x,y) => !inCheck(x,y, game.board, Black)}
    assert(possibleMoves == Seq())
    game.updateBoard()
  }

  "Chess Game" should "move pawns" in {
    for (_ <- 0 to 100) {
      val game = new ChessGame(Seq((Pawn(Black), (5, 5))), Black)
      val possibleMoves = PawnMoves(5, 5, game.board, Black)
      assert(possibleMoves == Seq((5,4)))
      game.updateBoard()
      assert(movedCorrectly(possibleMoves, game.board, Pawn(Black)))
    }
    val game = new ChessGame(Seq((Pawn(Black), (5, 5)),(Pawn(Black), (5, 4)), (Pawn(White), (4, 4))), Black)
    val possibleBlackMoves = PawnMoves(5, 5, game.board, Black)
    val possibleWhiteMoves = PawnMoves(4, 4, game.board, White)
    assert(possibleBlackMoves == Seq((4,4)))
    assert(possibleWhiteMoves == Seq((4,5), (5,5)))
    game.updateBoard()

    val game2 = new ChessGame(Seq((Pawn(Black), (5, 5)), (Pawn(White), (5, 4))), Black)
    val possibleMoves = PawnMoves(5, 5, game2.board, Black)
    assert(possibleMoves == Seq())
    game2.updateBoard()
  }


  private def movedCorrectly(moves: Seq[Position], board: Board, piece: ChessPiece): Boolean ={
    moves.exists { case (x, y) =>
      val (r, c) = toRowCol(x, y)
      board(r)(c).nonEmpty && board(r)(c).get == piece
    }
  }
}

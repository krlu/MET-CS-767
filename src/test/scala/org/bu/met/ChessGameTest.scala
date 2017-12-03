package org.bu.met

import org.bu.met.data.DataGenerator
import org.bu.met.types._
import org.scalatest.{FlatSpec, Matchers}

class ChessGameTest extends FlatSpec with Matchers {
  val game = new ChessGame(Seq((Knight(Black, 17), toRowCol(5,5))), Black)
  "Chess Game" should "move queens" in {
    for(_ <- 0 to 100) {
      val game = new ChessGame(Seq((Queen(Black, 19), (5,5))), Black)
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
      assert(movedCorrectly(possibleMoves, game.board, Queen(Black, 19)))
    }
  }

  "Chess Game" should "move bishops" in {
    for(_ <- 0 to 100) {
      val game = new ChessGame(Seq((Bishop(Black, 18), (5,5))), Black)
      val possibleMoves = BishopMoves(5,5, game.board, Black)
      assert(possibleMoves ==
        List(
          (6,4), (7,3),
          (4,4), (3,3), (2,2), (1,1), (0,0),
          (6,6), (7,7),
          (4,6), (3,7)))
      game.updateBoard()
      assert(movedCorrectly(possibleMoves, game.board, Bishop(Black, 18)))
    }
    for(_ <- 0 to 100){
      val game = new ChessGame(Seq((Bishop(Black, 18), (5, 5)), (Pawn(Black, 24), (4,4))), Black)
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
      val pawnMoved = movedCorrectly(bishopMoves, game.board, Bishop(Black, 18))
      val bishopMoved = movedCorrectly(pawnMoves, game.board, Pawn(Black, 24))
      assert(pawnMoved || bishopMoved)
    }
    for(_ <- 0 to 100){
      val game = new ChessGame(Seq((Bishop(Black, 18), (5, 5)), (Pawn(White, 8), (4,4))), Black)
      val bishopMoves = BishopMoves(5, 5, game.board, Black)
      assert(bishopMoves ==
        List(
          (6, 4), (7, 3),
          (4, 4), (6, 6),
          (7, 7), (4, 6),
          (3, 7))
      )
      game.updateBoard()
      assert(movedCorrectly(bishopMoves, game.board, Bishop(Black, 18)))
    }
  }

  "Chess Game" should "move rooks" in {
    for (_ <- 0 to 100) {
      val game = new ChessGame(Seq((Rook(Black, 16), (5, 5))), Black)
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
      assert(movedCorrectly(possibleMoves, game.board, Rook(Black, 16)))
    }
  }

  "Chess Game" should "move knights" in {
    for (_ <- 0 to 100) {
      val game = new ChessGame(Seq((Knight(Black, 17), (5, 5))), Black)
      val possibleMoves = KnightMoves(5, 5, game.board, Black)
      assert(possibleMoves == List((7,6), (7,4), (6,7), (6,3), (4,7), (4,3), (3,6), (3,4)))
      game.updateBoard()
      assert(movedCorrectly(possibleMoves, game.board, Knight(Black, 17)))
    }
  }

  "Chess Game" should "move kings" in {
    for (_ <- 0 to 100) {
      val game = new ChessGame(Seq((King(Black, 20), (5, 5))), Black)
      val possibleMoves = KingMoves(5, 5, game.board, Black)
      assert(possibleMoves == Seq((5,6), (5,4), (6,6), (6,5), (6,4), (4,6), (4,5), (4,4)))
      game.updateBoard()
      assert(movedCorrectly(possibleMoves, game.board, King(Black, 20)))
    }

    val game = new ChessGame(Seq((King(Black, 20), (7, 7))), Black)
    val possibleMoves = KingMoves(7, 7, game.board, Black)
    assert(possibleMoves == Seq((7,6), (6,7), (6,6)))
    game.updateBoard()
    assert(movedCorrectly(possibleMoves, game.board, King(Black, 20)))

  }

  "Chess Game" should "checkmate king" in {
    val game = new ChessGame(Seq((King(Black, 20), (7, 7)),(Rook(White, 0), (7, 5)), (King(White, 4), (5, 6))), Black)
    val possibleMoves = KingMoves(7, 7, game.board, Black).filter{case (x,y) => !inCheck(x,y, game.board, game.activePieces, Black)}
    assert(possibleMoves == Seq())
    game.updateBoard()
  }

  "Chess Game" should "move pawns" in {
    for (_ <- 0 to 100) {
      val game = new ChessGame(Seq((Pawn(Black, 24), (5, 5))), Black)
      val possibleMoves = PawnMoves(5, 5, game.board, Black)
      assert(possibleMoves == Seq((5,4)))
      game.updateBoard()
      assert(movedCorrectly(possibleMoves, game.board, Pawn(Black, 24)))
    }
    val game = new ChessGame(Seq((Pawn(Black, 24), (5, 5)),(Pawn(Black, 25), (5, 4)), (Pawn(White, 8), (4, 4))), Black)
    val possibleBlackMoves = PawnMoves(5, 5, game.board, Black)
    val possibleWhiteMoves = PawnMoves(4, 4, game.board, White)
    assert(possibleBlackMoves == Seq((4,4)))
    assert(possibleWhiteMoves == Seq((4,5), (5,5)))
    game.updateBoard()
  }

  "Chess Game" should "stalemate" in {
    val game = new ChessGame(Seq((Pawn(Black, 24), (5, 5)), (Pawn(White, 8), (5, 4))), Black)
    val possibleMoves = PawnMoves(5, 5, game.board, Black)
    assert(possibleMoves == Seq())
    game.updateBoard()
  }

  private def movedCorrectly(moves: Seq[Position], board: Board, piece: ChessPiece): Boolean ={
    moves.exists { case (x, y) =>
      val (r, c) = toRowCol(x, y)
      board(r)(c).nonEmpty && board(r)(c).get == piece
    }
  }

  "King" should "be in check" in {
    val pieces = Seq((King(White,4),(4,3)), (King(Black,20),(6,7)), (Rook(White,0),(3,7)))
    val game = new ChessGame(pieces, Black)
    val(oldRow, oldCol) = toRowCol(6,7)
    val(newRow, newCol) = toRowCol(7,7)
    val hypotheticalBoard = game.deepCopyBoard
    hypotheticalBoard(oldRow)(oldCol) = None
    hypotheticalBoard(newRow)(newCol) = Some(King(Black,20))
    assert(inCheck(7,7,hypotheticalBoard, game.activePieces,Black))
  }
  "King" should "be in checkmate" in {
    val pieces = Seq(
      (King(Black, 20), (7, 4)),
      (Pawn(Black, 24), (6, 4)),
      (Pawn(Black, 25), (7, 5)),
      (Queen(Black, 19), (3, 1)),
      (King(White, 4), (5,4)),
      (Knight(White, 1), (5, 5)),
      (Bishop(White, 2), (6, 2)))
    val game = new ChessGame(pieces, Black)
    game.runGame(1)
  }

  "Inference model" should "checkmate" in {
    val model = new InferenceModel
    model.train("case_specific_training_sets/old_training_data.csv")
    Seq(DataGenerator.testCase1, DataGenerator.testCase2, DataGenerator.testCase3).foreach{ pieces =>
      val game = new ChessGame(pieces, White)
      game.runGame(10, Some(model))
      assert(game.numMoves == 2)
    }
  }
}

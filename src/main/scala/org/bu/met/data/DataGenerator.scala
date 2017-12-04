package org.bu.met.data

import org.bu.met._
import org.bu.met.types._

object DataGenerator {

  val testCase1 = Seq(
    (King(Black, 20), (7, 7)),
    (King(White, 4), (1, 1)),
    (Rook(White, 0), (5, 1)),
    (Rook(White, 7), (6, 2)))

  val testCase2 = Seq(
    (King(Black, 20), (7, 4)),
    (Pawn(Black, 24), (6, 4)),
    (Pawn(Black, 25), (7, 5)),
    (Pawn(Black, 26), (7, 3)),
    (Pawn(White, 8), (6, 2)),
    (King(White, 4), (5,4)))

  val testCase3 = Seq(
    (King(Black, 20), (7, 4)),
    (Pawn(Black, 24), (6, 4)),
    (Pawn(Black, 25), (7, 5)),
    (Queen(Black, 19), (3, 1)),
    (King(White, 4), (5,4)),
    (Knight(White, 1), (4, 3)),
    (Bishop(White, 2), (6, 2)))

  val testCase4 = Seq(
    (Queen(Black, 19), (2, 7)),
    (Rook(Black, 16), (6, 7)),
    (King(Black, 20), (7, 7)),
    (Pawn(Black, 24), (5, 6)),
    (Pawn(Black, 25), (7, 6)),
    (Pawn(Black, 26), (6, 5)),
    (Pawn(White, 8), (5, 5)),
    (Bishop(White, 2), (5, 4)),
    (Queen(White, 3), (7, 3)),
    (King(White, 4), (4, 2)),
    (Rook(White, 0), (5, 2)))

  def main(args: Array[String]): Unit ={
    val pieces = testCase4
    for(i <- 1 to 100) {
      var gameOver = false
      var turn: Color = White
      while (!gameOver && turn == White) {
        val game = new ChessGame(pieces, White)
        game.runGame(2)
        turn = game.turn
        gameOver = game.isGameOver
      }
    }
  }
}

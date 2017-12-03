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

  def main(args: Array[String]): Unit ={
    // TODO: Need to setup good training example
    val pieces = testCase3
//    var totalmoves = Seq.empty[Int]
    for(i <- 1 to 100) {
//      println(i)
      var gameOver = false
      var turn: Color = White
      while (!gameOver && turn == White) {
        val game = new ChessGame(pieces, White)
        game.runGame(50)
        turn = game.turn
        gameOver = game.isGameOver
//        totalmoves = totalmoves :+ game.numMoves
      }
    }
//    println(totalmoves)
//    println(s"Average numMoves: ${totalmoves.sum/totalmoves.size}")
  }
}

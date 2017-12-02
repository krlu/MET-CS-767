package org.bu.met.data

import org.bu.met._
import org.bu.met.types._

object DataGenerator {
  def main(args: Array[String]): Unit ={
    // TODO: Need to setup good training example
    val pieces = Seq(
      (King(Black, 20), (7, 4)),
      (Pawn(Black, 24), (6, 4)),
      (Pawn(Black, 25), (7, 5)),
      (Queen(Black, 19), (3, 1)),
      (King(White, 4), (5,4)),
      (Knight(White, 1), (4, 3)),
      (Bishop(White, 2), (6, 2)))
    println(StateVector(pieces, White))
    for(i <- 1 to 1000) {
//      println(i)
      var gameOver = false
      var turn: Color = White
      while (!gameOver && turn == White) {
        val game = new ChessGame(pieces, White)
        game.runGame(5)
        turn = game.turn
        gameOver = game.isGameOver
      }
    }
  }
}

package org.bu.met.data

import org.bu.met._
import org.bu.met.types._

object DataGenerator {
  def main(args: Array[String]): Unit ={
    // TODO: Need to setup good training example
    val pieces = Seq((King(Black, 20), (7, 7)),(King(White, 4), (6,5)), (Rook(White, 0), (0, 0)))
    for(i <- 1 to 10) {
      println(i)
      var gameOver = false
      while (!gameOver) {
        val game = new ChessGame(pieces, White)
        game.runGame(50)
        gameOver = game.isGameOver
      }
    }
  }
}

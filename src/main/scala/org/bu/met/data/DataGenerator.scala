package org.bu.met.data

import org.bu.met._
import org.bu.met.types._

object DataGenerator {
  def main(args: Array[String]): Unit ={
    // TODO: Need to setup good training example
    val game = new ChessGame(Seq((Pawn(Black, 24), (5, 5)),(Pawn(Black, 25), (5, 4)), (Pawn(White, 8), (4, 4))), Black)
    game.runGame()
  }
}

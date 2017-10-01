package org.bu.met

import org.bu.met.types.{Black, Knight}

object MainApplication {
  def main(args: Array[String]): Unit = {
    val game = new ChessGame(Seq((Knight(Black), toRowCol(5,5))), Black)
    game.printBoard()
  }
  def toRowCol(x: Int, y: Int) = (x, range.max - y)
}

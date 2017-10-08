package org.bu.met

import org.bu.met.types.{Black, Knight}
import org.scalatest.Matchers
import org.scalatest.FlatSpec

/**
  * Created by Kenneth on 9/30/2017.
  */
class ChessGameTest extends FlatSpec with Matchers {
  val game = new ChessGame(Seq((Knight(Black), toRowCol(5,5))), Black)
  "Chess Game" should "Have pieces in the correct positions" in {

  }
  private def toRowCol(x: Int, y: Int) = (x, range.max - y)
}

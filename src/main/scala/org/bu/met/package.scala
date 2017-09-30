package org.bu

import org.bu.met.types.ChessPiece

/**
  * Created by Kenneth on 9/30/2017.
  */
package object met {
  val range = 0 to 7
  type Board = Array[Array[Option[ChessPiece]]]
}

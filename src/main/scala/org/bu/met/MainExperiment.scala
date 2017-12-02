package org.bu.met

import org.bu.met.types._

object MainExperiment {
  def main(args: Array[String]) {
    val model = new InferenceModel
    model.train("training_data.csv")
    val pieces = Seq(
      (King(Black, 20), (7, 4)),
      (King(White, 4), (5,4)),
      (Pawn(Black, 24), (6, 4)),
      (Pawn(Black, 25), (7, 5)),
      (Queen(White, 3), (6, 2)))
    val move = model.computeMoveVector(StateVector(pieces, White))
    println(move)
  }
}

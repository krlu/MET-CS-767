package org.bu.met

import org.bu.met.data.DataGenerator
import org.bu.met.types._

object MainExperiment {
  def main(args: Array[String]) {
    val model = new InferenceModel
    model.train("case_specific_training_sets/old_training_data.csv")
    val pieces = DataGenerator.testCase2
    val game = new ChessGame(pieces, White)
    game.runGame(10, Some(model))
    println(game.numMoves)
    val move = model.computeMoveVector(StateVector(pieces, White))
    println(move.toReadableMove)
  }
}

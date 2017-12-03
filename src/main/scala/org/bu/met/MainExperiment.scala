package org.bu.met

import org.bu.met.data.DataGenerator
import org.bu.met.types._

object MainExperiment {
  def main(args: Array[String]) {
    val model = new InferenceModel
    model.train("case_specific_training_sets/training_data_D3F.csv")
    val pieces = DataGenerator.testCase2
    val move = model.computeMoveVector(StateVector(pieces, White))
    println(move.toReadableMove)
  }
}

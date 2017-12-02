package org.bu.met

/**
  * Created by Kenneth on 12/2/2017.
  */
object MainExperiment {
  def main(args: Array[String]) {
    val model = new InferenceModel
    model.train("training_data.csv")
    model.computeMoveVector(null)
  }
}

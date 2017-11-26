package org.bu.met

import neuroflow.application.plugin.Notation._
import neuroflow.core.Activator._
import neuroflow.core._
import neuroflow.nets.cpu.DenseNetwork._
import shapeless._

import scala.io.Source

object InferenceModel {
  type NNVector = _root_.neuroflow.core.Network.Vector[Double]
  def main(args: Array[String]) = {

    implicit val wp = WeightProvider.Double.FFN.normal {
      Map(// normal config per weight layer
        0 ->(1.1, 0.1),
        1 ->(1.1, 0.1)
      )
    }

    // training data
    val (trainingInput, trainingOutput) = setupTrainingData("training_data.csv")
    val settings = Settings[Double](
      learningRate = {
        case (_, _) => 0.001
      },
      iterations = 100000, verbose = false
    )
    val fn = Linear
    val net = Network(Input(97) :: Output(3, fn) :: HNil, settings)

    println("training....")
    val t1 = System.currentTimeMillis()
    net.train(trainingInput, trainingOutput)
    val t2 = System.currentTimeMillis()
    println(s"training completed in ${(t2 - t1)/1000.0} seconds")
    trainingInput.foreach{ input =>
      val a = net.evaluate(input)
      println(s"Input: $input")
      println(s"Output: $a")
    }
//    println("Network was: " + net)
  }

  def setupTrainingData(fileName: String): (Seq[NNVector], Seq[NNVector]) ={
    var trainingInput: Seq[NNVector] = Seq()
    var trainingOutput: Seq[NNVector] = Seq()
    val bufferedSource = Source.fromFile(fileName)
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim.toDouble)
      val input = ->(cols.dropRight(3):_*)
      val output = ->(cols(97), cols(98), cols(99))
      trainingInput = trainingInput :+ input
      trainingOutput = trainingOutput :+ output
    }
    bufferedSource.close
    (trainingInput, trainingOutput)
  }
}

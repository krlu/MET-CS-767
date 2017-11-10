package org.bu.met

import neuroflow.application.plugin.Notation._
import neuroflow.core.Activator._
import neuroflow.core._
import neuroflow.nets.cpu.DenseNetwork._
import shapeless._

object InferenceModel {
  def main(args: Array[String]) = {

    implicit val wp = WeightProvider.Double.FFN.normal {
      Map(// normal config per weight layer
        0 ->(0.1, 0.1),
        1 ->(0.1, 0.1)
      )
    }

    // training data
    val trainingInput = Seq(->(0.0, 0.0), ->(0.0, 1.0), ->(1.0, 0.0), ->(1.0, 1.0))
    val trainingOutput = Seq(->(0.0), ->(1.0), ->(1.0), ->(0.0))
    val settings = Settings[Double](
      learningRate = {
        case (_, _) => 0.3
      },
      iterations = 10000, verbose = false
    )
    val fn = Tanh
    val net = Network(Input(2) :: Dense(129,fn) :: Output(1, fn) :: HNil, settings)
    net.train(trainingInput, trainingOutput)

    val a: _root_.neuroflow.core.Network.Vector[Double] = net.evaluate(->(0.0, 0.0))
    val b = net.evaluate(->(0.0, 1.0))
    val c = net.evaluate(->(1.0, 0.0))
    val d = net.evaluate(->(1.0, 1.0))

    println(s"Input: 0.0, 0.0   Output: $a")
    println(s"Input: 0.0, 1.0   Output: $b")
    println(s"Input: 1.0, 0.0   Output: $c")
    println(s"Input: 1.0, 1.0   Output: $d")

    println("Network was: " + net)

  }
}

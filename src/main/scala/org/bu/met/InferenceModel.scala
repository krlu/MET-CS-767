package org.bu.met

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.library.atomic.continuous.Normal
import neuroflow.application.plugin.Notation._
import neuroflow.core.Activator._
import neuroflow.core._
import shapeless._
import neuroflow.nets.cpu.DenseNetwork._

object InferenceModel {
  def main(args: Array[String]) {
    val x = Normal(10,1)
    val alg = Importance(100000, x)
    alg.start()
    alg.stop()
    val results: Stream[(Double, Double)] = alg.computeDistribution(x)
    println(results.map{case (a,b) => a*b}.sum)
    alg.kill()
  }
}


object XOR {

  def main(args: Array[String]) = {
    implicit val wp = neuroflow.core.WeightProvider.Double.FFN.normal {
      Map ( // normal config per weight layer
        0 -> (0.0, 1.0),
        1 -> (0.0, 0.1)
      )
    }
    val fn = Sigmoid

    // training data
    val xs = Seq(->(0.0, 0.0), ->(0.0, 1.0), ->(1.0, 0.0), ->(1.0, 1.0))
    val ys = Seq(->(0.0), ->(1.0), ->(1.0), ->(0.0))
    val settings = Settings[Double](
      learningRate = { case (_, _) => 1.0 },
      iterations = 10000)
    val net = Network(Input(2) :: Dense(3, fn) :: Output(1, fn) :: HNil, settings)
    net.train(xs, ys)

    val a = net.evaluate(->(0.0, 0.0))
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

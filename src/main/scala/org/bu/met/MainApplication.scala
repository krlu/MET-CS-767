package org.bu.met

import neuroflow.application.plugin.Notation._
import neuroflow.application.processor.Util._
import neuroflow.core.Activator.Sigmoid
import neuroflow.core.FFN.WeightProvider._
import neuroflow.core._
import neuroflow.nets.DefaultNetwork._
import shapeless._

object MainApplication {
  def main(args: Array[String]) {
    val src = scala.io.Source.fromFile(getResourceFile("income.txt")).getLines.map(_.split(",")).flatMap(k => {
      (if (k.length > 14) Some(k(14)) else None).map {
        over50k => (k(0).toDouble, if (over50k.equals(" >50K")) 1.0 else 0.0)
      }
    }).toList

    val train = src.take(2000)
    //val test = src.drop(1000)
    val sets = Settings(learningRate = { case _ => 1E-2 }, precision = 0.001, iterations = 10000,
      regularization = None, approximation = None, specifics = None)
    val network = Network(Input(1) :: Hidden(20, Sigmoid) :: Output(1, Sigmoid) :: HNil, sets)
    val maxAge = train.map(_._1).sorted.reverse.head
    val xs = train.map(a => ->(a._1 / maxAge))
    val ys = train.map(a => ->(a._2))
    network.train(xs, ys)

    val allOver = src.filter(_._2 == 1.0)
    val ratio = allOver.size / src.size
    val mean = allOver.map(_._1).sum / allOver.size

    println(s"Mean of all $mean")
    println(s"Ratio $ratio")

    val result = Range.Double(0.0, 1.1, 0.1).map(k => (k * maxAge, network(->(k))))
    val sum = result.map(_._2.apply(0)).sum
    println("Age, earning >50K")
    result.foreach { r => println(s"${r._1}, ${r._2(0) * (1 / sum)}")}
  }
}

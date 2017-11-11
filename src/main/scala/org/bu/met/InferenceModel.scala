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
        0 ->(0.1, 0.1),
        1 ->(0.1, 0.1)
      )
    }

    // training data
    val (trainingInput, trainingOutput) = setupTrainingData("training_data.csv")
    val settings = Settings[Double](
      learningRate = {
        case (_, _) => 1.0
      },
      iterations = 10000, verbose = false
    )
    val fn = Tanh
    val net = Network(Input(97) :: Dense(97, fn) ::Output(3, fn) :: HNil, settings)

    println("training....")
    val t1 = System.currentTimeMillis()
    net.train(trainingInput, trainingOutput)
    val t2 = System.currentTimeMillis()
    println(s"training completed in ${(t2 - t1)/1000.0} seconds")
    trainingInput.foreach{ input =>
      val a = net.evaluate(input)
      println(s"Input: 0.0, 0.0   Output: $a")
    }

//    println("Network was: " + net)

  }

  def setupTrainingData(fileName: String): (Seq[NNVector], Seq[NNVector]) ={
    var trainingInput: Seq[NNVector] = Seq()
    var trainingOutput: Seq[NNVector] = Seq()
    val bufferedSource = Source.fromFile(fileName)
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim.toDouble)
      val input = ->(cols(0),cols(1),cols(2),cols(3),cols(4),cols(5),cols(6),cols(7),
        cols(8),cols(9),cols(10),cols(11),cols(12),cols(13),cols(14),cols(15),
        cols(16),cols(17),cols(18),cols(19),cols(20),cols(21),cols(22),cols(23),
        cols(24),cols(25),cols(26),cols(27),cols(28),cols(29),cols(30),cols(31),
        cols(32),cols(33),cols(34),cols(35),cols(36),cols(37),cols(38),cols(39),
        cols(40),cols(41),cols(42),cols(43),cols(44),cols(45),cols(46),cols(47),
        cols(48),cols(49),cols(50),cols(51),cols(52),cols(53),cols(54),cols(55),
        cols(56),cols(57),cols(58),cols(59),cols(60),cols(61),cols(62),cols(63),
        cols(64),cols(65),cols(66),cols(67),cols(68),cols(69),cols(70),cols(71),
        cols(72),cols(73),cols(74),cols(75),cols(76),cols(77),cols(78),cols(79),
        cols(80),cols(81),cols(82),cols(83),cols(84),cols(85),cols(86),cols(87),
        cols(88),cols(89),cols(90),cols(91),cols(92),cols(93),cols(94),cols(95),
        cols(96))
      val output = ->(cols(97), cols(98), cols(99))
      trainingInput = trainingInput :+ input
      trainingOutput = trainingOutput :+ output
    }
    bufferedSource.close
    (trainingInput, trainingOutput)
  }
}

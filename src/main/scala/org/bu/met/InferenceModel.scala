package org.bu.met

import neuroflow.application.plugin.Notation._
import neuroflow.core.Activator._
import neuroflow.core._
import neuroflow.nets.cpu.DenseNetwork._
import org.bu.met.types.{MoveVector, StateVector}
import shapeless._

import scala.io.Source

class InferenceModel {
  type NNVector = _root_.neuroflow.core.Network.Vector[Double]
  implicit val wp = WeightProvider.Double.FFN.normal {
    Map(// normal config per weight layer
      0 ->(1.1, 0.1),
      1 ->(1.1, 0.1)
    )
  }
  val settings = Settings[Double](
    learningRate = {
      case (_, _) => 0.001
    },
    iterations = 100000, verbose = false
  )
  val fn = Linear
  val net = Network(Input(97) :: Output(3, fn) :: HNil, settings)

  def train(fileName: String): Unit ={
    println("training....")
    val t1 = System.currentTimeMillis()
    val (trainingInput, trainingOutput) = setupTrainingData(fileName)
    println(trainingInput.size)
    net.train(trainingInput, trainingOutput)
    val t2 = System.currentTimeMillis()
    println(s"training completed in ${(t2 - t1)/1000.0} seconds")
  }

  def computeMoveVector(state: StateVector): MoveVector ={
    val input = ->(state.elements.map(_.toDouble):_*)
    val output: NNVector = net.evaluate(input)
    println(s"Input: $input")
    println(s"Output: ${output.data.toSeq.map{Math.round}}")
    convertToMoveVector(output)
  }

  private def convertToMoveVector(vector: NNVector): MoveVector ={
    val roundedData = vector.data.toSeq.map{Math.round}
    MoveVector(roundedData.head.toInt, roundedData(1).toInt, roundedData(2).toInt)
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

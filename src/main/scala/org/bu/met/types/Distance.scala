package org.bu.met.types


object Distance extends ((StateVector, StateVector) => Double){
  override def apply(v1: StateVector, v2: StateVector): Double = {
    require(v1.elements.size == v2.elements.size)
    val pairs: Seq[(Int, Int)] = v1.elements zip v2.elements
    pairs.map{case(x,y) => Math.sqrt(Math.pow(x,2) + Math.pow(y,2))}.sum
  }
}

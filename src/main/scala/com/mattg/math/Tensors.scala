package com.mattg.math

object Tensors {
  def zeros(vectorSize: Int): Array[Double] = new Array[Double](vectorSize)
  def logZeros(vectorSize: Int): Array[Double] = initWithValue(vectorSize, Double.NegativeInfinity)
  def ones(vectorSize: Int): Array[Double] = initWithValue(vectorSize, 1)

  def initWithValue(vectorSize: Int, value: Double): Array[Double] = {
    val array = new Array[Double](vectorSize)
    for (i <- (0 until vectorSize)) array(i) = value
    array
  }

  def smoothDistributions(distributions: Seq[Array[Double]], smoothingAmount: Double) {
    for (distribution <- distributions) {
      val denominator = distribution.sum + smoothingAmount * distribution.length
      for (i <- (0 until distribution.length)) {
        distribution(i) = (distribution(i) + smoothingAmount) / denominator
      }
    }
  }

  def print(matrix: Seq[Array[Double]]) {
    for (vector <- matrix) {
      if (vector.length < 20) {
        println(vector.toList)
      } else {
        println("List(" + vector.take(10).mkString(", ") + ", ..., " + vector.takeRight(10).mkString(", ") + ")")
      }
    }
  }
}

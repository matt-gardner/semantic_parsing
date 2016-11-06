package com.mattg.math

object Tensors {
  def ones(vectorSize: Int): Array[Double] = {
    val array = new Array[Double](vectorSize)
    for (i <- (0 until vectorSize)) array(i) = 1
    array
  }
}

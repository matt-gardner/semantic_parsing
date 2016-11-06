package com.mattg.math

object LogMath {
  val precisionThreshold = 20.0
  def logSum(logX: Double, logY: Double): Double = {
    val (min, max) = if (logX > logY) (logY, logX) else (logX, logY)
    if (min == Double.NegativeInfinity) return max
    val diff = max - min
    if (diff > precisionThreshold) {
      max
    } else {
      min + math.log(1 + math.exp(diff))
    }
  }

  def logSum(logProbs: Seq[Double]): Double = {
    logProbs.foldLeft(Double.NegativeInfinity)(logSum)
  }
}

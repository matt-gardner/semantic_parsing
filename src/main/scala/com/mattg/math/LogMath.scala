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

  def logNormalize(logDistributions: Seq[Array[Double]]) {
    logDistributions.map(logNormalize)
  }

  /**
   * Takes an unnormalized distribution of expected log counts, and produces a normalized
   * probability distribution (no longer in log space).  This is done _in place_.
   */
  def logNormalize(logDistribution: Array[Double]) {
    val totalLogProb = logSum(logDistribution)
    val totalProb = math.exp(totalLogProb)
    for (i <- (0 until logDistribution.length)) {
      logDistribution(i) = if (totalProb == Double.NegativeInfinity || totalProb == Double.NaN) {
        0
      } else {
        math.exp(logDistribution(i)) / totalProb
      }
    }
  }
}

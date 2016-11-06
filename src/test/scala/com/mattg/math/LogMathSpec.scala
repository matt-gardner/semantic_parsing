package com.mattg.math

import org.scalatest._

class LogSumSpec extends FlatSpecLike with Matchers {
  "logSum" should "correctly add two numbers in log space" in {
    LogMath.logSum(5, 5) should be(math.log(2) + 5)
    val x = 23
    val y = 124
    val logX = math.log(x)
    val logY = math.log(y)
    LogMath.logSum(logX, logY) should be(math.log(x + y))
  }

  it should "correctly add an array of numbers in log space" in {
    LogMath.logSum(Seq(5, 5, 5, 5, 5)) should be(math.log(5) + 5)
    val x = 23
    val y = 124
    val z = 12
    val logX = math.log(x)
    val logY = math.log(y)
    val logZ = math.log(z)
    LogMath.logSum(Seq(logX, logY, logZ)) should be(math.log(x + y + z))
  }
}

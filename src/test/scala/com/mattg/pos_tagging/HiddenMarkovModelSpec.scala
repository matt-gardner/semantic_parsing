package com.mattg.pos_tagging

import com.mattg.math.LogMath

import org.scalatest._

class HiddenMarkovModelSpec extends FlatSpecLike with Matchers {
  val model = new HiddenMarkovModel(5, 3)
  model.transitionProbs(0) = Array(.3, .1, .59, 0, .01)
  model.transitionProbs(1) = Array(.5, .2, .2, 0, .1)
  model.transitionProbs(2) = Array(.01, .01, .5, 0, .5)
  model.transitionProbs(3) = Array(.1, .7, .2, 0, 0)
  model.transitionProbs(4) = Array(.0, .0, .0, 0, .0)
  model.emissionProbs(0) = Array(.05, .3, .3, .3, .05)
  model.emissionProbs(1) = Array(.8, .05, .05, .05, .05)
  model.emissionProbs(2) = Array(.05, .05, .05, .05, .8)

  "forward" should "have final state probabilities sum to one over all possible sequences" in {
    // This test case taken from an example in Dan Jurafsky's textbook:
    // https://web.stanford.edu/~jurafsky/slp3/8.pdf.
    val model = new HiddenMarkovModel(3, 2)
    model.transitionProbs(0) = Array(.6, .3, 0, .1)  // state 0 is "HOT"
    model.transitionProbs(1) = Array(.4, .5, 0, .1)  // state 1 is "COLD"
    model.transitionProbs(2) = Array(.8, .2, 0, 0)  // state 2 is "start"
    model.emissionProbs(0) = Array(.2, .4, .4)
    model.emissionProbs(1) = Array(.5, .4, .1)
    val emissionSequence = Array(2, 0, 2)
    val stateLogProbs = model.forward(emissionSequence)
    stateLogProbs(0)(0) should be(math.log(.32) +- .000000001)
    stateLogProbs(0)(1) should be(math.log(.02) +- .000000001)
    stateLogProbs(1)(0) should be(math.log(.040) +- .000000001)
    stateLogProbs(1)(1) should be(math.log(.053) +- .000000001)
  }

  "decode" should "return correct sequences with a simple model" in {
    val emissionSequence = Array(0, 1, 1, 3, 2, 4)
    val expectedStates = Array(1, 0, 0, 0, 0, 2)
    model.decode(emissionSequence) should be(expectedStates)
  }
}

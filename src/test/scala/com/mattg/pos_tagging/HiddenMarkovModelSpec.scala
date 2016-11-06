package com.mattg.pos_tagging

import org.scalatest._

class HiddenMarkovModelSpec extends FlatSpecLike with Matchers {

  "decode" should "return correct sequences with a simple model" in {
    val model = new HiddenMarkovModel(5, 3)
    model.transitionProbs(0) = Array(.3, .1, .6)
    model.transitionProbs(1) = Array(.5, .2, .3)
    model.transitionProbs(2) = Array(.01, .01, .98)
    model.emissionProbs(0) = Array(.05, .3, .3, .3, .05)
    model.emissionProbs(1) = Array(.8, .05, .05, .05, .05)
    model.emissionProbs(2) = Array(.05, .05, .05, .05, .8)
    val emissionSequence = Array(0, 1, 1, 3, 2, 4)
    val expectedStates = Array(1, 0, 0, 0, 0, 2)
    model.decode(emissionSequence) should be(expectedStates)
  }
}

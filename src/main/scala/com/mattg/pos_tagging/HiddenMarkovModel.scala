package com.mattg.pos_tagging

class HiddenMarkovModel(vocabSize: Int, numStates: Int) {
  val startState = 0
  val transitionProbs: Array[Array[Double]] = new Array(numStates)
  for (i <- (0 until numStates)) transitionProbs(i) = initWeights(numStates)
  val emissionProbs: Array[Array[Double]] = new Array(numStates)
  for (i <- (0 until numStates)) emissionProbs(i) = initWeights(vocabSize)

  private def initWeights(vectorSize: Int): Array[Double] = new Array(vectorSize)

  def decode(emissionSequence: Array[Int]): Array[Int] = {
    var prevStateLogProbs = new Array[Double](numStates)
    var currentStateLogProbs = new Array[Double](numStates)

    val backPointers = new Array[Array[Int]](emissionSequence.length)
    for (i <- (0 until emissionSequence.length)) {
      backPointers(i) = new Array(numStates)
    }

    for ((token, tokenIndex) <- emissionSequence.zipWithIndex) {
      println("Previous state log probs: " + prevStateLogProbs.toList)
      currentStateLogProbs = new Array[Double](numStates)
      for (currentState <- (0 until numStates)) {
        val prevStateResults = (0 until numStates).map(prevState => {
          val prevLogProb = prevStateLogProbs(prevState)
          val transitionLogProb = math.log(transitionProbs(prevState)(currentState))
          val currentLogProb = prevLogProb + transitionLogProb
          (prevState, currentLogProb)
        })
        val (bestPrevState, logProbSoFar) = prevStateResults.maxBy(_._2)
        backPointers(tokenIndex)(currentState) = bestPrevState
        val emissionLogProb = math.log(emissionProbs(currentState)(token))
        val currentLogProb = logProbSoFar + emissionLogProb
        currentStateLogProbs(currentState) = currentLogProb
      }
      prevStateLogProbs = currentStateLogProbs
    }
    println("Final state log probs: " + currentStateLogProbs.toList)
    println("Back pointers:")
    for (i <- (0 until emissionSequence.length)) println(backPointers(i).toList)
    val (finalLogProb, bestFinalState) = currentStateLogProbs.zipWithIndex.maxBy(_._1)
    val finalStates = new Array[Int](emissionSequence.length)
    finalStates(emissionSequence.length - 1) = bestFinalState
    for (i <- (0 until emissionSequence.length - 1).reverse) {
      finalStates(i) = backPointers(i + 1)(finalStates(i + 1))
    }
    finalStates
  }

  private def ones(vectorSize: Int): Array[Double] = {
    val array = new Array[Double](vectorSize)
    for (i <- (0 until vectorSize)) array(i) = 1
    array
  }
}

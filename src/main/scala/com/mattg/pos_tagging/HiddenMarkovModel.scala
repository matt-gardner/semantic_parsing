package com.mattg.pos_tagging

import com.mattg.data.IndexedPosTagCorpus
import com.mattg.math.LogMath

class HiddenMarkovModel(vocabSize: Int, numStates: Int) {
  val startState = numStates
  val endState = numStates + 1
  val transitionProbs: Array[Array[Double]] = new Array(numStates + 2)
  for (i <- (0 until numStates + 2)) transitionProbs(i) = initWeights(numStates + 2)
  val emissionProbs: Array[Array[Double]] = new Array(numStates)
  for (i <- (0 until numStates)) emissionProbs(i) = initWeights(vocabSize)

  private def initWeights(vectorSize: Int): Array[Double] = new Array(vectorSize)

  def trainWithLabeledData(corpus: IndexedPosTagCorpus) {
    // First, count emission statistics.
    val emissionCounts: Array[Array[Int]] = new Array(numStates)
    for (i <- (0 until numStates)) emissionCounts(i) = new Array(vocabSize)
    for (instance <- corpus.instances) {
      val emissions = instance._1.wordIndices.zip(instance._2.labels.map(_.label))
      for ((wordIndex, label) <- emissions) {
        emissionCounts(label)(wordIndex) += 1
      }
    }

    // Then update emission probabilities.
    for (state <- (0 until numStates)) {
      val stateEmissions = emissionCounts(state).sum
      if (stateEmissions > 0) {
        emissionProbs(state) = emissionCounts(state).map(_.toDouble / stateEmissions)
      }
    }

    // Second, count transition statistics.
    val transitionCounts: Array[Array[Int]] = new Array(numStates + 2)
    for (i <- (0 until numStates + 2)) transitionCounts(i) = new Array(numStates + 2)
    for (posTags <- corpus.instances.map(_._2.labels)) {
      transitionCounts(startState)(posTags.head.label) += 1
      for (i <- (1 until posTags.size)) {
        transitionCounts(posTags(i - 1).label)(posTags(i).label) += 1
      }
      transitionCounts(posTags.last.label)(endState) += 1
    }

    // Finally, update transition probabilities.
    for (state <- (0 until numStates + 2)) {
      val stateTransitions = transitionCounts(state).sum
      if (stateTransitions > 0) {
        transitionProbs(state) = transitionCounts(state).map(_.toDouble / stateTransitions)
      }
    }

  }

  def decode(emissionSequence: Array[Int]): Array[Int] = {
    var prevStateLogProbs = new Array[Double](numStates)
    for (i <- (0 until numStates)) prevStateLogProbs(i) = math.log(transitionProbs(startState)(i))
    var currentStateLogProbs = new Array[Double](numStates)

    val backPointers = new Array[Array[Int]](emissionSequence.length)
    for (i <- (0 until emissionSequence.length)) {
      backPointers(i) = new Array(numStates)
    }

    for ((token, tokenIndex) <- emissionSequence.zipWithIndex) {
      currentStateLogProbs = new Array[Double](numStates)
      for (currentState <- (0 until numStates)) {
        val prevStateResults = if (tokenIndex == 0) {
          Seq((startState, prevStateLogProbs(currentState)))
        } else {
          (0 until numStates).map(prevState => {
            val prevLogProb = prevStateLogProbs(prevState)
            val transitionLogProb = math.log(transitionProbs(prevState)(currentState))
            val currentLogProb = prevLogProb + transitionLogProb
            (prevState, currentLogProb)
          })
        }
        val (bestPrevState, logProbSoFar) = prevStateResults.maxBy(_._2)
        backPointers(tokenIndex)(currentState) = bestPrevState
        val emissionLogProb = math.log(emissionProbs(currentState)(token))
        val currentLogProb = logProbSoFar + emissionLogProb
        currentStateLogProbs(currentState) = currentLogProb
      }
      prevStateLogProbs = currentStateLogProbs
    }
    for (finalState <- (0 until numStates)) {
      val endStateProb = transitionProbs(finalState)(endState)
      currentStateLogProbs(finalState) += endStateProb
    }
    val (finalLogProb, bestFinalState) = currentStateLogProbs.zipWithIndex.maxBy(_._1)
    val finalStates = new Array[Int](emissionSequence.length)
    finalStates(emissionSequence.length - 1) = bestFinalState
    for (i <- (0 until emissionSequence.length - 1).reverse) {
      finalStates(i) = backPointers(i + 1)(finalStates(i + 1))
    }
    println(s"finalStates: ${finalStates.toList}")
    finalStates
  }

  def forward(emissionSequence: Array[Int]): Array[Array[Double]] = {
    var stateLogProbs = new Array[Array[Double]](emissionSequence.length + 1)
    for (i <- (0 to emissionSequence.length)) stateLogProbs(i) = new Array[Double](numStates)

    for ((token, tokenIndex) <- emissionSequence.zipWithIndex) {
      for (currentState <- (0 until numStates)) {
        // prevStateLogProbs is the probability of getting to the current state from all possible
        // combinations of prior states.  If this is the first emission, that's just the start
        // probability of the current state (we do some funny computation in this case, to handle
        // both cases similarly in the code that follows).  If it's not the first emission, this is
        // a sum that's built with dynamic programming.
        val prevStateLogProbs = if (tokenIndex == 0) {
          val startProbs = new Array[Double](numStates)
          for (i <- (0 until numStates)) startProbs(i) = Double.NegativeInfinity
          startProbs(currentState) = math.log(transitionProbs(startState)(currentState))
          startProbs.toSeq
        } else (0 until numStates).map(prevState => {
          val prevLogProb = stateLogProbs(tokenIndex - 1)(prevState)
          val transitionLogProb = math.log(transitionProbs(prevState)(currentState))
          prevLogProb + transitionLogProb
        })
        val logProbSoFar = LogMath.logSum(prevStateLogProbs)
        val emissionLogProb = math.log(emissionProbs(currentState)(token))
        val currentLogProb = logProbSoFar + emissionLogProb
        stateLogProbs(tokenIndex)(currentState) = currentLogProb
      }
    }
    for (finalState <- (0 until numStates)) {
      val endStateProb = transitionProbs(finalState)(endState)
      stateLogProbs(emissionSequence.length)(finalState) += endStateProb
    }
    stateLogProbs
  }
}

object HiddenMarkovModel {
  import com.mattg.util.MutableConcurrentDictionary
  import com.mattg.data.PosTagCorpus

  def main(args: Array[String]) {
    val twitterPosTagDataFile = "/Users/mattg/data/twpos-data-v0.3/oct27.splits/oct27.traindev"
    val testFile = "/Users/mattg/data/twpos-data-v0.3/oct27.splits/oct27.test"
    println("Loading data")
    val trainCorpus = PosTagCorpus.loadConllData(twitterPosTagDataFile)
    val testCorpus = PosTagCorpus.loadConllData(testFile)
    println("Indexing data")
    val tokenDictionary = new MutableConcurrentDictionary()
    val tagDictionary = new MutableConcurrentDictionary()
    val indexedTrainCorpus = trainCorpus.toIndexedPosTagCorpus(tokenDictionary, tagDictionary)
    val maxTrainIndex = tokenDictionary.size
    println(s"Training token types: ${tokenDictionary.size}")
    val indexedTestCorpus = testCorpus.toIndexedPosTagCorpus(tokenDictionary, tagDictionary)
    println(s"After indexing test corpus: ${tokenDictionary.size}")
    val model = new HiddenMarkovModel(tokenDictionary.size, tagDictionary.size)
    println("Training model")
    model.trainWithLabeledData(indexedTrainCorpus)
    println("Testing model")
    val noOovInstances = indexedTestCorpus.instances.filterNot { case (sentence, posTags) => {
      sentence.wordIndices.exists(_ >= maxTrainIndex)
    }}
    println(s"Number of test instances: ${testCorpus.instances.size}")
    println(s"Filtered: ${noOovInstances.size}")
    for (instance <- noOovInstances) {
      val tokens = instance._1.wordIndices
      val actualLabels = instance._2.labels.map(_.label)
      val predictedLabels = model.decode(tokens.toArray)
      print("Words:          ")
      for (tokenIndex <- tokens) {
        val token = tokenDictionary.getString(tokenIndex)
        print(f"${token}%10s")
      }
      println()
      print("Actual tags:    ")
      for (tagIndex <- actualLabels) {
        val tag = tagDictionary.getString(tagIndex)
        print(f"${tag}%10s")
      }
      println()
      print("Predicted tags: ")
      for (tagIndex <- predictedLabels) {
        val tag = tagDictionary.getString(tagIndex)
        print(f"${tag}%10s")
      }
      println("\n")
    }
  }
}

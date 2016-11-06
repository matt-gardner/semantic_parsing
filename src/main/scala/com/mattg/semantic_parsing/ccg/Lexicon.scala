package com.mattg.semantic_parsing.ccg

import scala.collection.mutable

// A Lexicon is at its heart a mapping from words to possible categories.  For now, that's
// basically all that's in here.
class Lexicon(entries: Map[String, Set[(Category, Double)]]) {
  def getCategories(word: String) = entries(word)
}

class LexiconBuilder {
  val lexicon = new mutable.HashMap[String, mutable.HashSet[(Category, Double)]]

  // NOTE: Adding the same category to a word more than once will currently break this method,
  // leading to the category showing up twice in the category set!
  def addCategoryToWord(word: String, category: Category, weight: Double) {
    val categories = if (lexicon.contains(word)) {
      lexicon(word)
    } else {
      val tmp = new mutable.HashSet[(Category, Double)]
      lexicon(word) = tmp
      tmp
    }
    categories += Tuple2(category, weight)
  }

  def build(): Lexicon = {
    new Lexicon(lexicon.par.mapValues(_.toSet).seq.toMap)
  }

  def normalizeAndBuild(): Lexicon = {
    new Lexicon(lexicon.par.mapValues(categories => {
      val sum = categories.map(_._2).sum
      categories.map(catScore => (catScore._1, catScore._2 / sum)).toSet
    }).seq.toMap)
  }
}

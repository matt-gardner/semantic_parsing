package com.mattg.data

case class Sentence(text: String)

case class TokenizedSentence(words: Seq[String])

case class IndexedSentence(wordIndices: Seq[Int])

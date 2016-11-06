package com.mattg.data

import com.mattg.util.FileUtil
import com.mattg.util.MutableConcurrentDictionary

import scala.collection.mutable
import scala.util.Random

case class PosTagCorpus(instances: Seq[(TokenizedSentence, PosTags)]) {
  def take(num: Int, shuffle: Boolean=false): PosTagCorpus = {
    val newInstances = if (shuffle) {
      val random = new Random()
      random.shuffle(instances).take(num)
    } else {
      instances.take(num)
    }
    PosTagCorpus(newInstances)
  }

  def toIndexedPosTagCorpus(
    tokenDictionary: MutableConcurrentDictionary,
    tagDictionary: MutableConcurrentDictionary
  ): IndexedPosTagCorpus = {
    val indexedInstances = instances.par.map { case (sentence, posTags) => {
      val wordIndices = sentence.words.map(tokenDictionary.getIndex)
      val indexedSentence = IndexedSentence(wordIndices)
      val posTagIndices = posTags.labels.map(l => IndexedPosTag(tagDictionary.getIndex(l.label)))
      val indexedPosTags = IndexedPosTags(posTagIndices)
      (indexedSentence, indexedPosTags)
    }}
    IndexedPosTagCorpus(indexedInstances.seq)
  }
}

case class IndexedPosTagCorpus(instances: Seq[(IndexedSentence, IndexedPosTags)]) {
  def take(num: Int, shuffle: Boolean=false): IndexedPosTagCorpus = {
    val newInstances = if (shuffle) {
      val random = new Random()
      random.shuffle(instances).take(num)
    } else {
      instances.take(num)
    }
    IndexedPosTagCorpus(newInstances)
  }
}

object PosTagCorpus {
  def loadConllData(filename: String, fileUtil: FileUtil=new FileUtil): PosTagCorpus = {
    val wordsAndTags = new mutable.ArrayBuffer[(String, PosTag)]
    val sentences = new mutable.ArrayBuffer[(TokenizedSentence, PosTags)]
    for (line <- fileUtil.readLinesFromFile(filename)) {
      if (line.isEmpty) {
        val sentence = TokenizedSentence(wordsAndTags.map(_._1))
        val posTags = PosTags(wordsAndTags.map(_._2))
        sentences += Tuple2(sentence, posTags)
        wordsAndTags.clear()
      } else {
        val fields = line.split("\t")
        val word = fields(0)
        val tag = fields(1)
        wordsAndTags += Tuple2(word, PosTag(tag))
      }
    }
    PosTagCorpus(sentences)
  }
}

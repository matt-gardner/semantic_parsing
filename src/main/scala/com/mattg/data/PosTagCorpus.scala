package com.mattg.data

import com.mattg.util.FileUtil

import scala.collection.mutable

case class PosTagCorpus(instances: Seq[(TokenizedSentence, PosTags)])

object PosTagCorpus {
  def loadConllData(filename: String, fileUtil: FileUtil=new FileUtil): PosTagCorpus = {
    val wordsAndTags = new mutable.ArrayBuffer[(String, PosTag)]
    val sentences = new mutable.ArrayBuffer[(TokenizedSentence, PosTags)]
    for (line <- fileUtil.readLinesFromFile(filename)) {
      if (line.isEmpty) {
        val sentence = TokenizedSentence(wordsAndTags.map(_._1))
        val posTags = PosTags(wordsAndTags.map(_._2))
        sentences += Tuple2(sentence, posTags)
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

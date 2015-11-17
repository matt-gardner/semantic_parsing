package edu.cmu.gardner.semantic_parsing.ccg

import edu.cmu.ml.rtw.users.matt.util.FileUtil
import edu.cmu.ml.rtw.users.matt.util.JsonHelper

import org.json4s._

import scala.collection.JavaConverters._

class CcgParser(params: JValue, fileUtil: FileUtil = new FileUtil) {
  val lexicon = loadLexicon()

  def loadLexicon(): Lexicon = {
    val lexiconPath = JsonHelper.extractWithDefault(params, "lexicon file", null:String)
    val builder = new LexiconBuilder
    if (lexiconPath != null) {
      for (line <-fileUtil.readLinesFromFile(lexiconPath).asScala) {
        val fields = line.split(" +")
        val word = fields(0)
        val category = fields(1)
        val probability = fields(4).toDouble
        builder.addCategoryToWord(word, Category(category), probability)
      }
    }
    builder.build()
  }
}

object CcgParser {
  def main(args: Array[String]) {
    println("Hello world!")
  }
}

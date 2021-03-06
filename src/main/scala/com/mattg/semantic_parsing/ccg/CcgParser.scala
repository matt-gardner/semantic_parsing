package com.mattg.semantic_parsing.ccg

import com.mattg.util.FileUtil
import com.mattg.util.JsonHelper

import org.json4s._

import scala.collection.JavaConverters._

class CcgParser(params: JValue, fileUtil: FileUtil = new FileUtil) {
  val lexicon = loadLexicon()

  def loadLexicon(): Lexicon = {
    val lexiconPath = JsonHelper.extractWithDefault(params, "lexicon file", null:String)
    val builder = new LexiconBuilder
    if (lexiconPath != null) {
      for (line <-fileUtil.readLinesFromFile(lexiconPath)) {
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
  def NOT_main(args: Array[String]) {
    println("Hello world!")
  }
}

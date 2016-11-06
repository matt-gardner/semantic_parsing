package com.mattg.semantic_parsing.ccg

import org.scalatest._

import com.mattg.util.FakeFileUtil

import org.json4s._
import org.json4s.JsonDSL._

class CcgParserSpec extends FlatSpecLike with Matchers {
  val fileUtil = new FakeFileUtil
  val lexiconFile = "/lexicon/file"
  val lexiconFileContents =
    "cat    N    7   0.01  0.7\n" +
    "cat    (S[dcl]\\NP)/NP  3  0.1  0.3\n" +
    "dog  N      3   0.02   1.0\n"
  fileUtil.addFileToBeRead(lexiconFile, lexiconFileContents)

  "loadLexicon" should "load a simple lexicon file" in {
    val params: JValue = ("lexicon file" -> lexiconFile)
    val parser = new CcgParser(params, fileUtil)
    parser.lexicon.getCategories("cat") should contain((Category("N"), 0.7))
    parser.lexicon.getCategories("cat") should contain((Category("(S[dcl]\\NP)/NP"), 0.3))
    parser.lexicon.getCategories("dog") should contain((Category("N"), 1.0))
  }
}

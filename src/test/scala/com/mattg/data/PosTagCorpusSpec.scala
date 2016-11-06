package com.mattg.data

import com.mattg.util.FakeFileUtil

import org.scalatest._

class PosTagCorpusSpec extends FlatSpecLike with Matchers {
  val conllText = """I	O
    |won't	V
    |win	V
    |.	,
    |
    |RT	~
    |@DjBlack_Pearl	@
    |?????	,
    |
    |Wednesday	^
    |nice	A
    |day	N
    |:)	E
    |
    |""".stripMargin('|')
  val conllFile = "/conll_file"
  val fileUtil = new FakeFileUtil
  fileUtil.addFileToBeRead(conllFile, conllText)

  "loadConllData" should "correctly read a file" in {
    val corpus = PosTagCorpus.loadConllData(conllFile, fileUtil)
    corpus.instances.size should be(3)
    corpus.instances(0)._1.words should be(Seq("I", "won't", "win", "."))
    corpus.instances(0)._2.labels should be(Seq(PosTag("O"), PosTag("V"), PosTag("V"), PosTag(",")))
    corpus.instances(1)._1.words should be(Seq("RT", "@DjBlack_Pearl", "?????"))
    corpus.instances(1)._2.labels should be(Seq(PosTag("~"), PosTag("@"), PosTag(",")))
    corpus.instances(2)._1.words should be(Seq("Wednesday", "nice", "day", ":)"))
    corpus.instances(2)._2.labels should be(Seq(PosTag("^"), PosTag("A"), PosTag("N"), PosTag("E")))
  }
}

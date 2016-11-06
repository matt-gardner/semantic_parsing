package com.mattg.data

case class IndexedPosTag(label: Int)
case class IndexedPosTags(labels: Seq[IndexedPosTag])
case class PosTag(label: String)
case class PosTags(labels: Seq[PosTag])

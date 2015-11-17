package edu.cmu.gardner.semantic_parsing.ccg

// A Category in CCG is a combination of syntactic information and semantic information.  As a
// first pass, I'm just going to include syntactic information (things like N/N).
case class Category(name: String) {

  def canCombineLeft(other: Category): Boolean = {
    false
  }

  def combineLeft(other: Category): Category = {
    this
  }

  def canCombineRight(other: Category): Boolean = {
    false
  }

  def combineRight(other: Category): Category = {
    this
  }
}


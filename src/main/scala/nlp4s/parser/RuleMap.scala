package nlp4s.parser

import nlp4s.base.WordTag
import scala.collection.mutable.MultiDict

case class RuleMap(entries: MultiDict[String, RuleMap.Entry]) {
  def lookup(word: String): List[RuleMap.Entry] =
    entries.get(word).toList
}

object RuleMap {
  case class Entry(
    wordTags: List[WordTag],
    linkRule: LinkRule
  )

  def newBuilder = new Builder

  class Builder {
    val entries = MultiDict.empty[String, Entry]

    def add(word: String, wordTags: List[WordTag], linkRules: List[LinkRule.NormalForm]): Unit = {
      for {
        rule <- linkRules
        item <- rule.disjunction
      } add(word, wordTags, item)
    }

    def add(word: String, wordTags: List[WordTag], linkRule: LinkRule.NormalForm): Unit = {
      linkRule.disjunction.foreach { add(word, wordTags, _) }
    }

    def add(word: String, wordTags: List[WordTag], linkRule: LinkRule): Unit = {
      entries.addOne(word -> Entry(wordTags, linkRule))
    }

    def result(): RuleMap = RuleMap(entries)
  }
}

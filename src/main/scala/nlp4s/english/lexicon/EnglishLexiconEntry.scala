package nlp4s.english.lexicon

import nlp4s.base.WordTag
import nlp4s.parser.LinkRule

trait EnglishLexiconEntry {
  def linkRules: List[(String, LinkRule.NormalForm)] = 
    wordEntries.map { entry => entry.word -> entry.linkRule }

  def words: List[String] = 
    wordEntries.map { _.word } .distinct

  def wordTags: List[(String, List[WordTag])] = 
    wordEntries.map { entry => entry.word -> entry.tags }

  val wordEntries: List[EnglishLexiconEntry.WordEntry]
}

object EnglishLexiconEntry {
  case class WordEntry(
    word: String,
    tags: List[WordTag],
    linkRule: LinkRule.NormalForm,
  )
}

package nlp4s.english

import nlp4s.english.lexicon._
import nlp4s.parser.RuleMap
import nlp4s.tokenizer.TokenLexicon

class Lexicon(
  val tokenLexicon: TokenLexicon,
  val ruleMap: RuleMap,
  val wordBook: WordBook,
) 

object Lexicon {
  def newBuilder(ignoreCase: Boolean) = new Builder(ignoreCase)

  class Builder(ignoreCase: Boolean) {
    lazy val entries = List.newBuilder[EnglishLexiconEntry]

    def addEntries(es: Iterable[EnglishLexiconEntry]): Builder = {
      es.foreach { entries += _ }

      this
    }

    def result(): Lexicon = {
      val allEntries = entries.result()
      val tokenLexiconBuilder = TokenLexicon.newBuilder(ignoreCase)
      val ruleMapBuilder = RuleMap.newBuilder(ignoreCase)
      val wordBookBuilder = WordBook.newBuilder

      for {
        e <- allEntries 
        w <- e.words
      } tokenLexiconBuilder.add(w)

      for {
        e <- allEntries
        w <- e.wordEntries
      } ruleMapBuilder.add(w.word, w.tags, w.linkRule)

      allEntries.foreach { entry => wordBookBuilder.addEntry(entry.label, entry) }

      new Lexicon(tokenLexiconBuilder.result(), ruleMapBuilder.result(), wordBookBuilder.result())
    }
  }
}


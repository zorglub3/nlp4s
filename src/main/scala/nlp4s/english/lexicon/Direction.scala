package nlp4s.english.lexicon

import nlp4s.parser.LinkRuleSyntax

case class Direction(word: String) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLinkTags._
  import EnglishLexiconEntry.WordEntry

  val label = word

  val wordEntries =
    List(
      WordEntry(
        word,
        List(EnglishWordTags.Label(label), EnglishWordTags.Direction, EnglishWordTags.Adverb),
        l(A),
      ),
    )
}

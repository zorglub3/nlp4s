package nlp4s.english.lexicon

import nlp4s.parser.LinkRuleSyntax

case class Negation() extends EnglishLexiconEntry {
  import EnglishLinkTags._
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val wordEntries =
    List(
      WordEntry(
        "not",
        List(EnglishWordTags.Negation),
        l(N) | r(N),
      )
    )
}

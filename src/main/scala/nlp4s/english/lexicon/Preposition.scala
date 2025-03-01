package nlp4s.english.lexicon

import nlp4s.parser.LinkRuleSyntax

case class Preposition(word: String) extends EnglishLexiconEntry {
  import EnglishLinkTags._
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val wordEntries =
    List(
      WordEntry(
        word,
        List(EnglishWordTags.Preposition),
        (l(P) & r(R) & opt(r(P))),
      ),
    )
}

package nlp4s.english.lexicon

import nlp4s.parser.LinkRuleSyntax

case class SimpleAdjective(
  absolute: String,
) extends EnglishLexiconEntry {
  import EnglishLinkTags._
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val label = absolute

  val wordEntries =
    List(
      WordEntry( 
        absolute, 
        List(EnglishWordTags.Adjective, EnglishWordTags.Absolute, EnglishWordTags.AdjectiveRoot(absolute)),
        (l(B) | (opt(l(J)) & r(J)))
      ),
    )
}

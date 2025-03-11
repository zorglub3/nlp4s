package nlp4s.english.lexicon

import nlp4s.parser.LinkRuleSyntax

case class Adjective(
  absolute: String,
  comparative: String,
  superlative: String,
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry
  import EnglishLinkTags._

  val label = absolute

  val wordEntries =
    List(
      WordEntry( 
        absolute, 
        List(EnglishWordTags.Adjective, EnglishWordTags.Absolute, EnglishWordTags.AdjectiveRoot(absolute)),
        (opt(l(C)) & l(B) | (opt(l(J)) & opt(l(C)) & r(J)))
      ),
      WordEntry(
        superlative,
        List(EnglishWordTags.Adjective, EnglishWordTags.Superlative, EnglishWordTags.AdjectiveRoot(absolute)),
        (l(B) | (opt(l(J)) & r(J)))
      ),
      // TODO comparative form
    )
}

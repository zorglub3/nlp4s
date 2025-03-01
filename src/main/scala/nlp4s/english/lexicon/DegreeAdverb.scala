package nlp4s.english.lexicon

import nlp4s.parser.LinkRuleSyntax

case class DegreeAdverb(
  form: String,
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry
  import EnglishLinkTags._

  val wordEntries =
    List(
      WordEntry(
        form,
        List(EnglishWordTags.DegreeAdverb),
        r(C)
      )
  )
}

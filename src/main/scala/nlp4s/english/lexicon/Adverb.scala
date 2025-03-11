package nlp4s.english.lexicon

import nlp4s.parser.LinkRuleSyntax

case class Adverb(
  word: String,
  adjectiveRoot: Option[String] = None,
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry
  import EnglishLinkTags._

  val root: Option[String] = 
    adjectiveRoot orElse Option.when(word.endsWith("ly"))(word.take(word.length - 2))

  val label = root.getOrElse(word)

  val wordEntries =
    List(
      WordEntry(
        word,
        EnglishWordTags.Adverb :: root.map(EnglishWordTags.AdjectiveRoot.apply _).toList, 
        r(A) | l(A),
      )
    )
}

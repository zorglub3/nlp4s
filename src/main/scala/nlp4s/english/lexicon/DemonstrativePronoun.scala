package nlp4s.english.lexicon

import nlp4s.base.{Casus => BaseCasus}
import nlp4s.parser.LinkRuleSyntax

case class DemonstrativePronoun(
  singular: String,
  plural: String,
  isFar: Boolean
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLinkTags._
  import EnglishWordTags._
  import EnglishLexiconEntry.WordEntry

  def nominativeSingular = r(Ss) | l(Sqs)
  def nominativePlural = r(Spp) | l(Sqpp)
  def accusative = l(O) | l(R)

  val wordEntries =
    List(
      WordEntry(
        singular,
        List(Pronoun, Demonstrative, Singular, Casus(BaseCasus.Nominative)),
        nominativeSingular,
      ),
      WordEntry(
        singular,
        List(Pronoun, Demonstrative, Singular, Casus(BaseCasus.Accusative)),
        accusative,
      ),
      WordEntry(
        plural,
        List(Pronoun, Demonstrative, Plural, Casus(BaseCasus.Nominative)),
        nominativePlural,
      ),
      WordEntry(
        plural,
        List(Pronoun, Demonstrative, Plural, Casus(BaseCasus.Accusative)),
        accusative,
      )
    )
}


package nlp4s.english.lexicon

import nlp4s.base.{Gender => BaseGender}
import nlp4s.base.{Person => BasePerson}
import nlp4s.parser.LinkRuleSyntax

case class ReflexivePronoun(
  accusative: String,
  person: BasePerson,
  isPlural: Boolean,
  genderOpt: Option[BaseGender],
) extends EnglishLexiconEntry {
  import EnglishLinkTags._
  import EnglishWordTags._
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val label = accusative

  val gender = genderOpt.map(EnglishWordTags.Gender.apply _).toList
  
  val tags = gender ++ List(Pronoun, Reflexive, Person(person)) :+ (if(isPlural) Plural else Singular)

  val wordEntries =
    List(
      WordEntry(
        accusative,
        tags,
        l(O) | l(R) | l(A),
      )
    )
}

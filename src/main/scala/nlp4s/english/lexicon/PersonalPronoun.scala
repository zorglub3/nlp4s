package nlp4s.english.lexicon

import nlp4s.base.{Casus => BaseCasus}
import nlp4s.base.{Gender => BaseGender}
import nlp4s.base.{Person => BasePerson}
import nlp4s.parser.LinkRuleSyntax

case class PersonalPronoun(
  nominative: String,
  accusative: String,
  possessive: String, // as in "the table is mine" - not "this is my table"
  person: BasePerson,
  isPlural: Boolean,
  genderOpt: Option[BaseGender],
) extends EnglishLexiconEntry {
  import EnglishLinkTags._
  import EnglishWordTags._
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  def nominativeRule = 
    if(isPlural) {
      (r(Spp) | l(Sqpp))
    } else { 
      person match {
        case BasePerson.First  => r(Spi) | l(Sqpi)
        case BasePerson.Second => r(Spp) | l(Sqpp)
        case BasePerson.Third  => r(Ss)  | l(Sqs)
      }
    }

  def gender = genderOpt.map(EnglishWordTags.Gender.apply _).toList

  def baseTags = gender ++ List(Pronoun, Person(person)) :+ (if(isPlural) Plural else Singular)

  val wordEntries =
    List(
      WordEntry(
        nominative,
        Casus(BaseCasus.Nominative) :: baseTags,
        nominativeRule,
      ),
      WordEntry(
        accusative,
        Casus(BaseCasus.Accusative) :: baseTags,
        l(O) | l(R),        
      ),
      WordEntry(
        possessive,
        Casus(BaseCasus.Possessive) :: baseTags,
        l(B) | l(R),
      ),
    )
}

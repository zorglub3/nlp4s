package nlp4s.english.lexicon

import nlp4s.base.WordTag
import nlp4s.base.{Casus => BaseCasus}
import nlp4s.base.{Gender => BaseGender}
import nlp4s.base.{Person => BasePerson}
import nlp4s.base.{Tense => BaseTense}

object EnglishWordTags {
  // word classes
  case object Noun extends WordTag("noun")
  case object Verb extends WordTag("verb")
  case object Adjective extends WordTag("adjective")
  case object Adverb extends WordTag("adverb")
  case object Preposition extends WordTag("preposition")
  case object Pronoun extends WordTag("pronoun")
  case object Determiner extends WordTag("determiner")
  case object Question extends WordTag("question")
  case object Wall extends WordTag("wall")
  case object ProperNoun extends WordTag("propernoun")
  case object Negation extends WordTag("negation")
  case object Direction extends WordTag("direction")
  case object DegreeAdverb extends WordTag("degreeadverb")

  // pronoun specific
  case class Person(person: BasePerson) extends WordTag("pronounperson")
  case object Demonstrative extends WordTag("demonstrative")
  case class Gender(gender: BaseGender) extends WordTag("gender")
  case object Reflexive extends WordTag("reflexive")
  
  // verb
  case object Transitive extends WordTag("transitive")
  case object Intransitive extends WordTag("intransitive")
  case object LinkVerb extends WordTag("linkverb")
  case object HelpVerb extends WordTag("helpverb")
  case object ToBe extends WordTag("tobe")
  case object ToHave extends WordTag("tohave")

  // case
  case class Casus(casus: BaseCasus) extends WordTag("casus")

  // number
  case object Singular extends WordTag("singular")
  case object Plural extends WordTag("plural")

  // nouns
  case class NounRoot(root: String) extends WordTag("nounroot")
  case object CountNoun extends WordTag("countnoun")
  case object MassNoun extends WordTag("massnoun")

  // tense
  case object RootForm extends WordTag("rootform")
  case class WordTense(tense: BaseTense) extends WordTag("tense")
  case class VerbRoot(root: String) extends WordTag("verbroot")

  // adjective
  case class AdjectiveRoot(root: String) extends WordTag("adjectiveroot")
  case object Absolute extends WordTag("absolute")
  case object Comparative extends WordTag("comparative")
  case object Superlative extends WordTag("superlative")
}

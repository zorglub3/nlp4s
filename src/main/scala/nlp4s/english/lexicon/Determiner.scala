package nlp4s.english.lexicon

import nlp4s.base.{Casus => BaseCasus}
import nlp4s.base.{Person => BasePerson}
import nlp4s.parser.LinkRuleSyntax

import EnglishLinkTags._

case class Determiner(word: String, isPlural: Boolean) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val label = word

  val wordEntries = 
    List(
      WordEntry(
        word,
        List(EnglishWordTags.Determiner),
        if(isPlural) { r(Dp) } else { r(Ds) },
      )
    )
}

case class PossessiveDeterminer(word: String, person: BasePerson, isPlural: Boolean) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry
  import EnglishWordTags._

  val label = word

  val wordEntries = 
    List(
      WordEntry(
        word,
        List(
          // EnglishWordTags.Determiner, 
          Pronoun, 
          Person(person), if(isPlural) { Plural } else { Singular}, 
          Casus(BaseCasus.Possessive),
        ),
        r(D),
      )      
    )
}

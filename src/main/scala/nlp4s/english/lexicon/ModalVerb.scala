package nlp4s.english.lexicon

import nlp4s.base.{Tense => BaseTense}
import nlp4s.parser.LinkRuleSyntax

case class ModalVerb(
  word: String,
  negate: Boolean = false,
  labelWord: Option[String] = None,
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLinkTags._
  import EnglishLexiconEntry.WordEntry
  import EnglishWordTags.{Verb, WordTense, VerbRoot, Singular, Plural, NegatedVerb}

  val label = labelWord.getOrElse(word) 

  val baseTags = locally {
    val l = List(
      EnglishWordTags.Label(label), 
      EnglishWordTags.HelpVerb, 
      Verb, 
      WordTense(BaseTense.Present), 
      VerbRoot(word))

    if(negate) {
      NegatedVerb :: l
    } else {
      l
    }
  }
      
  val wordEntries = 
    List(
      WordEntry(
        word,
        Singular :: baseTags,
        opt(l(Qw)) & opt(r(N)) & r(Hs),
      ),
      WordEntry(
        word,
        Plural :: baseTags,
        opt(l(Qw)) & opt(r(N)) & r(Hp),
      ),
    )
}

package nlp4s.english.lexicon

import nlp4s.base.Constants
import nlp4s.parser.LinkRuleSyntax

case object Wall extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry
  import EnglishLinkTags._

  val label = "wall"

  val wordEntries = 
    List(
      WordEntry(
        Constants.LeftWall,
        List(EnglishWordTags.Wall),
        opt(r(W)),
      )
    )
}


package nlp4s.english.lexicon

import nlp4s.parser.LinkRuleSyntax

// your regular count nouns such as chair, rock or glass
case class MassNoun(
  singular: String,
  singularPossessive: String,
) extends EnglishLexiconEntry {
  import EnglishWordTags._
  import EnglishLinkTags._
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val label = singular

  val singularNoun = opt(l(J)) & opt(l(Ds)) & opt(r(P))

  val singularNounVerb =
    l(Sqs) | r(Ss) | l(O)
    
  val wordEntries = 
    List(
      WordEntry(
        singular,
        List(EnglishWordTags.Noun, Singular, NounRoot(singular), EnglishWordTags.MassNoun),
        ((singularNoun & singularNounVerb) | (singularNoun & l(R)) | (singularNoun & l(W))),
      ),
      WordEntry(
        singularPossessive,
        List(EnglishWordTags.Noun, Singular, NounRoot(singular), EnglishWordTags.MassNoun),
        singularNoun & r(D),
      ),
    )
}

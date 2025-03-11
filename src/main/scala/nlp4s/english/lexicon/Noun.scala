package nlp4s.english.lexicon

import nlp4s.parser.LinkRuleSyntax

// your regular count nouns such as chair, rock or glass
case class Noun(
  singular: String,
  plural: String,
  singularPossessive: String,
  pluralPossessive: String,
) extends EnglishLexiconEntry {
  import EnglishLinkTags._
  import EnglishWordTags._
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val label = singular

  val singularNoun = opt(l(J)) & l(Ds) & opt(r(Tr)) & opt(r(P))
  val pluralNoun = opt(l(J)) & opt(l(Dp)) & opt(r(Tr)) & opt(r(P))

  val singularNounVerb =
    l(Sqs) | r(Ss) | l(O)
  val pluralNounVerb =
    l(Sqp) | r(Sp) | l(O)
    
  val wordEntries = 
    List(
      WordEntry(
        singular,
        List(EnglishWordTags.Noun, Singular, NounRoot(singular), EnglishWordTags.CountNoun),
        ((singularNoun & singularNounVerb) | (singularNoun & l(R)) | (singularNoun & l(W))),
      ),
      WordEntry(
        plural,
        List(EnglishWordTags.Noun, Plural, NounRoot(singular), EnglishWordTags.CountNoun),
        ((pluralNoun & pluralNounVerb) | (pluralNoun & l(R)) | (pluralNoun & l(W))),
      ),
      WordEntry(
        singularPossessive,
        List(EnglishWordTags.Noun, Singular, NounRoot(singular), EnglishWordTags.CountNoun),
        singularNoun & r(D),
      ),
      WordEntry(
        pluralPossessive,
        List(EnglishWordTags.Noun, Plural, NounRoot(singular), EnglishWordTags.CountNoun),
        pluralNoun & r(D),
      ),
    )
}

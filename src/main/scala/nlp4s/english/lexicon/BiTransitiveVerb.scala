package nlp4s.english.lexicon

import nlp4s.base.{Tense => BaseTense}
import nlp4s.parser.LinkRuleSyntax

case class BiTransitiveVerb(
  root: String,
  presentSingular: String,
  presentPlural: String,
  presentParticiple: String,
  past: String,
  pastParticiple: String
) extends EnglishLexiconEntry {
  import EnglishLinkTags._
  import EnglishWordTags._
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val label = root

  val activeVerbWithObject = opt(l(A)) & r(O) & r(O) & opt(r(A)) & opt(r(P)) & opt(r(A))

  val wordEntries =
    List(
      WordEntry(
        root,
        List(Verb, RootForm, Transitive),
        ((opt(l(Hr)) & l(W) & activeVerbWithObject) | 
         (opt(l(N)) & l(Ss) & l(Hs) & activeVerbWithObject) | 
         (opt(l(N)) & l(Sp) & l(Hp) & activeVerbWithObject) | 
         (l(Hp) & l(Sp) & activeVerbWithObject) | 
         (l(Hs) & l(Ss) & activeVerbWithObject) |
         (l(Hs) & l(Qs) & activeVerbWithObject))
      ),
      WordEntry(
        presentSingular,
        List(Verb, VerbRoot(root), Singular, Transitive, WordTense(BaseTense.Present)),
        (opt(l(A)) & l(Ss) & activeVerbWithObject),
      ),
      WordEntry(
        presentPlural,
        List(Verb, VerbRoot(root), Plural, Transitive, WordTense(BaseTense.Present)),
        (opt(l(A)) & l(Sp) & activeVerbWithObject),
      ),
      WordEntry(
        past,
        List(Verb, VerbRoot(root), WordTense(BaseTense.Past), Transitive),
        (opt(l(A)) & l(S) & activeVerbWithObject),
      ),
      WordEntry(
        presentParticiple,
        List(Verb, VerbRoot(root), WordTense(BaseTense.PresentParticiple), Transitive),
        (l(Tr) & r(O)),
      ),
      WordEntry(
        pastParticiple,
        List(Verb, VerbRoot(root), WordTense(BaseTense.PastParticiple), Transitive),
        (l(Ta) & activeVerbWithObject) | (opt(l(J)) & r(J)),
      ),
    )
}

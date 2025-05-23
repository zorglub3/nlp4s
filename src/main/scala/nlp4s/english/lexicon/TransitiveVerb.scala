package nlp4s.english.lexicon

import nlp4s.base.{Tense => BaseTense}
import nlp4s.parser.LinkRuleSyntax

case class TransitiveVerb(
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

  val activeVerbWithObject = opt(l(A)) & r(O) & opt(r(A)) & opt(r(P)) & opt(r(A))

  val wordEntries =
    List(
      WordEntry(
        root,
        List(Label(label), VerbRoot(root), Verb, RootForm, Transitive),
        ((opt(l(A)) & opt(l(Hr)) & l(W) & activeVerbWithObject) | 
         (opt(l(N)) & l(Ss) & l(Hs) & activeVerbWithObject) | 
         (opt(l(N)) & l(Sp) & l(Hp) & activeVerbWithObject) | 
         (l(Hp) & l(Sp) & activeVerbWithObject) | 
         (l(Hs) & l(Ss) & activeVerbWithObject) |
         (l(Hs) & l(Qs) & activeVerbWithObject)
        ),
      ),
      WordEntry(
        presentSingular,
        List(Label(label), Verb, VerbRoot(root), Singular, Transitive, WordTense(BaseTense.Present)),
        (opt(l(A)) & l(Ss) & activeVerbWithObject) | l(Qs) & activeVerbWithObject,
      ),
      WordEntry(
        presentPlural,
        List(Label(label), Verb, VerbRoot(root), Plural, Transitive, WordTense(BaseTense.Present)),
        (opt(l(A)) & l(Sp) & activeVerbWithObject),
      ),
      WordEntry(
        past,
        List(Label(label), Verb, VerbRoot(root), WordTense(BaseTense.Past), Transitive),
        (opt(l(A)) & l(S) & activeVerbWithObject) | l(Qs) & activeVerbWithObject,
      ),
      WordEntry(
        presentParticiple,
        List(Label(label), Verb, VerbRoot(root), WordTense(BaseTense.PresentParticiple), Transitive),
        (opt(l(A)) & l(Tr) & r(O)),
      ),
      WordEntry(
        pastParticiple,
        List(Label(label), Verb, VerbRoot(root), WordTense(BaseTense.PastParticiple), Transitive),
        (l(Ta) & activeVerbWithObject) | (opt(l(J)) & r(J)),
      ),
    )
}

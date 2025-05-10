package nlp4s.english.lexicon

import nlp4s.base.{Tense => BaseTense}
import nlp4s.parser.LinkRuleSyntax

case class IntransitiveVerb(
  root: String,
  presentSingular: String,
  presentPlural: String,
  presentParticiple: String,
  past: String,
  pastParticiple: String
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLinkTags._
  import EnglishWordTags._
  import EnglishLexiconEntry.WordEntry

  val label = root

  val activeVerb = opt(l(A)) & opt(r(A)) & opt(r(P)) & opt(r(A)) 

  val wordEntries =
    List(
      WordEntry(
        root,
        List(Label(label), Intransitive, Verb, RootForm, VerbRoot(root)),
        ((opt(l(A)) & opt(l(Hr)) & l(W) & activeVerb) | 
         (l(Hs) & l(Ss) & activeVerb) | 
         (l(Hs) & l(Qs) & activeVerb) |
         (l(Hp) & l(Sp) & activeVerb) | 
         (opt(l(N)) & l(Sp) & l(Hp) & activeVerb) | 
         (opt(l(N)) & l(Ss) & l(Hs) & activeVerb)
        ),
      ),
      WordEntry(
        presentSingular,
        List(Label(label), Intransitive, Verb, WordTense(BaseTense.Present), Singular, VerbRoot(root)),
        opt(l(A)) & l(Ss) & activeVerb | l(Qs) & activeVerb,
      ),
      WordEntry(
        presentPlural,
        List(Label(label), Intransitive, Verb, WordTense(BaseTense.Present), Plural, VerbRoot(root)),
        (opt(l(A)) & l(Sp) & activeVerb),
      ),
      WordEntry(
        past,
        List(Label(label), Intransitive, Verb, WordTense(BaseTense.Past), VerbRoot(root)),
        opt(l(A)) & l(S) & activeVerb | l(Qs) & activeVerb,
      ),
      WordEntry(
        presentParticiple,
        List(Label(label), Intransitive, Verb, WordTense(BaseTense.PresentParticiple), VerbRoot(root)),
        (l(Tr) & activeVerb) | (opt(l(J)) & activeVerb & r(J)),
      ),
      WordEntry(
        pastParticiple,
        List(Label(label), Intransitive, Verb, WordTense(BaseTense.PastParticiple), VerbRoot(root)),
        (l(Ta) & activeVerb), 
      )
    )
}

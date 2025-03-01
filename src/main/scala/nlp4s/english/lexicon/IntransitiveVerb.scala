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

  val activeVerb = opt(l(A)) & opt(r(A)) & opt(r(E)) & opt(r(P)) & opt(r(A)) 

  val wordEntries =
    List(
      WordEntry(
        root,
        List(Intransitive, Verb, RootForm, VerbRoot(root)),
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
        List(Intransitive, Verb, Tense(BaseTense.Present), Singular, VerbRoot(root)),
        opt(l(A)) & l(Ss) & activeVerb | l(Qs) & activeVerb,
      ),
      WordEntry(
        presentPlural,
        List(Intransitive, Verb, Tense(BaseTense.Present), Plural, VerbRoot(root)),
        (opt(l(A)) & l(Sp) & activeVerb),
      ),
      WordEntry(
        past,
        List(Intransitive, Verb, Tense(BaseTense.Past), VerbRoot(root)),
        opt(l(A)) & l(S) & activeVerb | l(Qs) & activeVerb,
      ),
      WordEntry(
        presentParticiple,
        List(Intransitive, Verb, Tense(BaseTense.PresentParticiple), VerbRoot(root)),
        (l(Tr) & activeVerb) | (opt(l(J)) & activeVerb & r(J)),
      ),
      WordEntry(
        pastParticiple,
        List(Intransitive, Verb, Tense(BaseTense.PastParticiple), VerbRoot(root)),
        (l(Ta) & activeVerb), 
      )
    )
}

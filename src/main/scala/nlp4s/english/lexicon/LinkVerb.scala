package nlp4s.english.lexicon

import nlp4s.base.{Tense => BaseTense}
import nlp4s.parser.LinkRuleSyntax

case class LinkVerb(
  root: String,
  presentSingular: String,
  presentPlural: String,
  presentParticiple: String,
  past: String,
  pastParticiple: String,
) extends EnglishLexiconEntry {
  import EnglishLinkTags._
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry
  import EnglishWordTags._

  val activeVerbWithPredicate =
    opt(l(A)) & (r(P) | r(B))

  val wordEntries =
    List(
      WordEntry(
        root,
        List(Verb, EnglishWordTags.LinkVerb, RootForm, VerbRoot(root)),
        ((opt(l(Hr)) & l(W) & activeVerbWithPredicate) | 
         (l(Ss) & l(Hs) & activeVerbWithPredicate) | 
         (l(Sp) & l(Hp) & activeVerbWithPredicate) | 
         (l(Hp) & l(Sp) & activeVerbWithPredicate) | 
         (l(Hs) & l(Ss) & activeVerbWithPredicate)), 
      ),
      WordEntry(
        presentSingular,
        List(Verb, EnglishWordTags.LinkVerb, VerbRoot(root), WordTense(BaseTense.Present)),
        (opt(l(A)) & l(Ss) & activeVerbWithPredicate),
      ),
      WordEntry(
        presentPlural,
        List(Verb, EnglishWordTags.LinkVerb, VerbRoot(root), WordTense(BaseTense.Present)),
        (opt(l(A)) & l(Sp) & activeVerbWithPredicate),
      ),
      WordEntry(
        past,
        List(Verb, EnglishWordTags.LinkVerb, VerbRoot(root), WordTense(BaseTense.Past)),
        (opt(l(A)) & l(S) & activeVerbWithPredicate),
      ),
      WordEntry(
        presentParticiple,
        List(Verb, EnglishWordTags.LinkVerb, VerbRoot(root), WordTense(BaseTense.PresentParticiple)),
        (l(Tr) & r(O)),
      ),
      WordEntry(
        pastParticiple,
        List(Verb, EnglishWordTags.LinkVerb, VerbRoot(root), WordTense(BaseTense.PastParticiple)),
        (l(Ta) & activeVerbWithPredicate) | (opt(l(J)) & r(J)),
      ),
    )
}

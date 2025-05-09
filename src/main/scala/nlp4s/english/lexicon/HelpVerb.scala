package nlp4s.english.lexicon

import nlp4s.base.{Tense => BaseTense}
import nlp4s.parser.LinkRuleSyntax

case class HelpVerb(
  root: String,
  presentSingular: String,
  presentPlural: String,
  presentParticiple: String,
  past: String,
  pastParticiple: String,
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLinkTags._
  import EnglishLexiconEntry.WordEntry
  import EnglishWordTags.{Verb, RootForm, WordTense, VerbRoot, Singular, Plural}

  val label = root

  val wordEntries = 
    List(
      WordEntry(
        root,
        List(EnglishWordTags.Label(label), EnglishWordTags.HelpVerb, Verb, RootForm, VerbRoot(root)),
        opt(r(N)) & r(Hr),
      ), 
      WordEntry(
        presentSingular,
        List(EnglishWordTags.Label(label), EnglishWordTags.HelpVerb, Verb, WordTense(BaseTense.Present), Singular, VerbRoot(root)),
        opt(l(Qw)) & opt(r(N)) & r(Hs) | opt(r(N)) & r(Hs),
      ),
      WordEntry(
        presentPlural,
        List(EnglishWordTags.Label(label), EnglishWordTags.HelpVerb, Verb, WordTense(BaseTense.Present), Plural, VerbRoot(root)),
        opt(l(Qw)) & opt(r(N)) & r(Hp),
      ),
      WordEntry(
        past,
        List(EnglishWordTags.Label(label), EnglishWordTags.HelpVerb, Verb, WordTense(BaseTense.Past), VerbRoot(root)),
        opt(l(Qw)) & opt(r(N)) & r(H) | opt(r(N)) & r(Hs),
      )
    )
}

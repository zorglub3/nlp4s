package nlp4s.english.lexicon

import nlp4s.base.{Tense => BaseTense}
import nlp4s.parser.LinkRuleSyntax

case class ToBe() extends EnglishLexiconEntry {
  import EnglishLinkTags._
  import EnglishWordTags._
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val label = "be"

  val toBeObj = r(B) | r(O) | r(P) | r(Tr)

  val wordEntries =
    List(
      WordEntry(
        "be",
        List(Label("be"), EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("be"), RootForm),
        toBeObj |
        (l(Hs) & l(Ss) & toBeObj) |
        (l(Hs) & l(Qs) & toBeObj) |
        (l(Hp) & l(Sp) & toBeObj) |
        (opt(l(N)) & l(Sp) & l(Hp) & toBeObj) |
        (opt(l(N)) & l(Ss) & l(Hs) & toBeObj),
      ),
      WordEntry(
        "am",
        List(Label("be"), EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("be"), WordTense(BaseTense.Present)),
        ((l(Spi) & opt(r(N)) & toBeObj) |
         (opt(l(Qw)) & r(Sqpi) & opt(r(N)) & toBeObj) |
         (l(Qo) & r(Sqpi)) |
         (l(Qw) & r(Sqpi))
        ),
      ),
      WordEntry(
        "are",
        List(Label("be"), EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("be"), WordTense(BaseTense.Present)),
        ((l(Spp) & opt(r(N)) & toBeObj) | 
         (opt(l(Qw)) & r(Sqpp) & opt(r(N)) & toBeObj) |
         (l(Qo) & r(Sqpp)) |
         (l(Qw) & r(Sqpp)) 
        ),
      ),
      WordEntry(
        "is",
        List(Label("be"), EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("be"), WordTense(BaseTense.Present)),
        ((l(Ss) & opt(r(N)) & toBeObj) | 
         (opt(l(Qw)) & r(Sqs) & opt(r(N)) & toBeObj) |
         (l(Qo) & r(Sqs)) |
         (l(Qw) & r(Sqs)) |
         (l(Qs) & opt(r(N)) & r(Tr))
        ),
      ),
      WordEntry(
        "was",
        List(Label("be"), EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("be"), WordTense(BaseTense.Past)),
        ((l(Spi) & opt(r(N)) & toBeObj) |
         (opt(l(Qw)) & r(Sqpi) & opt(r(N)) & toBeObj) |
         (l(Qo) & r(Sqpi)) |
         (l(Qw) & r(Sqpi))
        ),
      ),
      WordEntry(
        "was",
        List(Label("be"), EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("be"), WordTense(BaseTense.Past)),
        ((l(Ss) & opt(r(N)) & toBeObj) |
         (opt(l(Qw)) & r(Sqs)  & opt(r(N)) & toBeObj) |
         (l(Qo) & r(Sqs)) |
         (l(Qw) & r(Sqs)) |
         (l(Qs) & opt(r(N)) & r(Tr))
        ),
      ),
      WordEntry(
        "were",
        List(Label("be"), EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("be"), WordTense(BaseTense.Past)),
        ((l(Spp) & opt(r(N)) & toBeObj) |
         (opt(l(Qw)) & r(Sqpp) & opt(r(N)) & toBeObj) |
         (l(Qo) & r(Sqpp)) |
         (l(Qw) & r(Sqpp))
        ),
      ),
      WordEntry(
        "been",
        List(Label("be"), EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("be"), WordTense(BaseTense.PastParticiple), VerbRoot("be"), Label("be")),
        l(Ta) & toBeObj
      ),
    )
}

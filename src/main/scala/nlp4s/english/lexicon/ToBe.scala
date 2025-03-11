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
        List(EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("be"), RootForm),
        toBeObj,
      ),
      WordEntry(
        "am",
        List(EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("be"), WordTense(BaseTense.Present)),
        ((l(Spi) & opt(r(N)) & toBeObj) |
         (opt(l(Qw)) & r(Sqpi) & opt(r(N)) & toBeObj) |
         (l(Qo) & r(Sqpi)) |
         (l(Qw) & r(Sqpi))
        ),
      ),
      WordEntry(
        "are",
        List(EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("be"), WordTense(BaseTense.Present)),
        ((l(Spp) & opt(r(N)) & toBeObj) | 
         (opt(l(Qw)) & r(Sqpp) & opt(r(N)) & toBeObj) |
         (l(Qo) & r(Sqpp)) |
         (l(Qw) & r(Sqpp)) 
        ),
      ),
      WordEntry(
        "is",
        List(EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("be"), WordTense(BaseTense.Present)),
        ((l(Ss) & opt(r(N)) & toBeObj) | 
         (opt(l(Qw)) & r(Sqs) & opt(r(N)) & toBeObj) |
         (l(Qo) & r(Sqs)) |
         (l(Qw) & r(Sqs)) |
         (l(Qs) & opt(r(N)) & r(Tr))
        ),
      ),
      WordEntry(
        "was",
        List(EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("be"), WordTense(BaseTense.Past)),
        ((l(Spi) & opt(r(N)) & toBeObj) |
         (opt(l(Qw)) & r(Sqpi) & opt(r(N)) & toBeObj) |
         (l(Qo) & r(Sqpi)) |
         (l(Qw) & r(Sqpi))
        ),
      ),
      WordEntry(
        "was",
        List(EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("be"), WordTense(BaseTense.Past)),
        ((l(Ss) & opt(r(N)) & toBeObj) |
         (opt(l(Qw)) & r(Sqs)  & opt(r(N)) & toBeObj) |
         (l(Qo) & r(Sqs)) |
         (l(Qw) & r(Sqs)) |
         (l(Qs) & opt(r(N)) & r(Tr))
        ),
      ),
      WordEntry(
        "were",
        List(EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("be"), WordTense(BaseTense.Past)),
        ((l(Spp) & opt(r(N)) & toBeObj) |
         (opt(l(Qw)) & r(Sqpp) & opt(r(N)) & toBeObj) |
         (l(Qo) & r(Sqpp)) |
         (l(Qw) & r(Sqpp))
        ),
      ),
    )
}

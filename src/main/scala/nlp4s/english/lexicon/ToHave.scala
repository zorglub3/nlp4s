package nlp4s.english.lexicon

import nlp4s.base.{Tense => BaseTense}
import nlp4s.parser.LinkRuleSyntax

case class ToHave() extends EnglishLexiconEntry {
  import EnglishLinkTags._
  import EnglishWordTags._
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val label = "have"

  val toBeObj = r(B) | r(O) | r(P) | r(Ta)

  val wordEntries =
    List(
      WordEntry(
        "have",
        List(EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("have"), RootForm),
        toBeObj,
      ),
      WordEntry(
        "have",
        List(EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("have"), WordTense(BaseTense.Present)),
        ((l(Spi) & opt(r(N)) & toBeObj) |
         (opt(l(Qw)) & r(Sqpi) & opt(r(N)) & toBeObj) |
         (l(Qo) & r(Sqpi)) |
         (l(Qw) & r(Sqpi))
        ),
      ),
      WordEntry(
        "have",
        List(EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("have"), WordTense(BaseTense.Present)),
        ((l(Spp) & opt(r(N)) & toBeObj) | 
         (opt(l(Qw)) & r(Sqpp) & opt(r(N)) & toBeObj) |
         (l(Qo) & r(Sqpp)) |
         (l(Qw) & r(Sqpp)) 
        ),
      ),
      WordEntry(
        "has",
        List(EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("have"), WordTense(BaseTense.Present)),
        ((l(Ss) & opt(r(N)) & toBeObj) | 
         (opt(l(Qw)) & r(Sqs) & opt(r(N)) & toBeObj) |
         (l(Qo) & r(Sqs)) |
         (l(Qw) & r(Sqs)) |
         (l(Qs) & opt(r(N)) & r(Ta))
        ),
      ),
      WordEntry(
        "had",
        List(EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("have"), WordTense(BaseTense.Past)),
        ((l(Spi) & opt(r(N)) & toBeObj) |
         (opt(l(Qw)) & r(Sqpi) & opt(r(N)) & toBeObj) |
         (l(Qo) & r(Sqpi)) |
         (l(Qw) & r(Sqpi))
        ),
      ),
      WordEntry(
        "had",
        List(EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("have"), WordTense(BaseTense.Past)),
        ((l(Ss) & opt(r(N)) & toBeObj) |
         (opt(l(Qw)) & r(Sqs)  & opt(r(N)) & toBeObj) |
         (l(Qo) & r(Sqs)) |
         (l(Qw) & r(Sqs)) |
         (l(Qs) & opt(r(N)) & r(Ta))
        ),
      ),
      WordEntry(
        "had",
        List(EnglishWordTags.ToBe, Verb, HelpVerb, VerbRoot("have"), WordTense(BaseTense.Past)),
        ((l(Spp) & opt(r(N)) & toBeObj) |
         (opt(l(Qw)) & r(Sqpp) & opt(r(N)) & toBeObj) |
         (l(Qo) & r(Sqpp)) |
         (l(Qw) & r(Sqpp))
        ),
      ),
    )
}

package nlp4s.english.lexicon

import nlp4s.parser.LinkRuleSyntax

case class Question() extends EnglishLexiconEntry {
  import EnglishLinkTags._
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val label = "question" // TODO

  val wordEntries =
    List(
      WordEntry(
        "who",
        List(EnglishWordTags.Question),
        /*l(W) & r(Ss) |*/ l(W) & r(Qo) | l(W) & r(Qs),
      ),
      WordEntry(
        "what",
        List(EnglishWordTags.Question),
        /*l(W) & r(Ss) |*/ l(W) & r(Qo) | l(W) & r(Qs),
      ),
      WordEntry(
        "where",
        List(EnglishWordTags.Question),
        l(W) & r(Qw),
      ),
      WordEntry(
        "why",
        List(EnglishWordTags.Question),
        l(W) & r(Qw),
      ),
      WordEntry(
        "when",
        List(EnglishWordTags.Question),
        l(W) & r(Qw),
      ),
      WordEntry(
        "how",
        List(EnglishWordTags.Question),
        l(W) & opt(r(A)) & r(Qw),
      ),
    )
}



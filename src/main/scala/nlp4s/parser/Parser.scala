package nlp4s.parser

import cats.data.StateT
import nlp4s.base.NlpResult
import nlp4s.base.WordTag

class Parser(ruleMap: RuleMap) {
  import cats.syntax.all._

  type Parse[A] = StateT[List, ParseState, A]

  case class ParseState(
    words: Vector[String],
    wordTags: Vector[List[WordTag]],
    graph: ImmutableSentenceGraph.T
  ) {
    def result: Parser.Output = {
      (wordTags, graph)
    }
  }

  def init(words: Vector[String]): ParseState =
    ParseState(
      words,
      Vector.fill(words.length)(List.empty),
      ImmutableSentenceGraph.empty)

  def wordRuleEntry(w: Int): Parse[RuleMap.Entry] =
    StateT.inspectF { s => ruleMap.lookup(s.words(w)) }

  def guard(b: Boolean): Parse[Unit] = StateT.liftF(if(b) List( () ) else List.empty)

  def addLink(edge: SentenceEdge): Parse[Unit] =
    StateT.modify { parseState => parseState.copy(graph = parseState.graph + edge) }

  def tagWord(w: Int, tags: List[WordTag]): Parse[Unit] = {
    StateT.modify { parseState =>
      val newTags = Vector.tabulate(parseState.words.length) { n =>
        if(n == w) { tags ++ parseState.wordTags(n) } else { parseState.wordTags(n) }
      }
      parseState.copy(wordTags = newTags)
    }
  }

  def where(cond: Boolean)(p: => Parse[Unit]): Parse[Unit] = {
    if(cond) p else StateT.pure(())
  }

  def makeLink(
    word1: Int,
    word2: Int,
    as: List[RightLink],
    bs: List[LeftLink]
  ): Parse[Unit] = {
    import SentenceEdgeSyntax._

    for {
      a <- StateT.liftF(as.take(1))
      b <- StateT.liftF(bs.take(1))
      _ <- guard(a.linkTag.matches(b.linkTag))
      _ <- addLink(word1 ~ word2 :+ a.linkTag.simplify)
    } yield ()
  }

  def link(
    leftIndex: Int,
    rightIndex: Int,
    l: List[LeftLink],
    r: List[RightLink]
  ): Parse[Unit] = {
    if(leftIndex + 1 == rightIndex) {
      if(l.isEmpty && r.isEmpty) { 
        StateT.pure[List, ParseState, Unit](()) // succeed
      } else { 
        StateT.liftF(List.empty) // fail
      }
    } else {
      def leftLinks(w: Int, linkRule: LinkRule): Parse[Boolean] = {
        for {
          _ <- makeLink(leftIndex, w, r, linkRule.leftLinks)
          _ <- link(leftIndex, w, linkRule.leftLinks.tail, r.tail)
        } yield true
      }

      def rightLinks(w: Int, linkRule: LinkRule): Parse[Boolean] = {
        for {
          _ <- makeLink(w, rightIndex, linkRule.rightLinks, l)
          _ <- link(w, rightIndex, l.tail, linkRule.rightLinks.tail)
        } yield true
      }

      for {
        w <- StateT.liftF((leftIndex + 1 until rightIndex).toList)
        entry <- wordRuleEntry(w)
        tags = entry.wordTags
        linkRule = entry.linkRule
        hasLeftLinks <- leftLinks(w, linkRule) <+> StateT.pure(false)
        hasRightLinks <- rightLinks(w, linkRule) <+> StateT.pure(false)
        _ <- where(hasLeftLinks) { link(w, rightIndex, l, linkRule.rightLinks) }
        _ <- where(hasRightLinks) { link(leftIndex, w, linkRule.leftLinks, r) }
        _ <- guard(hasLeftLinks || hasRightLinks)
        _ <- tagWord(w, tags)
      } yield ()
    }
  }

  def begin(start: Int, end: Int): Parse[Unit] = {
    for {
      r <- wordRuleEntry(start)
      _ <- guard(r.linkRule.l.isEmpty)
      _ <- link(start, end, List.empty, r.linkRule.rightLinks)
      _ <- tagWord(start, r.wordTags)
    } yield ()
  }

  def run(words: Vector[String]): NlpResult[List[Parser.Output]] = {
    val l = words.length

    (begin(0, l) <+> begin(1, l)).runS(init(words)).map(_.result) match {
      case Nil => Left(NoParse())
      case h::t => Right(h::t)
    }
  }
}

object Parser {
  type Output = (Vector[List[WordTag]], ImmutableSentenceGraph.T)
}

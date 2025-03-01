package nlp4s.parser

import cats.data.StateT
import nlp4s.base.NlpResult
import nlp4s.base.WordTag

class Parser(ruleMap: RuleMap) {
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

  def get: Parse[ParseState] = StateT.get

  def set(s: ParseState): Parse[Unit] = StateT.set(s)

  def success: Parse[Unit] = StateT.pure( () )

  def fail: Parse[Unit] = StateT.liftF(List.empty)

  def liftF[A](l: List[A]): Parse[A] = StateT.liftF(l)

  def pure[A](v: A): Parse[A] = StateT.pure(v)

  def liftOption[A](a: Option[A]): Parse[A] = StateT.liftF(a.toList)

  def wordRuleEntry(w: Int): Parse[RuleMap.Entry] = {
    // TODO use inspectF here
    for {
      parseState <- get
      entry <- liftF(ruleMap.lookup(parseState.words(w)))
    } yield entry
  }

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

  implicit class ParseSyntax[A](a: Parse[A]) {
    def orElse(b: Parse[A]): Parse[A] = {
      val sb = for {
        bb <- b
        s <- get
      } yield (s, bb)

      for {
        state <- get
        f <- liftF(a.runF)
        p = f.apply(state)
        p <- if(p.nonEmpty) liftF(p) else sb
        _ <- set(p._1)
      } yield p._2
    }
  }

  def where(cond: Boolean)(p: Parse[Unit]): Parse[Unit] = {
    if(cond) p else pure(())
  }

  def makeLink(
    word1: Int,
    word2: Int,
    as: List[RightLink],
    bs: List[LeftLink]
  ): Parse[Unit] = {
    import SentenceEdgeSyntax._

    for {
      a <- liftOption(as.headOption)
      b <- liftOption(bs.headOption)
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
        success 
      } else { 
        fail 
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
        w <- liftF((leftIndex + 1 until rightIndex).toList)
        entry <- wordRuleEntry(w)
        tags = entry.wordTags
        linkRule = entry.linkRule
        hasLeftLinks <- leftLinks(w, linkRule) orElse pure(false)
        hasRightLinks <- rightLinks(w, linkRule) orElse pure(false)
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

  def parse(words: Vector[String]): NlpResult[List[Parser.Output]] = {
    val l = words.length

    (begin(0, l) orElse begin(1, l)).runS(init(words)).map(_.result) match {
      case Nil => Left(NoParse())
      case h::t => Right(h::t)
    }
  }
}

object Parser {
  type Output = (Vector[List[WordTag]], ImmutableSentenceGraph.T)
}

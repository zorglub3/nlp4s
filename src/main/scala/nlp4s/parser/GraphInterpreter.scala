package nlp4s.parser

import cats.data.StateT
import nlp4s.base.LinkTag
import nlp4s.base.WordTag
import nlp4s.mrs.MRS
import nlp4s.parser.ImmutableSentenceGraph

class GraphInterpreter {
  type Interpret[A] = StateT[Option, S, A]

  case class S(
    wordTags: Vector[List[WordTag]],
    graph: ImmutableSentenceGraph.T,
    mrsBuilder: MRS.Builder,
  )

  def init(wordTags: Vector[List[WordTag]], graph: ImmutableSentenceGraph.T): S =
    S(wordTags, graph, MRS.newBuilder)

  def tags(position: Int): Interpret[List[WordTag]] =
    StateT.inspect(_.wordTags(position))

  def tokenHasTag(position: Int, tag: WordTag): Interpret[Boolean] =
    tags(position).map(_.contains(tag))

  def collectTag[T](position: Int)(f: PartialFunction[WordTag, T]): Interpret[List[T]] =
    tags(position).map(_.collect(f))

  import SentenceEdgeSyntax._

  def graphEdge(tag: LinkTag): Interpret[(Int, Int)] =
    StateT.inspectF(_.graph.edges.map(_.outer).collectFirst {
      case x :~ y +: t if tag.matches(t) => (x.min(y), x.max(y))
    } )

  def graphEdgeFrom(tag: LinkTag, p: Int): Interpret[Int] =
    StateT.inspectF(_.graph.edges.map(_.outer).collectFirst {
      case q :~ y +: t if tag.matches(t) && p == q => y
      case x :~ q +: t if tag.matches(t) && p == q => x
    } )

  def graphEdgeLeft(tag: LinkTag, p: Int): Interpret[Int] =
    StateT.inspectF(_.graph.edges.map(_.outer).collectFirst {
      case x :~ q +: t if tag.matches(t) && p == q && x < p => x
    } )

  def graphEdgeRight(tag: LinkTag, p: Int): Interpret[Int] =
    StateT.inspectF(_.graph.edges.map(_.outer).collectFirst {
      case q :~ x +: t if tag.matches(t) && p == q && x > p => x
    } )

  def guard(b: Boolean): Interpret[Unit] =
    StateT.liftF(Option.when(b)(()))
}

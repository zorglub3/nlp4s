package nlp4s.mrs

import cats.data.StateT
import nlp4s.base.LinkTag
import nlp4s.base.WordTag
import nlp4s.mrs.MRS
import nlp4s.parser.ImmutableSentenceGraph
import nlp4s.parser.SentenceEdgeSyntax

class GraphInterpreter {
  type Interpret[A] = StateT[Option, InterpreterState, A]

  case class InterpreterState(
    wordTags: Vector[List[WordTag]],
    graph: ImmutableSentenceGraph.T,
    mrsBuilder: MRS.Builder,
  )

  def init(wordTags: Vector[List[WordTag]], graph: ImmutableSentenceGraph.T): InterpreterState =
    InterpreterState(wordTags, graph, MRS.newBuilder)

  def tags(position: Int): Interpret[List[WordTag]] =
    StateT.inspect(_.wordTags(position))

  def tokenHasTag(position: Int, tag: WordTag): Interpret[Boolean] =
    tags(position).map(_.contains(tag))

  def guardTokenHasTag(position: Int, tag: WordTag): Interpret[Unit] =
    tags(position).map(_.contains(tag)).flatMap(guard(_))

  def collectTag[T](position: Int)(f: PartialFunction[WordTag, T]): Interpret[List[T]] =
    tags(position).map(_.collect(f))

  def makeHandle(): Interpret[Handle] =
    for {
      s <- StateT.get[Option, InterpreterState]
      p = s.mrsBuilder.mkHandle()
      _ <- StateT.set[Option, InterpreterState](s.copy(mrsBuilder = p._1))
    } yield p._2

  def mrsOp(f: MRS.Builder => MRS.Builder): Interpret[Unit] =
    for {
      s <- StateT.get[Option, InterpreterState]
      _ <- StateT.set[Option, InterpreterState](s.copy(mrsBuilder = f(s.mrsBuilder)))
    } yield ()

  def addRelation(handle: Handle, relation: Relation[Handle]): Interpret[Unit] =
    mrsOp(_.addRelation(handle, relation))

  def addRelationBag(handle: Handle, relations: List[Relation[Handle]]): Interpret[Unit] =
    mrsOp(_.addRelationBag(handle, relations))

  def addConstraint(source: Handle, target: Handle): Interpret[Unit] =
    mrsOp(_.addQeqConstraint(source, target))

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

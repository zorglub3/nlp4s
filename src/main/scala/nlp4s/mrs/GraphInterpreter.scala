package nlp4s.mrs

import cats.data.StateT
import nlp4s.base.LinkTag
import nlp4s.base.WordTag
import nlp4s.mrs.MRS
import nlp4s.parser.ImmutableSentenceGraph
import nlp4s.parser.SentenceEdgeSyntax

class GraphInterpreter {
  import cats.syntax.all._

  type Interpret[A] = StateT[Option, InterpreterState, A]

  case class InterpreterState(
    words: Vector[String],
    wordTags: Vector[List[WordTag]],
    graph: ImmutableSentenceGraph.T,
    mrsBuilder: MRS.Builder,
  )

  def init(
    words: Vector[String],
    wordTags: Vector[List[WordTag]], 
    graph: ImmutableSentenceGraph.T
  ): InterpreterState = {
    InterpreterState(words, wordTags, graph, MRS.newBuilder)
  }

  def word(position: Int): Interpret[String] = 
    StateT.inspect(_.words(position))

  def tags(position: Int): Interpret[List[WordTag]] =
    StateT.inspect(_.wordTags(position))

  def tokenHasTag(position: Int, tag: WordTag): Interpret[Boolean] =
    tags(position).map(_.contains(tag))

  def guardTokenHasTag(position: Int, tag: WordTag): Interpret[Unit] =
    tags(position).map(_.contains(tag)).flatMap(guard(_))

  def collectTag[T](position: Int)(f: PartialFunction[WordTag, T]): Interpret[List[T]] =
    tags(position).map(_.collect(f))

  def collectFirstTag[T](position: Int)(f: PartialFunction[WordTag, T]): Interpret[T] =
    tags(position).flatMap(x => StateT.liftF(x.collectFirst(f)))

  def makeHandle(): Interpret[Handle] =
    for {
      s <- StateT.get[Option, InterpreterState]
      p = s.mrsBuilder.mkHandle()
      _ <- StateT.set[Option, InterpreterState](s.copy(mrsBuilder = p._1))
    } yield p._2

  def makeVariable(): Interpret[Variable] =
    for {
      s <- StateT.get[Option, InterpreterState]
      p = s.mrsBuilder.mkVariable()
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

  def getMRSTop(): Interpret[Handle] =
    StateT.inspect(_.mrsBuilder.top)

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

  def guardEmpty[T](f: Interpret[T]): Interpret[Unit] = {
    for {
      v <- f.map(_ => false) <+> pure(true)
      _ <- guard(v)
    } yield ()
  }

  def guard(b: Boolean): Interpret[Unit] =
    StateT.liftF(Option.when(b)(()))

  def fail[T](): Interpret[T] =
    StateT.liftF(None)

  def fromOption[T](v: Option[T]): Interpret[T] =
    StateT.liftF(v)

  def optional[T](v: Option[Interpret[T]]): Interpret[Unit] =
    v.map(_.flatMap(_ => pure(()))).getOrElse(pure(()))

  def toOption[T](v: Interpret[T]): Interpret[Option[T]] =
    v.map(Option.apply) <+> pure(None)

  def pure[T](v: T): Interpret[T] =
    StateT.pure(v)

  implicit class InterpretSyntax[T](v: Interpret[T]) {
    def andThen[U](other: Interpret[U]): Interpret[U] = {
      v.flatMap(_ => other)
    }
  }
}

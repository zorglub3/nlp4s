package nlp4s.parser

import nlp4s.base.LinkTag
import scalax.collection.generic.{AbstractUnDiEdge, UnapplyLabeledEdge, UnapplyLabel}
import scalax.collection.edges.UnDiEdge

case class SentenceEdge(
  override val source: Int, 
  override val target: Int, 
  label: LinkTag,
) extends AbstractUnDiEdge(source, target)

object SentenceEdgeSyntax {
  implicit final class SentenceUnDiEdgeImplicits(val source: Int) extends AnyVal {
    def ~(target: Int) = UnDiEdge[Int](source, target)
  }

  implicit final class SentenceEdgeImplicits(val e: UnDiEdge[Int]) extends AnyVal {
    def :+(label: LinkTag) = SentenceEdge(e.source, e.target, label)
  }

  type Node = Int
  type Label = LinkTag

  object :~ extends UnapplyLabeledEdge[Node, SentenceEdge, Label] {
    protected def label(e: SentenceEdge): Label = e.label
  }

  object +: extends UnapplyLabel[Node, Label]
}

package nlp4s.parser

import scalax.collection.immutable.{Graph => ImmutableGraph}
import scalax.collection.mutable.Builder
import scalax.collection.mutable.TypedGraphFactory
import scalax.collection.mutable.{Graph => MutableGraph}

object ImmutableSentenceGraph {
  type T = ImmutableGraph[Int, SentenceEdge]

  def empty: T = from(List.empty, List.empty)

  def from(nodes: Iterable[Int], edges: Iterable[SentenceEdge]): T = 
    ImmutableGraph.from(nodes, edges)

  def newBuilder: Builder[Int, SentenceEdge, ImmutableGraph] = ImmutableGraph.newBuilder
}

object MutableSentenceGraph extends TypedGraphFactory[Int, SentenceEdge] {
  type T = MutableGraph[Int, SentenceEdge]
}

package nlp4s.parser

import scalax.collection.mutable.{Graph => MutableGraph, TypedGraphFactory, Builder}
import scalax.collection.immutable.{Graph => ImmutableGraph}

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

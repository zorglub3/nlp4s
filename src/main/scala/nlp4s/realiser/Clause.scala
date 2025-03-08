package nlp4s.realiser

import nlp4s.mrs.Relation

trait Clause

object Clause {
  case class StringClause(items: List[String]) extends Clause
  // case class MixedClause(items: List[Item]) extends Clause
  case class MRSClause(mrs: Relation.Recursive) extends Clause

  // trait Item

  // case class StringItem(s: String) extends Item
}

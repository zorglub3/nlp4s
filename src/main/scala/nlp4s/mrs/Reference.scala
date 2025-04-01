package nlp4s.mrs

abstract class Reference[E] {
  type ReferenceMap = Map[Variable, Reference.R[E]]
  type Relations = List[Relation[Relation.Recursive]]

  // Lookup an entity based on the relations we gather, that relates to it
  // `entities` is a map of entities we have resolved already
  // return list of entities for which all relations/predicates are true
  def lookup(
    relations: Relations,
    entities: ReferenceMap,
  ): Reference.R[E]

  def combine(
    pairs: List[(Relations, ReferenceMap)]
  ): (Relations, ReferenceMap) = {
    (pairs.map(_._1).flatten, pairs.map(_._2).flatten.toMap)
  }

  def resolveRec(
    rec: Relation.Recursive,
    referenceMap: ReferenceMap
  ): (Relations, ReferenceMap) = {
    rec match {
      case Relation.Recursive(_, relations) => {
        val relationsAndMap = relations.map(resolveRel(_, referenceMap))
        combine((relations, referenceMap) +: relationsAndMap)
      }
    }
  }

  def resolveRel(
    rel: Relation[Relation.Recursive],
    referenceMap: ReferenceMap
  ): (Relations, ReferenceMap) = {
    rel match {
      case Relation.Quantifier(name, variable, predicate, scope) => {
        val (rs, rm) = resolveRec(predicate, referenceMap)
        resolveRec(scope, rm + (variable -> lookup(rs, rm)))
      } 
      case _ => {
        combine(rel.scopalArgs.toList.map(resolveRec(_, referenceMap)))
      }
    }
  }

  // Recursively go through the HPSG form. Resolve inner quantified variables
  // before the outer ones
  def resolve(
    ast: AST,
    referenceMap: ReferenceMap = Map.empty
  ): ReferenceMap = {
    ast.tree match {
      case Relation.Recursive(_handle, relations) => {
        relations
          .map(resolveRel(_, referenceMap))
          .map(_._2)
          .foldLeft(Map.empty[Variable, Reference.R[E]])(_ ++ _)
      }
    }
  }
}

object Reference {
  sealed trait R[E]

  final case class Individual[E](entity: E) extends R[E]
  final case class Group[E](entities: Set[E]) extends R[E]
  final case class OneOf[E](entities: Set[E]) extends R[E]
  final case class Unresolved[E]() extends R[E]
}

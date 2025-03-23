package nlp4s.mrs

abstract class Reference[E] {
  // Lookup an entity based on the relations we gather, that relates to it
  // `entities` is a map of entities we have resolved already
  // return list of entities for which all relations/predicates are true
  def lookup(relations: List[Relation[Relation.Recursive]], entities: Map[Variable, E]): List[E]

  // Recursively go through the HPSG form. Resolve inner quantified variables
  // before the outer ones
  def resolve(rel: Relation.Recursive): Map[Variable, E] = ???
}

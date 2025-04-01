package nlp4s.mrs

case class AST(
  tree: Relation.Recursive,
  globalPredicates: Set[Relation[Handle]],
  globalVariables: Set[Variable]
)

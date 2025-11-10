package nlp4s.mrs

case class AST(
  tree: Relation.Recursive,
  globalPredicates: Set[Relation[Handle]],
  globalVariables: Set[Variable]
)

object AST {
  def pp(ast: AST): Unit = {
    def pr(r: Relation[Handle]): String = {
      // TODO better print of verb relations, eg, "x2:take(x4, x43)" (the "verb global variable")
      val args: Seq[String] =
        r.scopedVariables.map(_.name) ++
        r.variableArgs.map(_.name) ++
        r.scopalArgs.map(_.asString)

      s"${r.name}(${args.mkString(", ")})"
    }

    println("<")
    println("  ast:")
    Relation.pp(ast.tree, 4)
    println(s"  variables:  ${ast.globalVariables.map(_.name).mkString(", ")}")
    println(s"  predicates: ${ast.globalPredicates.map(pr).mkString(", ")}")
    println(">")
  }
}

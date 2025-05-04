package nlp4s.mrs

case class MRS(
  hook: MRS.Hook,
  eps: Map[Handle, List[Relation[Handle]]],
  constraints: Set[Constraint],
) {
  def globalTop = hook.globalTop
  def localTop = hook.localTop
  def globalVariables = hook.globalVariables
  def globalRelations = hook.globalPredicates

  def allHandles: Set[Handle] = 
    (eps.keys ++ constraints.map { _.src }).toSet + globalTop + localTop

  def floatingEps: Set[Handle] = {
    allHandles.filter { h => 
      ! eps.values.exists { _.exists { _.scopalArgs.contains(h) } }
    } - globalTop
  }

  private[mrs] def initState: QuantifierScope.QuantifierState = {
    QuantifierScope.QuantifierState(
      hook.globalVariables,
      Set.empty,
      eps.filter { p => ! floatingEps.contains(p._1) },
      eps.filter { p => floatingEps.contains(p._1) },
      constraints)
  }
}

object MRS {
  case class Hook(
    globalTop: Handle,
    localTop: Handle,
    index: Option[Variable],
    globalVariables: Set[Variable],
    globalPredicates: Set[Relation[Handle]],
  )

  object Hook {
    def basic(gt: Handle, lt: Handle): Hook = 
      Hook(gt, lt, None, Set.empty, Set.empty)
  }

  def newBuilder: Builder = {
    val hg = Handle.initGenerator
    val (hg1, h0) = hg.generate()
    val vg = Variable.initGenerator

    Builder(hg1, vg, h0, Map.empty, Set.empty, Set.empty, Set.empty)
  }

  case class Builder(
    handleGenerator: Handle.Generator,
    variableGenerator: Variable.Generator,
    top: Handle, 
    eps: Map[Handle, List[Relation[Handle]]],
    constraints: Set[Constraint],
    globalVariables: Set[Variable],
    globalRelations: Set[Relation[Handle]],
  ) {
    def mkHandle(): (Builder, Handle) = {
      val (newHg, h) = handleGenerator.generate()
      (copy(handleGenerator = newHg), h)
    }

    def mkVariable(): (Builder, Variable) = {
      val (newVg, v) = variableGenerator.generate()
      (copy(variableGenerator = newVg), v)
    }

    def mkGlobalVariable(): (Builder, Variable) = {
      val (newVg, v) = variableGenerator.generate()
      (copy(variableGenerator = newVg, globalVariables = globalVariables + v), v)
    }

    def addRelation(handle: Handle, relation: Relation[Handle]): Builder =
      copy(eps = eps + (handle -> List(relation)))

    def addGlobalRelation(relation: Relation[Handle]): Builder =
      copy(globalRelations = globalRelations + relation)

    def addRelationBag(handle: Handle, relations: List[Relation[Handle]]): Builder =
      copy(eps = eps + (handle -> relations))

    def addQeqConstraint(src: Handle, target: Handle): Builder =
      copy(constraints = constraints + Constraint(src, target))

    def getTop(): Handle = top

    def result(): MRS = 
      MRS(
        Hook(top, top, None, globalVariables, globalRelations),
        eps,
        constraints)
  }

  def pp(mrs: MRS): Unit = {
    def pr(r: Relation[Handle]): String = {
      // TODO better print of verb relations, eg, "x2:take(x4, x43)" (the "verb global variable")

      val args: Seq[String] = 
        r.scopedVariables.map(_.name) ++
        r.variableArgs.map(_.name) ++
        r.scopalArgs.map(_.asString)

      s"${r.name}(${args.mkString(", ")})"
    }

    println("<")
    println(s"  global top:    ${mrs.globalTop.asString}")
    println(s"  local top:     ${mrs.localTop.asString}")
    println(s"  relations:   { ${mrs.eps.map { case (k, v) => s"${k.asString}: (${v.map(pr).mkString(", ")})" } .mkString(",\n                 ")} }")
    println(s"  constraints: { ${mrs.constraints.map { case Constraint(s, t) => s"${s.asString} =_q ${t.asString}" } .mkString(",\n                 ")} }")
    println(s"  variables:   { ${mrs.globalVariables.map { _.name } .mkString(", ")} }")
    println(s"  t-relations: { ${mrs.globalRelations.map(pr).mkString(", ")} }")
    println(">")
  }
}

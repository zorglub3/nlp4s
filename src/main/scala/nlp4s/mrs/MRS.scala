package nlp4s.mrs

case class MRS(
  globalTop: Handle,
  localTop: Handle,
  eps: Map[Handle, List[Relation[Handle]]],
  constraints: Map[Handle, Constraint],
) {
  def allHandles: Set[Handle] = 
    (eps.keys ++ constraints.keys).toSet + globalTop + localTop

  // find all handles that are not scopal args of another relation, ie, 
  // find the handles of all the _floating_ EPs.
  // Note 1: all _floating_ EPs should have scopal args that are in 'constraints'
  // Note 2: all _floating_ EPs should be quantifiers
  def floatingEps: Set[Handle] = {
    allHandles.filter { h => 
      ! eps.values.exists { _.exists { _.scopalArgs.contains(h) } }
    } - globalTop
  }

  private[mrs] def initState: QuantifierScope.QuantifierState = {
    QuantifierScope.QuantifierState(
      Set.empty,
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
  )

  object Hook {
    def basic(gt: Handle, lt: Handle): Hook = 
      Hook(gt, lt, None)
  }

  def newBuilder: Builder = {
    val hg = Handle.initGenerator
    val (hg1, h0) = hg.generate()
    val vg = Variable.initGenerator

    Builder(hg1, vg, h0, Map.empty, Map.empty)
  }

  case class Builder(
    handleGenerator: Handle.Generator,
    variableGenerator: Variable.Generator,
    top: Handle, 
    eps: Map[Handle, List[Relation[Handle]]],
    constraints: Map[Handle, Constraint],
  ) {
    def mkHandle(): (Builder, Handle) = {
      val (newHg, h) = handleGenerator.generate()
      (copy(handleGenerator = newHg), h)
    }

    def mkVariable(): (Builder, Variable) = {
      val (newVg, v) = variableGenerator.generate()
      (copy(variableGenerator = newVg), v)
    }

    def addRelation(handle: Handle, relation: Relation[Handle]): Builder =
      copy(eps = eps + (handle -> List(relation)))

    def addRelationBag(handle: Handle, relations: List[Relation[Handle]]): Builder =
      copy(eps = eps + (handle -> relations))

    def addQeqConstraint(src: Handle, target: Handle): Builder =
      copy(constraints = constraints + (src -> Constraint(target)))

    def result(): MRS = MRS(top, top, eps, constraints)
  }

  def pp(mrs: MRS): Unit = {
    def pr(r: Relation[Handle]): String = {
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
    println(s"  constraints: { ${mrs.constraints.map { case (k, Constraint(v)) => s"${k.asString} =_q ${v.asString}" } .mkString(",\n                 ")} }")
    println(">")
  }
}

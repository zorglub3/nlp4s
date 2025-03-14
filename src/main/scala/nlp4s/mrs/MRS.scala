package nlp4s.mrs

import cats.data.StateT

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

  private[mrs] def initState: MRS.K = {
    MRS.K(
      constraints,
      eps.filter { p => ! floatingEps.contains(p._1) },
      eps.filter { p => floatingEps.contains(p._1) },
      Set.empty)
  }

  def instances: List[Relation.Recursive] = {
    val ep = {
      for {
        ep <- MRS.mkRecursiveEP(globalTop)
        _ <- MRS.finalCheck()
      } yield ep
    }

    ep.runA(initState)
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

  import cats.syntax.all._

  private[mrs] type F[T] = StateT[List, K, T]

  private[mrs] case class K(
    constraints: Map[Handle, Constraint],
    eps: Map[Handle, List[Relation[Handle]]],
    floatingEps: Map[Handle, List[Relation[Handle]]],
    scopedVariables: Set[Variable],
  )

  private[mrs] def pure[T](v: T): F[T] = StateT.pure(v)

  private[mrs] def liftFF[T](v: List[F[T]]): F[List[T]] = v.sequence 

  private[mrs] def andAlso[T](a: F[T], b: F[T]): F[T] = a <+> b

  private[mrs] def modifyState(f: K => K): F[Unit] =
    StateT.modify(f)

  private[mrs] def withVariables[T](variables: Seq[Variable])(f: F[T]): F[T] = {
    for {
      _ <- modifyState(s => s.copy(scopedVariables = s.scopedVariables ++ variables))
      t <- f
      _ <- modifyState(s => s.copy(scopedVariables = s.scopedVariables -- variables))
    } yield t
  }

  private[mrs] def verifyEP(eps: List[Relation[Handle]]): F[Unit] = {
    val allVariables = eps.flatMap(_.variableArgs)

    // TODO StateT.inspectF?
    for {
      s <- StateT.get[List, K]
      b =  allVariables.forall(s.scopedVariables.contains(_))
      _ <- StateT.liftF[List, K, Unit](if(b) List(()) else List.empty)
    } yield ()
  }
      
  private[mrs] def getEP(h: Handle): F[List[Relation[Handle]]] = {
    // TODO StateT.inspectF
    for {
      state <- StateT.get[List, K]
      ep <- StateT.liftF(state.eps.get(h).toList)
      _ <- verifyEP(ep)
      _ <- StateT.set[List, K](state.copy(eps = state.eps - h))
    } yield ep
  }

  private[mrs] def getFloatingEP(target: Handle): F[List[Relation[Handle]]] = {
    def getTarget = getEP(target)
    def nextFloating = {
      // TODO StateT.inspectF
      for {
        state <- StateT.get[List, K]
        p <- StateT.liftF(state.floatingEps.toList)
        _ <- verifyEP(p._2)
        _ <- StateT.set[List, K](state.copy(floatingEps = state.floatingEps - p._1))
      } yield p._2
    }

    andAlso(getTarget, nextFloating)
  }
  
  private[mrs] def getConstrainedEP(h: Handle): F[List[Relation[Handle]]] = {
    // TODO StateT.inspectF
    for {
      state <- StateT.get[List, K]
      constraint <- StateT.liftF(state.constraints.get(h).toList)
      _ <- StateT.set[List, K](state.copy(constraints = state.constraints - h))
      ep <- getFloatingEP(constraint.handle)
    } yield ep
  }

  private[mrs] def finalCheck(): F[Unit] = {
    // TODO StateT.inspectF
    for {
      state <- StateT.get[List, K]
      b = state.eps.isEmpty && state.floatingEps.isEmpty
      _ <- StateT.liftF(if(b) List(()) else List.empty)
    } yield ()
  }

  private[mrs] def mkRecursiveEP(h: Handle): F[Relation.Recursive] = {
    for {
      eps <- andAlso(getEP(h), getConstrainedEP(h))
      rec <- liftFF(eps.map(_.flatMapH(mkRecursiveEP _)))
    } yield Relation.Recursive(h, rec)
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
    println(s"  global top:  ${mrs.globalTop.asString}")
    println(s"  local top:   ${mrs.localTop.asString}")
    println(s"  relations:   { ${mrs.eps.map { case (k, v) => s"${k.asString}: (${v.map(pr).mkString(", ")})" } .mkString(", ")} }")
    println(s"  constraints: { ${mrs.constraints.map { case (k, Constraint(v)) => s"${k.asString} =_q ${v.asString}" } .mkString(", ")} }")
    println(">")
  }
}

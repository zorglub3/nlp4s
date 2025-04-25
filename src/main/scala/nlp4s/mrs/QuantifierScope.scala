package nlp4s.mrs

import cats.data.StateT

class QuantifierScope {
  import cats.syntax.all._
  import QuantifierScope._

  def getEP(h: Handle): F[Relation.Recursive] = {
    for {
      ep1 <- inspectF { s => s.eps.get(h).toList }
      _   <- verify(ep1)
      _   <- modifyState { s => s.copy(eps = s.eps - h) }
      _   <- modifyState { _.removeTarget(h) }
      ep2 <- ep1.map(_.flatMapH(resolveRec(_))).sequence
    } yield Relation.Recursive(h, ep2)
  }

  def getConstrainedEP(h: Handle): F[Relation.Recursive] = {
    for {
      constraint <- inspectF { _.constraints.filter { _.src == h } .toList }
      _ <- modifyState { s => s.copy(constraints = s.constraints - constraint) }
      _ <- modifyState { _.removeTarget(h) }
      h <- inspectF { _.floating.keys.toList }
      rel <- withTarget(constraint.tgt) { resolveRec(h) }
    } yield rel
  }

  def getFloatingEP(h: Handle): F[Relation.Recursive] = {
    for {
      ep1 <- inspectF { s => s.floating.get(h).toList }
      _   <- verify(ep1)
      _   <- modifyState { s => s.copy(floating = s.floating - h) }
      _   <- modifyState { _.removeTarget(h) }
      ep2 <- ep1.map(_.flatMapH(resolveRec(_))).sequence
    } yield Relation.Recursive(h, ep2)
  }

  def resolveRec(h: Handle): F[Relation.Recursive] = {
    getEP(h) <+> getFloatingEP(h) <+> getConstrainedEP(h)
  }

  def resolve(mrs: MRS): List[AST] = {
    val ep = {
      for {
        ep <- resolveRec(mrs.globalTop)
        _  <- finalCheck()
      } yield ep
    }

    ep.runA(mrs.initState).map(AST.apply(_, mrs.globalRelations, mrs.globalVariables))
  }
}

object QuantifierScope {
  type F[T] = StateT[List, QuantifierState, T]

  case class QuantifierState(
    variables: Set[Variable],
    targets: Set[Handle],
    eps: Map[Handle, List[Relation[Handle]]],
    floating: Map[Handle, List[Relation[Handle]]],
    constraints: Set[Constraint]
  ) {
    def addVariables(vs: Seq[Variable]): QuantifierState =
      copy(variables = variables ++ vs)

    def removeVariables(vs: Seq[Variable]): QuantifierState =
      copy(variables = variables -- vs)

    def addTarget(target: Handle): QuantifierState =
      copy(targets = targets + target)
    
    def hasTarget(target: Handle): Boolean =
      targets.contains(target)

    def removeTarget(target: Handle): QuantifierState =
      copy(targets = targets - target)
  }

  def modifyState(f: QuantifierState => QuantifierState): F[Unit] =
    StateT.modify(f)

  def inspectF[A](f: QuantifierState => List[A]): F[A] =
    StateT.inspectF(f)

  def inspect[A](f: QuantifierState => A): F[A] =
    StateT.inspect(f)

  def guard(b: Boolean): F[Unit] = 
    StateT.liftF(if(b) List(()) else List.empty)

  def pure[A](v: A): F[A] =
    StateT.pure(v)

  def withVariables[T](variables: Seq[Variable])(f: F[T]): F[T] = {
    for {
      _ <- modifyState { _.addVariables(variables) }
      t <- f
      _ <- modifyState { _.removeVariables(variables) }
    } yield t
  }

  def withTarget[T](target: Handle)(f: F[T]): F[T] = {
    for {
      _ <- modifyState { _.addTarget(target) }
      t <- f
      b <- inspect { _.hasTarget(target) }
      _ <- guard(!b)
    } yield t
  }

  def verify(eps: List[Relation[Handle]]): F[Unit] = {
    val allVariables = eps.flatMap(_.variableArgs)

    for {
      scopedVariables <- inspect { _.variables }
      _ <- guard(allVariables.forall(scopedVariables.contains))
    } yield ()
  }

  def finalCheck(): F[Unit] = {
    for {
      b <- inspect { s => s.eps.isEmpty && s.floating.isEmpty }
      _ <- guard(b)
    } yield ()
  }
}

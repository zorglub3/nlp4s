package nlp4s.realiser

import cats.data.StateT
import cats.data.WriterT
import cats.syntax.all._
import nlp4s.base.NlpResult
import nlp4s.base.Mode
import nlp4s.base.Tense
import nlp4s.mrs.Relation
import nlp4s.mrs.Variable
import nlp4s.mrs.AST

abstract class Realiser {
  type E[T] = Either[RealiserError, T]
  type W[T] = WriterT[E, List[String], T]
  type F[T] = StateT[W, RealiserState, T]

  case class Modality(verb: String, negated: Boolean)

  case class RealiserState(
    variableRelations: List[(Variable, List[Relation[Relation.Recursive]])],
    globalRelations: List[Relation[Relation.Recursive]],
    modality: List[Modality],
  ) {
    def pushModality(verb: String, negated: Boolean): RealiserState =
      RealiserState(variableRelations, globalRelations, Modality(verb, negated)::modality)
    def popModality(): RealiserState = {
      if(modality.isEmpty) {
        RealiserState(variableRelations, globalRelations, List.empty)
      } else {
        RealiserState(variableRelations, globalRelations, modality.tail)
      }
    }

    def push(p: (Variable, List[Relation[Relation.Recursive]])): RealiserState =
      RealiserState(p :: variableRelations, globalRelations, modality)
    def pop(): RealiserState = {
      if(variableRelations.isEmpty) {
        RealiserState(List.empty, globalRelations, modality)
      } else {
        RealiserState(variableRelations.tail, globalRelations, modality)
      }
    }

    def currentModality(): Option[Modality] = modality.headOption
  }

  def init(): RealiserState = RealiserState(List.empty, List.empty, List.empty)

  def pushModality(verb: String, negated: Boolean): F[Unit] =
    StateT.modify(_.pushModality(verb, negated))

  def popModality(): F[Unit] =
    StateT.modify(_.popModality())

  def push(p: (Variable, List[Relation[Relation.Recursive]])): F[Unit] =
    StateT.modify(_.push(p))

  def pop(): F[Unit] =
    StateT.modify(_.pop())

  def globalPredicate(u: Variable): F[List[Relation[Relation.Recursive]]] = {
    StateT.inspect(_.globalRelations.filter(_.variableArgs.contains(u)))
  }

  def allRelations(v: Variable): F[List[Relation[Relation.Recursive]]] = {
    for {
      global <- globalPredicate(v)
      local <- variableRelations(v)
    } yield global ++ local
  }

  def currentModality(): F[Option[Modality]] =
    StateT.inspect(_.currentModality())

  def variableRelations(u: Variable): F[List[Relation[Relation.Recursive]]] = {
    StateT.inspect(_.variableRelations
      .filter { _._1 == u } 
      .map { _._2 }
      .flatten)
  }

  def setGlobalPredicates(gr: List[Relation[Relation.Recursive]]): F[Unit] = 
    StateT.modify(_.copy(globalRelations = gr))

  def failRelation(relation: Relation[_]): F[Unit] = 
    StateT.liftF(WriterT.valueT(Left(RealiserRelationFail(relation))))

  def failClause(clause: Clause): F[Unit] =
    StateT.liftF(WriterT.valueT(Left(RealiserClauseFail(clause))))

  def pure[T](v: T): F[T] = StateT.liftF(WriterT.value(v))

  def when(b: Boolean)(f: => F[_]): F[Unit] = 
    if(b) { f >> pure(()) } else { pure(()) }

  def whenOpt[T](o: Option[T])(f: T => F[_]): F[Unit] =
    o.map(x => (f(x) >> pure(()))).getOrElse(pure(()))

  def tell(s: String): F[Unit] = StateT.liftF(WriterT.tell(List(s)))

  def tellMore(ss: List[String]): F[Unit] = StateT.liftF(WriterT.tell(ss))

  def liftOption[T](v: Option[T], err: => RealiserError): F[T] = {
    v match {
      case Some(u) => StateT.liftF(WriterT.valueT(Right(u)))
      case None => StateT.liftF(WriterT.valueT(Left(err)))
    }
  }

  import Relation._

  def verbMode(v: Variable): F[Mode] = {
    StateT.inspectF { s => 
      WriterT.valueT(
        s.globalRelations.collectFirst { case VerbMode(mode, u) if v == u => mode } match {
          case None => Left(RealiserMissingMode(v))
          case Some(mode) => Right(mode)
        }
      ) 
    }
  }

  def verbTense(v: Variable): F[Tense] = {
    StateT.inspectF { s =>
      WriterT.valueT(
        s.globalRelations.collectFirst { case VerbTense(tense, u) if v == u => tense } match {
          case None => Left(RealiserMissingTense(v))
          case Some(tense) => Right(tense)
        }
      )
    }
  }

  def collectRelations(x: Variable, rel: Relation[Recursive]): F[List[Relation[Recursive]]] = {
    rel match {
      case Quantifier(_, y, rstr, body) if x == y => {
        for {
          r1 <- collectRelationsRec(x, rstr)
          r2 <- collectRelationsRec(x, body)
        } yield r1 ++ r2
      }
      case rel if rel.subject == Some(x) => pure(List(rel))
      case _ => pure(List.empty)
    }
  }

  def collectRelationsRec(x: Variable, rstr: Recursive): F[List[Relation[Recursive]]] =
    rstr.relations.map(collectRelations(x, _)).sequence.map(_.flatten)

  def tellRelationList(body: List[Relation[Recursive]], f: Recursive => F[Unit]): F[Unit]

  def tellRecRelation(body: Recursive): F[Unit] = {
    body match {
      case Recursive(_, List(q @ Quantifier(lbl, x, rstr, body2))) => {
        for {
          rels <- collectRelationsRec(x, rstr)
          _ <- push(x -> (q::rels))
          _ <- tellRecRelation(body2)
          _ <- pop()
        } yield ()
      }
      case Recursive(_, rels) if !rels.exists(_.isQuantifier) => tellRelationList(rels, tellRecRelation)
      case _ => ??? // TODO
    }
  }

  def realiseMRS(ast: AST): F[Unit] = {
    import Relation._

    ast.tree match {
      case Recursive(_, List(q @ Quantifier(lbl, x, rstr, body))) => {
        for {
          rels <- collectRelationsRec(x, rstr)
          _ <- push(x -> (q::rels))
          _ <- tellRecRelation(body)
          _ <- pop()
        } yield ()
      }
      case _ => tellRecRelation(ast.tree)
    }
  }
    
  def realiseClause(clause: Clause): F[Unit] = {
    clause match {
      case Clause.StringClause(items) => tellMore(items) >> tell(clause.punctuation)
      case Clause.MRSClause(ast) => {
        setGlobalPredicates(ast.globalPredicates) >> realiseMRS(ast) >> tell(clause.punctuation)
      }
      case _ => failClause(clause)
    }
  }

  def run(clauses: List[Clause]): NlpResult[List[String]] = {
    clauses.map(realiseClause).sequenceVoid.runS(init()).run.map(_._1)
  }
}

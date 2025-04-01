package nlp4s.realiser

import cats.data.StateT
import cats.data.WriterT
import cats.syntax.all._
import nlp4s.base.NlpResult
import nlp4s.base.Mode
import nlp4s.base.Tense
import nlp4s.mrs.Relation
import nlp4s.mrs.Variable
import nlp4s.mrs.Handle
import nlp4s.mrs.AST

abstract class Realiser {
  type W[T] = WriterT[Option, List[String], T]
  type F[T] = StateT[W, RealiserState, T]

  case class RealiserState(
    variableRelations: List[(Variable, List[Relation[Relation.Recursive]])],
    globalRelations: Set[Relation[Handle]]
  ) {
    def push(p: (Variable, List[Relation[Relation.Recursive]])): RealiserState =
      RealiserState(p :: variableRelations, globalRelations)
    def pop(): RealiserState = {
      if(variableRelations.isEmpty) {
        RealiserState(List.empty, globalRelations)
      } else {
        RealiserState(variableRelations.tail, globalRelations)
      }
    }
  }

  def init(): RealiserState = RealiserState(List.empty, Set.empty)

  def push(p: (Variable, List[Relation[Relation.Recursive]])): F[Unit] =
    StateT.modify(_.push(p))

  def pop(): F[Unit] =
    StateT.modify(_.pop())

  def globalPredicate(u: Variable): F[Set[Relation[Handle]]] = {
    StateT.inspect(_.globalRelations.filter(_.variableArgs.contains(u)))
  }

  def variableRelations(u: Variable): F[List[Relation[Relation.Recursive]]] = {
    StateT.inspect(_.variableRelations
      .filter { _._1 == u } 
      .map { _._2 }
      .flatten)
  }

  def setGlobalPredicates(gr: Set[Relation[Handle]]): F[Unit] = 
    StateT.modify(_.copy(globalRelations = gr))

  def fail: F[Unit] = StateT.liftF(WriterT.valueT(None))

  def pure[T](v: T): F[T] = StateT.liftF(WriterT.value(v))

  def when(b: Boolean)(f: => F[_]): F[Unit] = 
    if(b) { f >> pure(()) } else { pure(()) }

  def whenOpt[T](o: Option[T])(f: T => F[_]): F[Unit] =
    o.map(x => (f(x) >> pure(()))).getOrElse(pure(()))

  def tell(s: String): F[Unit] = StateT.liftF(WriterT.tell(List(s)))

  def tellMore(ss: List[String]): F[Unit] = StateT.liftF(WriterT.tell(ss))

  def liftOption[T](v: Option[T]): F[T] = StateT.liftF(WriterT.valueT(v))

  import Relation._

  def verbMode(v: Variable): F[Mode] = {
    StateT.inspectF { s => 
      WriterT.valueT(
        s.globalRelations.collectFirst { case VerbMode(mode, u) if v == u => mode }
      ) 
    }
  }

  def verbTense(v: Variable): F[Tense] = {
    StateT.inspectF { s =>
      WriterT.valueT(
        s.globalRelations.collectFirst { case VerbTense(tense, u) if v == u => tense }
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

  def tellRelationList(body: List[Relation[Recursive]]): F[Unit]

  def tellRecRelation(body: Recursive): F[Unit] = {
    body match {
      case Recursive(_, List(q @ Quantifier(lbl, x, rstr, body2))) => {
        for {
          rels <- collectRelationsRec(x, rstr)
          _ <- push(x -> (q::rels))
          _ = println(s"xxx: ${q::rels}")
          _ <- tellRecRelation(body2)
          _ <- pop()
        } yield ()
      }
      case Recursive(_, rels) if !rels.exists(_.isQuantifier) => tellRelationList(rels)
      case _ => pure(()) // TODO
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
      case _ => fail
    }
  }

  def run(clauses: List[Clause]): NlpResult[List[String]] = {
    clauses.map(realiseClause).sequenceVoid.runS(init()).run match {
      case None => Left(RealiserError("Could not realise clause"))
      case Some(l) => Right(l._1)
    }
  }
}
